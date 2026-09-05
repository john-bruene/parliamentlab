#!/usr/bin/env python3
"""extract_activities.py — count MEP activities from the Parltrack dump.

Parltrack publishes ep_mep_activities.json.lz: one record per MEP, each
holding lists of activities grouped by type (CRE speeches, REPORT, WQ ...).
The file is lzip-compressed, which neither R nor Python's standard library
reads directly, and it expands to roughly 400 MB of JSON. This helper does
the decompression and the counting in one streaming pass and writes a small
CSV, so the R side never has to hold the whole thing in memory.

  python3 extract_activities.py <dump.lz> <out.csv> [start_date] [end_date]

Dates are inclusive, ISO format. Activities without a parseable date are
counted only when no date window is given.
"""
import json
import lzma
import sys

# The categories carried in the existing ParliamentLab files.
CATEGORIES = ["CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW",
              "COMPARL_SHADOW", "MOTION", "OQ", "WEXP", "WQ",
              "MINT", "IMOTION", "PRUNACT"]


def lzip_members(path):
    """Yield the decompressed bytes of each lzip member.

    lzip is a raw LZMA1 stream wrapped in a 6-byte header ('LZIP', version,
    dictionary-size byte) and a 20-byte trailer. Python's lzma can decode the
    stream once the dictionary size is read out of the header.
    """
    data = open(path, "rb").read()
    pos = 0
    while pos < len(data):
        if data[pos:pos + 4] != b"LZIP":
            break
        ds = data[pos + 5]
        dict_size = 1 << (ds & 0x1F)
        dict_size -= (dict_size // 16) * ((ds >> 5) & 0x7)
        dec = lzma.LZMADecompressor(
            format=lzma.FORMAT_RAW,
            filters=[{"id": lzma.FILTER_LZMA1, "dict_size": dict_size,
                      "lc": 3, "lp": 0, "pb": 2}])
        yield dec.decompress(data[pos + 6:])
        consumed = len(data) - pos - 6 - len(dec.unused_data)
        pos = pos + 6 + consumed + 20


def iter_records(text):
    """Walk a large JSON array one object at a time, discarding as we go."""
    dec = json.JSONDecoder()
    i = text.find("[")
    if i < 0:
        return
    i += 1
    n = len(text)
    while i < n:
        while i < n and text[i] in " \t\r\n,":
            i += 1
        if i >= n or text[i] == "]":
            return
        obj, end = dec.raw_decode(text, i)
        yield obj
        i = end


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    dump, out = sys.argv[1], sys.argv[2]
    start = sys.argv[3] if len(sys.argv) > 3 else None
    end = sys.argv[4] if len(sys.argv) > 4 else None

    text = b"".join(lzip_members(dump)).decode("utf-8", "replace")

    rows, n_rec, undated = [], 0, 0
    for rec in iter_records(text):
        mep_id = rec.get("mep_id")
        if mep_id is None:
            continue
        n_rec += 1
        counts = dict.fromkeys(CATEGORIES, 0)
        for cat in CATEGORIES:
            items = rec.get(cat)
            if not isinstance(items, list):
                continue
            for it in items:
                d = it.get("date") if isinstance(it, dict) else None
                if d:
                    d = str(d)[:10]
                    if start and d < start:
                        continue
                    if end and d > end:
                        continue
                elif start or end:
                    undated += 1
                    continue
                counts[cat] += 1
        rows.append((mep_id, counts))

    with open(out, "w", encoding="utf-8") as f:
        f.write("mep_id," + ",".join(CATEGORIES) + "\n")
        for mep_id, c in rows:
            f.write(f"{mep_id}," + ",".join(str(c[k]) for k in CATEGORIES) + "\n")

    print(f"records read      : {n_rec}")
    print(f"rows written      : {len(rows)} -> {out}")
    if undated:
        print(f"skipped (no date) : {undated}")


if __name__ == "__main__":
    main()
