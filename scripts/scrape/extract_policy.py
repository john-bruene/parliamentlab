#!/usr/bin/env python3
"""extract_policy.py — recover the policy area behind each roll-call vote.

The Parliament's vote records carry no committee or procedure reference, so
the chain runs through Parltrack:

    vote id  ->  procedure reference (epref)  ->  dossier  ->  responsible
    committee  ->  main_policy_name (mapped on the R side)

Both Parltrack dumps are lzip-compressed and large (the dossier file expands
to about 1.3 GB), so they are decompressed and parsed incrementally rather
than held in memory.

  python3 extract_policy.py <votes.lz> <dossiers.lz> <out.csv> [start_date]
"""
import json
import lzma
import sys

CHUNK = 8 << 20   # 8 MB of decompressed output at a time


def stream_text(path):
    """Yield decompressed text from an lzip file in bounded-size chunks."""
    with open(path, "rb") as f:
        data = f.read()
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
        member = data[pos + 6:]
        fed = 0
        while not dec.eof and fed < len(member):
            out = dec.decompress(member[fed:fed + (1 << 20)], CHUNK)
            fed += (1 << 20)
            if out:
                yield out.decode("utf-8", "replace")
            while not dec.eof and dec.needs_input is False:
                out = dec.decompress(b"", CHUNK)
                if not out:
                    break
                yield out.decode("utf-8", "replace")
        consumed = len(data) - pos - 6 - len(dec.unused_data)
        pos = pos + 6 + consumed + 20


def records(path):
    """Walk a huge JSON array of objects without holding the whole file."""
    dec = json.JSONDecoder()
    buf = ""
    started = False
    for chunk in stream_text(path):
        buf += chunk
        if not started:
            i = buf.find("[")
            if i < 0:
                continue
            buf = buf[i + 1:]
            started = True
        while True:
            j = 0
            while j < len(buf) and buf[j] in " \t\r\n,":
                j += 1
            if j >= len(buf) or buf[j] == "]":
                buf = buf[j:]
                break
            try:
                obj, end = dec.raw_decode(buf, j)
            except ValueError:
                buf = buf[j:]
                break          # incomplete object: wait for more input
            yield obj
            buf = buf[end:]


def main():
    if len(sys.argv) < 4:
        sys.exit(__doc__)
    votes_lz, doss_lz, out = sys.argv[1], sys.argv[2], sys.argv[3]
    start = sys.argv[4] if len(sys.argv) > 4 else None

    # 1. vote id -> procedure reference
    vote_ref, n_votes = {}, 0
    for rec in records(votes_lz):
        ts = str(rec.get("ts", ""))[:10]
        if start and ts < start:
            continue
        n_votes += 1
        ref = rec.get("epref")
        if isinstance(ref, list):
            ref = ref[0] if ref else None
        if ref:
            vote_ref[str(rec.get("voteid"))] = (ts, str(ref), str(rec.get("doc") or ""),
                                                str(rec.get("title") or ""))
    print(f"votes with a procedure reference : {len(vote_ref)} (of {n_votes} scanned)")

    # 2. procedure reference -> responsible committee
    wanted = {r[1] for r in vote_ref.values()}
    ref_cttee = {}
    for rec in records(doss_lz):
        proc = rec.get("procedure") or {}
        ref = proc.get("reference")
        if not ref or ref not in wanted:
            continue
        # A dossier may name several committees, and a single entry may itself
        # hold a list when committees share responsibility. Take the lead one.
        def first(v):
            while isinstance(v, list):
                v = v[0] if v else ""
            return str(v or "")

        code = full = ""
        for c in (rec.get("committees") or []):
            if not isinstance(c, dict):
                continue
            if c.get("responsible") is True:
                code, full = first(c.get("committee")), first(c.get("committee_full"))
                break
            if not code:
                code, full = first(c.get("committee")), first(c.get("committee_full"))
        subj = proc.get("subject")
        subj = "; ".join(subj) if isinstance(subj, list) else str(subj or "")
        ref_cttee[ref] = (code, full, subj, str(proc.get("title") or ""))
    print(f"procedure references resolved    : {len(ref_cttee)} of {len(wanted)}")

    # 3. join
    with open(out, "w", encoding="utf-8", newline="") as f:
        import csv as _csv
        w = _csv.writer(f)
        w.writerow(["voting_id", "date", "epref", "doc", "vote_title",
                    "committee_code", "committee_full", "subject", "procedure_title"])
        hit = 0
        for vid, (ts, ref, doc, vtitle) in vote_ref.items():
            c = ref_cttee.get(ref)
            if c:
                hit += 1
            w.writerow([vid, ts, ref, doc, vtitle, *(c if c else ("", "", "", ""))])
    print(f"rows written                     : {len(vote_ref)} ({hit} with a committee) -> {out}")


if __name__ == "__main__":
    main()
