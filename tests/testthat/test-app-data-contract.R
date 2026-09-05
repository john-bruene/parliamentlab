# Every lookup in server.R keys on a string that the data has to spell the
# same way. Nothing errors when a key misses — the group turns grey, the
# country drops off the map, the filter returns nothing — so these mismatches
# survive review and only show up as a chart that quietly looks wrong. Three
# of them were live at once: EP7's "Europe of freedom and democracy Group"
# capitalised in the lookup, Denmark absent from the flag map, and Greece
# keyed as "GR" where the shapefile says "EL".

PARLS <- c("P6", "P7", "P8", "P9", "P9full", "P10")

# Pull a top-level list or vector literal out of server.R by brace matching.
server_literal <- function(name) {
  src <- readLines(proj_path("server.R"), warn = FALSE)
  i <- grep(paste0("^\\s*", name, "\\s*<-\\s*(list|c|tibble)\\("), src)[1]
  if (is.na(i)) stop("literal not found in server.R: ", name)
  depth <- 0
  out <- character(0)
  for (k in seq(i, length(src))) {
    out <- c(out, src[k])
    bare <- gsub("#.*$", "", src[k])
    depth <- depth +
      lengths(regmatches(bare, gregexpr("\\(", bare))) -
      lengths(regmatches(bare, gregexpr("\\)", bare)))
    if (k > i && depth <= 0) break
  }
  e <- new.env(parent = globalenv())
  # server.R builds one of these with tibble(); give the literal what it needs
  # without dragging the whole app's library() list into the test.
  e$tibble <- function(...) data.frame(..., stringsAsFactors = FALSE)
  eval(parse(text = paste(out, collapse = "\n")), envir = e)
}

parl <- function(p) {
  f <- proj_path(sprintf("data/%s_umap.rds", p))
  if (!file.exists(f)) return(NULL)
  suppressWarnings(readRDS(f))
}

test_that("every political group resolves in order, colour and label", {
  lro <- server_literal("left_right_orders")
  pcl <- server_literal("party_colors_list")
  alb <- server_literal("axis_labels_list")
  for (p in PARLS) {
    d <- parl(p); if (is.null(d)) next
    epg <- unique(as.character(d$EPG))
    epg <- epg[!is.na(epg)]
    expect_equal(setdiff(epg, names(lro[[p]])), character(0), label = paste(p, "seating order"))
    expect_equal(setdiff(epg, names(pcl[[p]])), character(0), label = paste(p, "colours"))
    expect_equal(setdiff(epg, names(alb[[p]])), character(0), label = paste(p, "axis labels"))
  }
})

test_that("seating positions are unique within a legislature", {
  lro <- server_literal("left_right_orders")
  for (p in names(lro)) {
    o <- lro[[p]]
    expect_equal(anyDuplicated(unname(o)), 0L, label = paste(p, "duplicate seat position"))
  }
})

test_that("every country has a flag and a place on the map", {
  ccm <- server_literal("country_code_map")
  geo <- server_literal("country_codes")
  shp <- suppressWarnings(readRDS(proj_path("data/SHP_0.rds")))
  regions <- unique(as.character(shp$NUTS_ID))
  for (p in PARLS) {
    d <- parl(p); if (is.null(d)) next
    cs <- unique(as.character(d$Country))
    cs <- cs[!is.na(cs)]
    expect_equal(setdiff(cs, names(ccm)), character(0), label = paste(p, "flag lookup"))
    expect_equal(setdiff(cs, geo$Country), character(0), label = paste(p, "map lookup"))
  }
  # The shapefile keys on Eurostat NUTS codes, where Greece is EL, not GR.
  expect_equal(setdiff(geo$geo, regions), character(0))
})

test_that("gender uses the one coding the filter offers", {
  # ui.R's checkbox group offers M and F. Anything else - the API's MALE /
  # FEMALE, say - means unticking one box empties the whole legislature.
  for (p in PARLS) {
    d <- parl(p); if (is.null(d)) next
    g <- unique(as.character(d$Gender))
    expect_equal(setdiff(g[!is.na(g)], c("M", "F")), character(0), label = paste(p, "gender coding"))
  }
})

test_that("the derived indices are populated, not silently blank", {
  # Winning_Score used to come out NA for 732 of EP10's 738 MEPs: eleven votes
  # whose tally the API left incomplete turned every rowSums() into NA.
  for (p in PARLS) {
    d <- parl(p); if (is.null(d)) next
    for (cc in c("Attendance_Score", "Winning_Score", "loyalty_score")) {
      v <- suppressWarnings(as.numeric(d[[cc]]))
      expect_lt(sum(is.na(v)) / length(v), 0.05, label = paste(p, cc, "share NA"))
    }
    expect_true(all(abs(d$Winning_Score) <= 1, na.rm = TRUE), label = paste(p, "Winning_Score in range"))
    expect_true(all(d$Attendance_Score >= 0 & d$Attendance_Score <= 1, na.rm = TRUE),
                label = paste(p, "Attendance_Score in range"))
  }
})

test_that("activity counts cover most of the chamber", {
  CATS <- c("CRE", "WDECL", "COMPARL", "REPORT", "REPORT_SHADOW", "COMPARL_SHADOW",
            "MOTION", "OQ", "WEXP", "WQ", "MINT", "IMOTION", "PRUNACT")
  # P9full once read 462 of 873, because the Parltrack dump restricted to the
  # term credits 391 of the published MEPs with nothing.
  for (p in c("P9", "P9full", "P10")) {
    d <- parl(p); if (is.null(d)) next
    have <- intersect(CATS, names(d))
    tot <- rowSums(vapply(d[have], function(x) {
      x <- as.numeric(x); x[is.na(x)] <- 0; x
    }, numeric(nrow(d))))
    expect_gt(mean(tot > 0), 0.7, label = paste(p, "share of MEPs with any recorded activity"))
  }
})

test_that("the hemicycle seats every MEP in left-right order", {
  skip_if_not(file.exists(proj_path("R/parliament_local.R")))
  suppressPackageStartupMessages({
    library(dplyr); library(ggplot2)
  })
  source(proj_path("R/parliament_local.R"), local = TRUE)
  lro <- server_literal("left_right_orders")
  for (p in PARLS) {
    d <- parl(p); if (is.null(d)) next
    lr <- lro[[p]]
    # Mirrors create_interactive_parliament_plot(): positions come from the
    # order map, and the factor levels have to span all of them. Hardcoding
    # 1:8 dropped the ninth group of EP8, EP9 and EP10 out of the factor.
    pos <- unname(lr[as.character(d$EPG)])
    lv <- factor(pos, levels = seq_len(max(lr, na.rm = TRUE)))
    expect_false(any(is.na(lv) & !is.na(pos)), label = paste(p, "positions outside the factor levels"))

    dat <- data.frame(EPG = as.character(d$EPG), left_right = lv, seats = 1L)
    dat <- dat[order(dat$left_right), ]
    co <- parliament_data_local(election_data = dat, parl_rows = 10,
                                type = "semicircle", party_seats = dat$seats)
    expect_equal(nrow(co), nrow(d), label = paste(p, "seats placed"))
    # Each group must occupy one contiguous wedge, or the chamber reads as noise.
    runs <- rle(as.character(dat$EPG))$values
    expect_equal(anyDuplicated(runs), 0L, label = paste(p, "group split across the chamber"))
  }
})
