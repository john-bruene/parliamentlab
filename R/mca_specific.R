# R/mca_specific.R
#
# MCA on the vote matrix, with "not in office" and "absent" left out of the
# geometry.
#
# Code 0 means the MEP was not a member when the vote took place; codes 4-6
# mean they were a member but did not vote. Plain MCA treats both like
# answers, and because they are rare for most MEPs they weigh a lot. An MEP
# who left early (a new Commissioner, say) or never turned up then shares one
# "answer" with almost nobody on thousands of votes, and an axis ends up
# measuring who was there rather than how people vote. In EP10 the first axis
# explained 2% of the differences between groups; with these codes set aside
# it explains 93%. In EP8 one MEP who held his seat all term without casting a
# vote sat nine units out on the second axis, which explained 10% of the group
# differences; set aside, he sits in the middle and the axis explains 80%.
#
# The fix is the textbook one for "junk" categories (Le Roux & Rouanet's
# specific MCA): FactoMineR's `excl` keeps the codes in the table but gives
# them no say in the axes. W-NOMINATE treats them the same way (notInLegis = 0,
# missing = 3 and 4). Yes, no and abstain (1-3) stay as answers.

# Positions of the junk levels in FactoMineR's disjunctive table: one column
# per level of each factor, factors in order, levels in levels() order.
mca_junk_idx <- function(f, junk = c("0", "4", "5", "6")) {
  lev <- lapply(f, levels)
  off <- c(0, cumsum(lengths(lev)))[seq_along(lev)]
  idx <- unlist(mapply(function(l, o) o + which(l %in% junk), lev, off),
                use.names = FALSE)
  if (length(idx)) idx else NULL
}

# FactoMineR::MCA with those codes excluded. `f` is a data frame of factors.
mca_votes <- function(f, ncp = 2, junk = c("0", "4", "5", "6")) {
  FactoMineR::MCA(f, ncp = ncp, graph = FALSE, excl = mca_junk_idx(f, junk))
}
