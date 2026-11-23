# Data explorer (#82): reads only the variables asked for, frequencies and numbers by team / interviewer / variable
test_that("the explorer reads chosen variables with interviewer, team and date, and tabulates them", {
  dir <- add_synth_fs(make_synth())
  cfg <- mics7_autoconfig(dir, "Synthetic")
  ed <- ipm_explore_data(cfg, "fs", c("FL14", "FL10"))
  expect_true(all(c("FL14", "FL10", "interv", "team", "doi", "woi", "result", "interv_label") %in% names(ed$x)))
  expect_false("FL19INTRO" %in% names(ed$x))                                   # only what was asked
  t <- ipm_tabulate(ed, "FL14")
  expect_equal(t$type, "categorical")
  expect_equal(sum(t$freq$n), t$n); expect_equal(sum(t$freq$pct), 100)
  expect_true(t$blank > 0); expect_false(anyNA(t$freq$value))                  # blanks (not asked) left out by default
  t2 <- ipm_tabulate(ed, "FL14", skip_blank = FALSE)
  expect_equal(t2$n, t$n + t$blank)
  ti <- ipm_tabulate(ed, "FL14", by = "interv")
  expect_lt(ti$test$p, 0.001)                                                  # interviewer 23 stops children at the practice
  p23 <- ti$cross[key == 23 & value == 2, pct]; expect_gt(p23, 70)
  tt <- ipm_tabulate(ed, "FL14", by = "team", teams = 2)
  expect_equal(nrow(tt$groups), 1)
  tv <- ipm_tabulate(ed, "FL14", by = "var", by_var = "FL10")
  expect_setequal(tv$groups$key, unique(stats::na.omit(ed$x[!is.na(FL14) & result == 1]$FL10)))
  for (w in c("dist", "by")) expect_s3_class(ipm_tab_plot(ti, w), "ggplot")
})

test_that("numbers: summary, last digit and break-down, linked file takes the parent's interviewer", {
  dir <- make_synth(); cfg <- mics7_autoconfig(dir, "Synthetic")
  ed <- ipm_explore_data(cfg, "hl", "HL6")
  expect_false(all(is.na(ed$x$interv)))
  t <- ipm_tabulate(ed, "HL6", by = "team", from = as.Date("2026-01-19"))
  expect_equal(t$type, "numeric")
  expect_equal(sum(t$digits$n), t$stats$n)
  expect_true(all(ed$x[doi >= as.IDate("2026-01-19")]$doi >= as.IDate("2026-01-19")))
  expect_equal(sum(t$num_by$n), t$stats$n)
  expect_true(all(t$num_by$pct_0_5 >= 0 & t$num_by$pct_0_5 <= 100))
  expect_s3_class(ipm_tab_plot(t, "digits"), "ggplot")
  expect_s3_class(ipm_tab_plot(t, "by"), "ggplot")
  expect_equal(ipm_tabulate(ed, "HL6", type = "categorical")$type, "categorical")
  expect_error(ipm_tabulate(ed, "HL4"), "not read")
})

test_that("labelled don't know codes are marked and left out of the numbers", {
  ed <- list(x = data.table::data.table(V = c(rep(c(10, 12, 15, 20, 21, 25, 30, 33, 35, 40, 41, 44, 45, 50, 52, 55), 3), 98, 98),
                                        interv = 1, team = 1L, doi = data.table::as.IDate("2026-01-05"), woi = 1L, result = 1, interv_label = "1 (T1)"),
             meta = list(varlab = c(V = "Age"), vallab = list(V = c(DK = 98))), team_info = data.table::data.table(team = 1L, team_label = "T1"), completed_codes = 1)
  t <- ipm_tabulate(ed, "V")
  expect_equal(t$type, "numeric"); expect_equal(t$stats$special_codes, 2); expect_equal(t$stats$max, 55)
  expect_equal(t$digits[digit %in% c(0, 5), sum(pct)], 100 * 30 / 48)
})

test_that("a numeric break-down variable is grouped (quintiles), and a test that cannot be computed is left out", {
  set.seed(3)
  x <- data.table::data.table(Y = sample(1:2, 500, TRUE), S = round(rnorm(500), 4), interv = 1, team = 1L,
                              doi = data.table::as.IDate("2026-01-05"), woi = 1L, result = 1, interv_label = "1 (T1)")
  ed <- list(x = x, meta = list(varlab = c(Y = "Area", S = "Wealth score"), vallab = list(Y = c(URBAN = 1, RURAL = 2))),
             team_info = data.table::data.table(team = 1L, team_label = "T1"), completed_codes = 1)
  t <- ipm_tabulate(ed, "Y", by = "var", by_var = "S")
  expect_equal(t$binned$groups, 5); expect_equal(nrow(t$groups), 5)
  expect_true(all(abs(t$groups$n - 100) <= 1))
  expect_true(startsWith(t$groups$group[1], "Quintile 1 ("))
  expect_equal(nrow(ipm_tabulate(ed, "Y", by = "var", by_var = "S", by_groups = 10)$groups), 10)
  ed$x[, Y := 2L]                                                    # one answer only: no test, no error
  expect_null(ipm_tabulate(ed, "Y", by = "var", by_var = "S")$test)
  ts <- ipm_tabulate(ed, "S", by = "var", by_var = "Y")              # swapped: the score by area, no last digit for decimals
  expect_true(all(c("min", "max", "mean") %in% names(ts$num_by)))
  expect_null(ts$digits); expect_true(is.na(ts$num_by$pct_0_5))
})

test_that("weights: weighted % and means, counts and tests stay on the records", {
  x <- data.table::data.table(Y = c(1L, 1L, 2L, 2L), A = c(10, 20, 30, 40), G = c(1L, 2L, 1L, 2L), W = c(3, 1, 1, NA),
                              interv = 1, team = 1L, doi = data.table::as.IDate("2026-01-05"), woi = 1L, result = 1, interv_label = "1 (T1)")
  ed <- list(x = x, meta = list(varlab = c(Y = "Area", A = "Age", G = "Sex", W = "Weight"), vallab = list(Y = c(URBAN = 1, RURAL = 2))),
             team_info = data.table::data.table(team = 1L, team_label = "T1"), completed_codes = 1)
  t <- ipm_tabulate(ed, "Y", weight = "W")
  expect_equal(t$no_weight, 1); expect_equal(t$freq$n, c(2L, 1L))               # record without a weight left out, N = records
  expect_equal(t$freq$pct, c(80, 20))                                            # (3 + 1) / 5 and 1 / 5
  tu <- ipm_tabulate(ed, "Y"); expect_equal(tu$freq$pct, c(50, 50)); expect_null(tu$weight)
  tn <- ipm_tabulate(ed, "A", type = "numeric", weight = "W")
  expect_equal(tn$stats$mean, (3 * 10 + 20 + 30) / 5)
  expect_equal(tn$stats$median, 10)                                              # weighted median: 60% of the weight is on 10
  tb <- ipm_tabulate(ed, "Y", by = "var", by_var = "G", weight = "W")
  expect_equal(tb$cross[key == 1 & value == 1, pct], 75)
  expect_error(ipm_tabulate(ed, "Y", weight = "NOPE"), "not read")
})
