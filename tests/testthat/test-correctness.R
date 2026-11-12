# Phase 1 correctness fixes (review of Nov 2026)
synth_cfg <- function(dir = make_synth()) {
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 28
  cfg$datasets$hh$flag_modules <- "HC*"; cfg$datasets$hh$flag_units <- "HC4U"
  cfg
}

test_that("one impossible date does not blank the other dates", {
  expect_equal(as.character(ipmR:::ymd_date(c(2026, 2026, NA, 2026), c(2, 1, 1, 13), c(31, 5, 5, 1))), c(NA, "2026-01-05", NA, NA))
  dir <- make_synth(); hh <- haven::read_sav(file.path(dir, "hh.sav"))
  hh$HH5D[1] <- 31; hh$HH5M[1] <- 2                                # 31 February on the first record
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  cfg <- synth_cfg(dir)
  expect_false(is.null(cfg$period$start)); expect_false(is.null(cfg$fieldwork))
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  expect_equal(sum(is.na(p$ds$hh$doi)), 1)
})

test_that("the period of analysis covers exactly `days` days (Stata-compatible: one more)", {
  cfg <- list(period = list(start = "2026-10-01", days = 21), options = list())
  pb <- ipmR:::period_bounds(cfg)
  expect_equal(as.character(pb$end), "2026-10-21")
  cfg$options$stata_compat <- TRUE
  expect_equal(as.character(ipmR:::period_bounds(cfg)$end), "2026-10-22")
})

test_that("records before week 1 are not counted in week 1", {
  dir <- make_synth(); hh <- haven::read_sav(file.path(dir, "hh.sav"))
  hh$HH5D[1:3] <- c(2, 3, 4); hh$HH5M[1:3] <- 1                    # 1-3 days before the first full day (5 Jan)
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  cfg <- synth_cfg(dir)
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  early <- p$ds$hh[doi < as.IDate("2026-01-05")]
  expect_true(all(is.na(early$woi)))
  cfg$options$stata_compat <- TRUE
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  expect_true(all(p$ds$hh[doi < as.IDate("2026-01-05")]$woi == 1))
})

test_that("linked files keep their team when teams come from a supervisor variable", {
  cfg <- synth_cfg(); cfg$teams <- list(variable = "HH4")
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  expect_true(mean(!is.na(p$ds$hl$team)) > 0.99)
  expect_setequal(unique(p$ds$hl$team), unique(p$ds$hh$team))
})

test_that("an empty questionnaire is a warning and is skipped, not a stop", {
  dir <- make_synth(); hh <- haven::read_sav(file.path(dir, "hh.sav"))
  wm <- hh[0, ]; names(wm) <- sub("^HH", "WM", names(wm)); haven::write_sav(wm, file.path(dir, "wm.sav"))
  cfg <- synth_cfg(dir)
  cfg$datasets$wm <- list(file = "wm.sav", interviewer = "WM3", result = "WM46", day = "WM5D")
  iss <- validate_config(cfg)
  expect_false(any(iss$level == "error"))
  expect_true(any(iss$where == "wm" & iss$level == "warning"))
  res <- run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE)
  expect_false("wm" %in% res$data_summary$dataset)
  expect_true("hh" %in% res$data_summary$dataset)
})

test_that("missing, reversed or overlapping team ranges are errors", {
  cfg <- synth_cfg()
  msg <- function(r) { cfg$teams <- list(ranges = r); i <- validate_config(cfg); paste(i$message[i$where == "teams" & i$level == "error"], collapse = " ") }
  expect_match(msg(character()), "define teams")
  expect_match(msg(c("11-24", "21-34")), "overlapping")
  expect_match(msg(c("14-11")), "larger")
  expect_equal(msg(c("11-14", "21-24", "31-34")), "")
})

test_that("interviewers are tested on the answers to the question, not on their records", {
  expect_true(ipm_defaults()$test_min_nonmissing)
  expect_false(ipmR:::normalise_config(list(options = list(stata_compat = TRUE)))$options$test_min_nonmissing)
  expect_false(ipmR:::normalise_config(list(options = list(stata_compat = TRUE, test_min_nonmissing = TRUE)))$options$test_min_nonmissing)
  dir <- make_synth(); hh <- haven::read_sav(file.path(dir, "hh.sav"))
  hh$HC2[hh$HH3 == 11] <- NA; hh$HC2[which(hh$HH3 == 11)[1:2]] <- 1   # interviewer 11 answered HC2 only twice
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  cfg <- synth_cfg(dir)
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  t <- flag_analysis(p, make_plots = FALSE)$tests
  expect_false(any(t$varname == "HC2" & t$interv == 11))
  cfg$options$stata_compat <- TRUE
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  t <- flag_analysis(p, make_plots = FALSE)$tests
  expect_true(any(t$varname == "HC2" & t$interv == 11))
})
