test_that("auto-config, prepare and outputs run on a synthetic survey", {
  dir <- make_synth()
  cfg <- mics7_autoconfig(dir, "Synthetic")
  expect_equal(cfg$datasets$hh$interviewer, "HH3")
  expect_equal(cfg$datasets$hl$parent, "hh")
  expect_equal(length(cfg$teams$ranges), 3)
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  cfg$datasets$hh$flag_units <- "HC4U"
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  expect_true(all(!is.na(p$ds$hl$interv)))                # interviewer inherited from parent
  expect_equal(min(p$ds$hh$woi, na.rm = TRUE), 1L)
  r <- out_quescrates(p); expect_true(nrow(r$data) > 0)
  r <- out_quesmtimes(p); expect_true(all(r$data$median_minutes <= 60))
  r <- out_meannohhm(p); expect_true(nrow(r$data) > 0)
})

test_that("flag engine finds planted bias and matches the naive implementation", {
  dir <- make_synth()
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  cfg$datasets$hh$flag_modules <- "HC*"; cfg$datasets$hh$flag_units <- "HC4U"
  p <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  fa <- flag_analysis(p, make_plots = FALSE)
  t <- fa$tests
  expect_true(t[varname == "HC2" & interv == 13, flag] == 1)
  expect_true(t[varname == "HC3" & interv == 22, flag] == 1)
  expect_false("HC5" %in% t$varname)                      # uniform variable skipped
  expect_true("HC4U_HC4N_pair" %in% t$varname)
  nv <- flag_tests_naive(p)
  m <- merge(t, nv, by = c("dataset", "module", "varname", "team", "interv"))
  expect_equal(nrow(m), nrow(t)); expect_equal(nrow(m), nrow(nv))
  expect_equal(m$p, m$p_naive, tolerance = 1e-9)
})

test_that("full run writes report files", {
  skip_on_cran()
  dir <- make_synth()
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  cfg$outputs$meannohhm <- FALSE
  cfg$results_dir <- file.path(dir, "out")
  res <- run_ipm(cfg, progress = function(...) NULL)
  expect_true(file.exists(file.path(dir, "out", "IPM_report.docx")))
  expect_true(file.exists(file.path(dir, "out", "data", "flag_tests.csv")))
})

test_that("auto-configuration is language-agnostic (French labels, non-standard file names)", {
  dir <- make_synth(lang = "fr")
  file.rename(file.path(dir, "hh.sav"), file.path(dir, "Menages_MICS7.sav"))
  file.rename(file.path(dir, "hl.sav"), file.path(dir, "membres.sav"))
  cfg <- mics_autoconfig(dir, "Synthetique")
  q <- cfg$datasets$menages_mics7
  expect_equal(c(q$interviewer, q$result, q$day, q$start_hour, q$end_hour), c("HH3", "HH46", "HH5D", "HH11H", "HH13H"))
  expect_equal(cfg$datasets$membres$parent, "menages_mics7")
  expect_equal(cfg$extras$hhm$dataset, "membres")
  expect_equal(length(cfg$teams$ranges), 3)
})

test_that("label folding handles accents and curly quotes", {
  expect_equal(ipmR:::ascii_fold("Résultat de l’interview"), "resultat de l'interview")
  expect_equal(ipmR:::ascii_fold("Code chef d’équipe"), "code chef d'equipe")
})

test_that("flagged tests carry a plain-language reading and the narrative names the biased interviewers", {
  dir <- make_synth()
  cfg <- mics_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  cfg$datasets$hh$flag_modules <- "HC*"
  res <- run_ipm(cfg, write = FALSE, progress = function(...) NULL, keep_data = FALSE)
  t <- res$outputs$flaganal$tests
  expect_match(t[varname == "HC2" & interv == 13, pattern], "Interviewer 13 recorded '1. YES'")
  expect_match(t[varname == "HC3" & interv == 22, pattern], "mean")
  expect_true(all(is.na(t[flag == 0, pattern])))
  s <- ipm_summary(res)
  expect_true(all(c("overall", "modules", "interviewers", "caveats") %in% names(s$sections)))
  expect_true(any(c(13, 22) %in% head(s$tables$interviewers$interv, 3)))
  expect_s3_class(s$plots$team_module, "ggplot")
  expect_true(nrow(res$data_summary) == 2)
})

test_that("slim loading gives the same flag results as full loading", {
  dir <- make_synth()
  cfg <- mics_autoconfig(dir, "Synthetic"); cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  p1 <- ipm_prepare(ipm_load(cfg, quiet = TRUE), cfg, quiet = TRUE)
  p2 <- ipm_prepare(ipm_load(cfg, quiet = TRUE, slim = TRUE), cfg, quiet = TRUE, inplace = TRUE)
  f1 <- flag_analysis(p1, make_plots = FALSE, make_tables = FALSE)$tests
  f2 <- flag_analysis(p2, make_plots = FALSE, make_tables = FALSE)$tests
  expect_equal(f1$p, f2$p)
})

test_that("auto-config reports the fieldwork dates and proposes the last 21 days", {
  cfg <- mics7_autoconfig(make_synth(), "Synthetic")
  fw <- cfg$fieldwork
  expect_identical(fw$source, "hh.sav")
  expect_true(fw$first >= "2026-01-05" && fw$last <= "2026-02-01")
  expect_equal(fw$days, as.integer(as.Date(fw$last) - as.Date(fw$first)) + 1L)
  expect_identical(cfg$period$start, format(max(as.Date(fw$first), as.Date(fw$last) - 20)))
})

test_that("the initial module of a questionnaire is tested, without its administrative variables", {
  dir <- make_synth(); cfg <- mics7_autoconfig(dir, "Synthetic")
  hh <- cfg$datasets$hh
  expect_identical(hh$flag_modules[1], "HH*")
  expect_true(all(c("HH1", "HH2", "HH3", "HH4", "HH5D", "HH5M", "HH5Y", "HH11H", "HH11M", "HH13H", "HH13M") %in% hh$flag_ignore))
  expect_false("HH46" %in% hh$flag_ignore)                                  # result of interview stays in
  L <- ipm_load(cfg, quiet = TRUE, n_max = 400)
  fc <- ipm_flag_check(cfg, L$meta)
  v <- fc$variables[dataset == "hh" & module == "HH"]
  expect_identical(v[variable == "HH46", status], "tested")
  expect_identical(v[variable == "HH3", status], "ignored")
  expect_true(all(c("variables", "tested", "ignored") %in% names(fc$modules)))
  cfg$datasets$hh$flag_modules <- c(cfg$datasets$hh$flag_modules, "ZZ*"); cfg$datasets$hh$flag_ignore <- c(cfg$datasets$hh$flag_ignore, "NOTAVAR")
  pr <- ipm_flag_check(cfg, L$meta)$problems
  expect_true(any(grepl("ZZ\\* matches no variable", pr))); expect_true(any(grepl("NOTAVAR", pr)))
})

test_that("auto-config reports interview dates per questionnaire", {
  cfg <- mics7_autoconfig(make_synth(), "Synthetic")
  expect_true("hh" %in% names(cfg$fieldwork$by_questionnaire))
  expect_identical(cfg$fieldwork$by_questionnaire$hh$last, cfg$fieldwork$last)
})
