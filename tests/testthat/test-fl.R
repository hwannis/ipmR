# Foundational learning checks (#29): synthetic 5-17 questionnaire with one interviewer (23) who stops children at the reading practice
test_that("FL checks are auto-configured, computed and find the planted interviewer", {
  dir <- add_synth_fs(make_synth())
  cfg <- mics7_autoconfig(dir, "Synthetic")
  e <- cfg$extras$fl
  expect_equal(e$dataset, "fs")
  expect_equal(c(e$result, e$start_hour, e$end_hour, e$read_agree, e$story, e$numeracy_first, e$numeracy_next),
               c("FL28", "FL2H", "FS11H", "FL10", "FL19INTRO", "FL23A", "FL24A"))
  expect_equal(e$practice, "FL14"); expect_equal(e$read_try, "FL10C")
  expect_true(isTRUE(cfg$outputs$fl))
  cfg$period$start <- "2026-01-19"; cfg$period$days <- 13
  cfg$datasets$hh$flag_modules <- "HC*"; cfg$datasets$hh$flag_units <- "HC4U"
  cfg$datasets$fs$flag_modules <- character()
  res <- run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE)
  r <- res$outputs$fl
  expect_setequal(unique(r$data$check), c("completed", "minutes", "declined", "practice_exit", "story", "num_exit"))
  expect_true(all(r$data[check != "minutes", value >= 0 & value <= 100]))
  expect_true(all(r$data[check == "minutes", value > 0 & value <= cfg$options$maxquestime]))
  expect_equal(unname(r$averages["num_exit"]), 20, tolerance = 5)
  expect_equal(unname(r$averages["declined"]), 12.5, tolerance = 4)       # 25% say no, half of them then try in Somali
  pe <- r$data[check == "practice_exit", .(v = sum(value * n) / sum(n)), by = interv]
  expect_equal(pe[which.max(v), interv], 23)
  sc <- ipm_scorecard(res)
  s <- sc$signals[interv == 23]
  expect_equal(sum(s$area == "fl"), 1)                                     # the FL checks of one interviewer are one issue
  ev <- s[area == "fl"]$evidence
  expect_match(ev, "reading practice"); expect_match(ev, "reading story"); expect_match(ev, "minutes per FL assessment")
  expect_equal(s[area == "fl"]$type, "fl_assessment")
  expect_false(any(sc$signals[level == "interviewer" & interv != 23]$area == "fl"))
  sm <- ipm_summary(res)
  expect_true(any(grepl("reading practice", unlist(lapply(sm$sections, `[[`, "bullets")))))
})

test_that("FL is switched off when the questionnaire has no FL module", {
  cfg <- mics7_autoconfig(make_synth(), "Synthetic")
  expect_null(cfg$extras$fl)
  expect_false(isTRUE(cfg$outputs$fl))
})

test_that("scorecard, plan and report work without a flag analysis (descriptive signals only)", {
  dir <- add_synth_fs(make_synth())
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-19"; cfg$period$days <- 13
  for (k in names(cfg$outputs)) cfg$outputs[[k]] <- k %in% c("quescrates", "fl")
  cfg$results_dir <- file.path(dir, "out")
  res <- run_ipm(cfg, write = TRUE, progress = function(m) NULL, keep_data = FALSE)
  sc <- ipm_scorecard(res)
  expect_true(all(c("rate", "ratio", "top_modules") %in% names(sc$teams)))
  expect_true(length(ipm_plan_rules(sc)$teams) > 0)
  expect_true(file.exists(file.path(dir, "out", "IPM_report.docx")))
  expect_true(file.exists(file.path(dir, "out", "data", "fl.csv")))
})

test_that("a single individual questionnaire without the household file is auto-configured (no 'argument is of length zero')", {
  dir <- add_synth_fs(make_synth())
  file.remove(file.path(dir, c("hh.sav", "hl.sav")))
  cfg <- mics_autoconfig(dir, "Only 5-17")
  expect_equal(names(cfg$datasets), "fs")
  expect_null(cfg$datasets$fs$parent)
  expect_equal(cfg$fieldwork$source, "fs.sav")
  expect_false(is.null(cfg$period$start))
})

test_that("the report can be built with chosen sections and files only (#81)", {
  dir <- add_synth_fs(make_synth())
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-19"; cfg$period$days <- 13
  for (k in names(cfg$outputs)) cfg$outputs[[k]] <- k %in% c("quescrates", "quesmtimes", "fl")
  res <- run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE)
  pt <- ipm_report_parts(res)
  expect_equal(names(pt$sections), c("summary", names(res$outputs)))
  expect_false("flag_tables" %in% names(pt$files)); expect_false("anthro_xlsx" %in% names(pt$files))
  out <- file.path(dir, "part")
  rep <- ipm_report(res, out, sections = c("summary", "fl"), files = "data")
  expect_true(file.exists(rep))
  expect_true(file.exists(file.path(out, "data", "fl.csv")))
  expect_false(file.exists(file.path(out, "team_action_plan.docx")))
  txt <- officer::docx_summary(officer::read_docx(rep))$text
  expect_true(any(grepl("Left out of this report", txt)))
  expect_true(any(grepl("KEY FINDINGS", txt)))
  expect_false(any(grepl(res$outputs$quesmtimes$title, txt, fixed = TRUE)))
  out2 <- file.path(dir, "all"); ipm_report(res, out2)
  expect_true(file.exists(file.path(out2, "team_action_plan.docx")))
})
