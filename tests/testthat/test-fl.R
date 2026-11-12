# Foundational learning checks (#29): synthetic 5-17 questionnaire with one interviewer (23) who stops children at the reading practice
add_synth_fs <- function(dir) {
  set.seed(7)
  lab <- function(x, l) haven::labelled(x, label = l)
  ints <- c(11:14, 21:24, 31:34); days <- seq(as.Date("2026-01-05"), by = "day", length.out = 28)
  n <- 900
  iv <- sample(ints, n, TRUE); d <- sample(days, n, TRUE); bad <- iv == 23
  res <- sample(c(1, 1, 1, 1, 1, 1, 1, 2, 4), n, TRUE)
  done <- res == 1
  agree <- ifelse(done, sample(c(1, 1, 1, 2), n, TRUE), NA)
  try_other <- ifelse(agree %in% 2, sample(c(11, 95), n, TRUE), NA)          # no at FL10A/B: offered the story in Somali
  fl14 <- ifelse(agree %in% 1, ifelse(bad, sample(1:2, n, TRUE, prob = c(.1, .9)), sample(1:2, n, TRUE, prob = c(.7, .3))), NA)
  story <- ifelse(fl14 %in% 1, 1, NA)
  fl23 <- ifelse(done, sample(1:3, n, TRUE, prob = c(.8, .1, .1)), NA)
  fl24 <- ifelse(fl23 %in% 1, 1, NA)
  sh <- sample(9:15, n, TRUE); sm <- sample(0:39, n, TRUE)
  dur <- ifelse(bad, sample(2:5, n, TRUE), sample(12:20, n, TRUE))
  fs <- data.frame(
    HH1 = lab(rep(1:90, each = 10)[1:n], "Cluster number"), HH2 = lab(rep(1:10, 90)[1:n], "Household number"), LN = lab(rep(3, n), "Line number"),
    FS5 = lab(iv, "Interviewer number"), FS6 = lab(iv %/% 10 * 10, "Supervisor number"),
    FS7D = lab(as.integer(format(d, "%d")), "Day of interview"), FS7M = lab(as.integer(format(d, "%m")), "Month of interview"),
    FS7Y = lab(as.integer(format(d, "%Y")), "Year of interview"),
    FS17 = lab(rep(1, n), "Result of interview for children age 5-17"),
    FS8H = lab(sh, "Start of interview - Hour"), FS8M = lab(sm, "Start of interview - Minutes"),
    FS11H = lab(sh + (sm + 10 + dur) %/% 60, "End of interview - Hour"), FS11M = lab((sm + 10 + dur) %% 60, "End of interview - Minutes"),
    FL2H = lab(sh + (sm + 10) %/% 60, "Start of interview with child - Hour"), FL2M = lab((sm + 10) %% 60, "Start of interview with child - Minutes"),
    FL10 = lab(agree, "Like to read story"), FL10C = lab(try_other, "Like to read story (other language)"), FL14 = lab(fl14, "Child read every word in the practice correctly"),
    FL19INTRO = lab(story, "FL19. INTRO"), FL23A = lab(fl23, "Child recognizes symbol: 9"), FL24A = lab(fl24, "Child identifies bigger of two numbers"),
    FL28 = lab(res, "Result of interview with child"))
  haven::write_sav(fs, file.path(dir, "fs.sav"))
  dir
}

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
