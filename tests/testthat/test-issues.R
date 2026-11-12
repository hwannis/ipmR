# Phase 3: one rule engine - questionnaire, team and interviewer issues
mini_res <- function(minutes, datasets = "hh") {
  g <- data.table::CJ(dataset = datasets, team = 1:3, k = 1:3, woi = 1:3)
  g[, interv := team * 10 + k]
  g[, median_minutes := minutes(team, interv, dataset)][, n := 10L]
  list(cfg = list(options = ipm_defaults(), survey_name = "Test"),
       team_info = data.table::data.table(team = 1:3, team_label = c("T1", "T2", "T3")),
       data_summary = data.table::data.table(dataset = datasets, records = 300),
       outputs = list(quesmtimes = list(data = g[, .(dataset, team, interv, woi, median_minutes, n)])))
}

test_that("a whole team with short interviews is one team issue, not one issue per interviewer", {
  sc <- ipm_scorecard(mini_res(function(team, interv, ds) ifelse(team == 3, 15, 40)))
  s <- sc$signals
  expect_equal(nrow(s[level == "team"]), 1)
  expect_equal(s[level == "team"]$type, "team_duration")
  expect_equal(s[level == "team"]$team, 3L)
  expect_match(s[level == "team"]$evidence, "Whole team, median interview length: 15 min vs 40 min in the other teams")
  expect_match(s[level == "team"]$evidence, "3 of 3 interviewers")
  expect_equal(nrow(s[level == "interviewer"]), 0)                            # the team's issue, listed once
  expect_equal(s[level == "team"]$severity, "high")
  expect_equal(s[level == "team"]$headline, "Whole team, short interviews: hh 15 vs 40 min")
  expect_equal(sc$teams[team == 3]$priority, "Medium")                       # one high-severity issue
  expect_equal(sc$teams[team == 1]$priority, "Low")
})

test_that("one short interviewer is an interviewer issue, and two questionnaires make one issue", {
  sc <- ipm_scorecard(mini_res(function(team, interv, ds) ifelse(interv == 12, 15, 40), datasets = c("hh", "wm")))
  s <- sc$signals
  expect_equal(nrow(s[level == "team"]), 0)                                   # team-mates are normal: not the whole team
  expect_equal(nrow(s[level == "interviewer"]), 1)
  expect_equal(s[level == "interviewer"]$interv, 12)
  expect_equal(s[level == "interviewer"]$dataset, "hh, wm")
  expect_equal(s[level == "interviewer"]$type, "short_interviews")
  expect_equal(sc$teams[team == 1]$priority, "Medium")
})

test_that("questions flagged in most teams are questionnaire problems and do not count against teams", {
  cfg <- mics_autoconfig(make_synth(), "Synthetic"); cfg$period$start <- "2026-01-05"; cfg$period$days <- 28
  res <- run_ipm(cfg, write = FALSE, progress = function(...) NULL, keep_data = FALSE)
  t <- res$outputs$flaganal$tests
  # plant: question HC1 flagged for one interviewer in every team
  ints <- t[varname == "HC1", .SD[1], by = team]$interv
  t[varname == "HC1" & interv %in% ints, flag := 1L]
  res$outputs$flaganal$tests <- t
  sc <- ipm_scorecard(res)
  expect_true(sc$questions[question == "HC1"]$survey_wide)
  expect_true(any(sc$signals[level == "survey"]$question == "HC1", na.rm = TRUE))
  expect_gt(sc$survey$flag_rate, sc$survey$flag_rate_excl)
  expect_false(any(grepl("HC1", sc$signals[level != "survey"]$evidence)))
  s <- ipm_summary(res, sc)
  expect_true(any(grepl("HC1", s$sections$modules$bullets)))
  expect_match(s$sections$overall$bullets[1], "^Priorities: ")
})

test_that("summary, scorecard and plan list the same teams", {
  cfg <- mics_autoconfig(make_synth(), "Synthetic"); cfg$period$start <- "2026-01-05"; cfg$period$days <- 28
  res <- run_ipm(cfg, write = FALSE, progress = function(...) NULL, keep_data = FALSE)
  sc <- ipm_scorecard(res); s <- ipm_summary(res, sc); pl <- ipm_plan_rules(sc)
  want <- sc$teams[priority != "Low", team_label]
  expect_setequal(vapply(pl$teams, `[[`, "", "team"), want)
  if (length(want)) expect_true(all(vapply(want, function(w) any(startsWith(s$sections$teams$bullets, w)), TRUE)))
})
