# Phase 4b: team and interviewer pages, supervisor brief
prof_res <- function() {
  g <- data.table::CJ(dataset = "hh", team = 1:3, k = 1:3, woi = 1:4)
  g[, interv := team * 10 + k]
  g[, median_minutes := data.table::fifelse(team == 3, 15, data.table::fifelse(interv == 12, 12, 40))][, n := 10L]
  g[, completion_rate := data.table::fifelse(interv == 21, 60, 98)]
  list(cfg = list(options = ipm_defaults(), survey_name = "Test survey"),
       team_info = data.table::data.table(team = 1:3, team_label = c("T1 (100)", "T2 (200)", "T3 (300)")),
       data_summary = data.table::data.table(dataset = "hh", records = 360, first_day = as.Date("2026-01-05"), last_day = as.Date("2026-02-01")),
       outputs = list(quesmtimes = list(data = g[, .(dataset, team, interv, woi, median_minutes, n)]),
                      quescrates = list(data = g[, .(dataset, team, interv, woi, completion_rate, n)])))
}

test_that("the team page has the team's issues, team first, its interviewers and its trends", {
  r <- prof_res(); sc <- ipm_scorecard(r)
  p <- ipm_team_profile(r, sc, 3)
  expect_equal(p$team$team, 3L); expect_equal(p$questionnaire, "hh")
  expect_true(all(p$issues$team == 3)); expect_equal(p$issues$level[1], "team")
  expect_true(any(p$issues$type == "team_duration"))
  expect_setequal(p$interviewers$interv, 31:33)
  expect_equal(p$interviewers$minutes, c(15, 15, 15))
  expect_equal(levels(p$trends$minutes$group), c("This team", "Other teams"))
  expect_equal(p$trends$minutes[group == "This team" & woi == 1]$value, 15)
  # interviewers with an issue come first
  p1 <- ipm_team_profile(r, sc, 1)
  expect_equal(p1$interviewers$interv[1], 12); expect_gt(p1$interviewers$issues[1], 0)
  expect_error(ipm_team_profile(r, sc, 9), "not in the scorecard")
})

test_that("the interviewer page compares the interviewer with the team and the survey", {
  r <- prof_res(); sc <- ipm_scorecard(r)
  p <- ipm_interviewer_profile(r, sc, 21)
  expect_equal(p$interviewer$team, 2L)
  f <- p$figures[what == "completion"]
  expect_equal(f$interviewer, 60); expect_equal(f$team, (60 + 98 + 98) / 3)
  expect_true(all(p$issues$interv == 21)); expect_true(any(p$issues$type == "low_completion"))
  expect_equal(levels(p$trends$minutes$group), c("This interviewer", "Team", "Survey"))
  expect_s3_class(ipm_trend_plot(p$trends$minutes, "x"), "ggplot")
  expect_error(ipm_interviewer_profile(r, sc, 99), "not in the results")
})

test_that("the supervisor brief has one page per team", {
  skip_if_not_installed("officer")
  r <- prof_res(); sc <- ipm_scorecard(r)
  f <- tempfile(fileext = ".docx"); on.exit(unlink(f), add = TRUE)
  ipm_supervisor_brief(r, sc, f, teams = c(1, 3))
  x <- officer::docx_summary(officer::read_docx(f))
  expect_equal(sum(grepl("supervisor brief$", x$text)), 2)
  expect_true(any(grepl("^T3 \\(300\\) - supervisor brief", x$text)))
  expect_true(any(grepl("This week, do", x$text)))
})
