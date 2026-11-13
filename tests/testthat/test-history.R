# Phase 4a: run history - new, persisting and resolved issues between runs of the same survey
hist_res <- function(last, short) {
  g <- data.table::CJ(dataset = "hh", team = 1:3, k = 1:3, woi = 1:3)
  g[, interv := team * 10 + k][, median_minutes := ifelse(interv %in% short, 15, 40)][, n := 10L]
  list(cfg = list(options = ipm_defaults(), survey_name = "Test survey"),
       team_info = data.table::data.table(team = 1:3, team_label = c("T1", "T2", "T3")),
       data_summary = data.table::data.table(dataset = "hh", records = 300, first_day = as.Date("2026-01-05"), last_day = as.Date(last)),
       outputs = list(quesmtimes = list(data = g[, .(dataset, team, interv, woi, median_minutes, n)])))
}

test_that("issues are new, then persisting, then resolved", {
  dir <- tempfile("hist"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r1 <- hist_res("2026-02-01", c(12, 22)); c1 <- ipm_history_update(r1, ipm_scorecard(r1), dir, save = TRUE)
  expect_true(all(c1$issues$status == "first")); expect_equal(c1$runs, 0L); expect_true(file.exists(c1$saved))

  r2 <- hist_res("2026-02-08", c(12, 31)); c2 <- ipm_history_update(r2, ipm_scorecard(r2), dir, save = TRUE)
  i <- c2$issues[level == "interviewer"]
  expect_equal(i[interv == 12]$status, "persisting"); expect_equal(i[interv == 12]$runs, 2L)
  expect_equal(i[interv == 12]$since, as.Date("2026-02-01"))
  expect_equal(i[interv == 31]$status, "new")
  expect_equal(c2$resolved$interv, 22)

  # the same export again replaces its snapshot and is still compared with the earlier export
  c2b <- ipm_history_update(r2, ipm_scorecard(r2), dir, save = TRUE)
  expect_equal(c2b$issues[interv == 12]$runs, 2L)
  expect_length(ipm_history(r2, dir), 2)
  ipm_history_forget(r2, dir); expect_length(ipm_history(r2, dir), 0)
})

test_that("an interviewer issue that became the whole team's is not 'resolved'", {
  dir <- tempfile("hist"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r1 <- hist_res("2026-02-01", 31); ipm_history_update(r1, ipm_scorecard(r1), dir, save = TRUE)
  r2 <- hist_res("2026-02-08", 31:33); c2 <- ipm_history_update(r2, ipm_scorecard(r2), dir, save = TRUE)
  expect_equal(nrow(c2$issues[level == "team" & team == 3]), 1)
  expect_equal(nrow(c2$resolved), 0)
})

test_that("no history is written when it is off", {
  dir <- tempfile("hist"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r1 <- hist_res("2026-02-01", 12); c1 <- ipm_history_update(r1, ipm_scorecard(r1), dir, save = FALSE)
  expect_null(c1$saved); expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("the briefing lists teams to act on, interviewers and the team health grid", {
  dir <- tempfile("hist"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r1 <- hist_res("2026-02-01", 12); ipm_history_update(r1, ipm_scorecard(r1), dir, save = TRUE)
  r2 <- hist_res("2026-02-08", c(12, 31:33)); sc <- ipm_scorecard(r2)
  b <- ipm_briefing(sc, ipm_history_update(r2, sc, dir, save = TRUE))
  expect_setequal(b$teams$team, sc$teams[priority != "Low", team])
  expect_equal(b$interviewers$interv, 12)
  expect_match(b$interviewers$status, "^2nd run in a row")
  expect_false(is.na(b$grid[team == 3 & harea == "duration", sev])); expect_true(is.na(b$grid[team == 2 & harea == "duration", sev]))
  expect_equal(b$kpi[what == "teams", before], 1)
  b0 <- ipm_briefing(sc)                                   # no history
  expect_true(all(is.na(b0$kpi$before))); expect_null(b0$previous)
})
