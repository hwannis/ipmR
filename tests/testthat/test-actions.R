# Phase 4c: action follow-up and questionnaire heat map
act_res <- function(last = "2026-02-01", short = 12) {
  g <- data.table::CJ(dataset = "hh", team = 1:3, k = 1:3, woi = 1:3)
  g[, interv := team * 10 + k][, median_minutes := ifelse(interv %in% short, 15, 40)][, n := 10L]
  list(cfg = list(options = ipm_defaults(), survey_name = "Action test"),
       team_info = data.table::data.table(team = 1:3, team_label = c("T1", "T2", "T3")),
       data_summary = data.table::data.table(dataset = "hh", records = 270, first_day = as.Date("2026-01-05"), last_day = as.Date(last)),
       outputs = list(quesmtimes = list(data = g[, .(dataset, team, interv, woi, median_minutes, n)])))
}

test_that("a follow-up is recorded, its latest status is kept, and 'done but still found' is flagged", {
  dir <- tempfile("act"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r1 <- act_res(); sc1 <- ipm_scorecard(r1); c1 <- ipm_history_update(r1, sc1, dir, save = TRUE)
  k <- c1$issues[interv == 12]$key
  expect_length(k, 1)
  expect_equal(nrow(ipm_actions(r1, dir)), 0)
  ipm_action_set(r1, k, "assigned", "Supervisor T1", "observe Monday", dir = dir)
  ipm_action_set(r1, k, "done", "", "observed twice", dir = dir)
  a <- ipm_actions(r1, dir)
  expect_equal(nrow(a), 1); expect_equal(a$status, "done"); expect_equal(a$note, "observed twice")
  expect_equal(nrow(ipm_actions(r1, dir, log = TRUE)), 2)
  s1 <- ipm_action_status(c1$issues, a, as.Date("2026-02-01"))
  expect_equal(s1[key == k]$action, "done"); expect_false(s1[key == k]$still_found)
  expect_true(all(s1[key != k]$action == "open"))
  # next export: the issue is still there
  r2 <- act_res("2026-02-08"); c2 <- ipm_history_update(r2, ipm_scorecard(r2), dir, save = TRUE)
  s2 <- ipm_action_status(c2$issues, ipm_actions(r2, dir), as.Date("2026-02-08"))
  expect_true(s2[key == k]$still_found)
  expect_error(ipm_action_set(r1, k, "maybe", dir = dir))
})

test_that("the questionnaire heat map has one row per question and one cell per team tested", {
  ft <- data.table::CJ(dataset = "hh", varname = c("Q1", "Q2", "Q3"), team = 1:4, interv = 1:2)
  ft[, `:=`(module = "HC*", vardesc = paste("label", varname), interv = team * 10 + interv)]
  ft[, flag := as.integer((varname == "Q1") | (varname == "Q2" & team == 2 & interv == 21))]
  r <- list(outputs = list(flaganal = list(tests = ft)))
  sc <- list(teams = data.table::data.table(team = 1:4, team_label = paste0("T", 1:4), priority = "Low"),
             questions = data.table::data.table(dataset = "hh", question = c("Q1", "Q2", "Q3"), survey_wide = c(TRUE, FALSE, FALSE)))
  h <- ipm_question_heat(r, sc)
  expect_equal(h$rows$row, c("hh Q1", "hh Q2"))           # Q3 never flagged
  expect_equal(h$rows$teams_flagged, c(4L, 1L)); expect_equal(h$rows$survey_wide, c(TRUE, FALSE))
  expect_equal(h$cells[row == "hh Q2" & team == 2]$rate, 50); expect_equal(h$cells[row == "hh Q2" & team == 2]$interviewers, "21")
  expect_equal(nrow(h$cells), 8)
  expect_equal(ipm_question_heat(r, sc, hide_survey_wide = TRUE)$rows$row, "hh Q2")
  expect_equal(ipm_question_heat(r, sc, by = "module")$rows$row, "hh HC")
  expect_null(ipm_question_heat(list(outputs = list()), sc))
})

test_that("the shareable briefing is one self-contained UTF-8 page with the teams and the follow-up", {
  dir <- tempfile("act"); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  r <- act_res(short = 11:13); sc <- ipm_scorecard(r); cmp <- ipm_history_update(r, sc, dir, save = TRUE)
  expect_true("T1" %in% sc$teams[priority != "Low", team_label])
  ipm_action_set(r, cmp$issues[level == "team" & team == 1]$key[1], "assigned", "Sup T1", dir = dir)
  f <- tempfile(fileext = ".html"); on.exit(unlink(f), add = TRUE)
  ipm_briefing_html(r, sc, f, cmp, ipm_actions(r, dir))
  x <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(x, "<meta charset=\"utf-8\">", fixed = TRUE)
  expect_match(x, "Action test", fixed = TRUE)
  expect_match(x, "Follow-up: 1 assigned", fixed = TRUE)
  expect_match(x, "→", fixed = TRUE)
  expect_false(grepl("<script|https?://", x))          # nothing to load from the internet
})
