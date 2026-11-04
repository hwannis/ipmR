synth060 <- function() {
  dir <- make_synth()
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-19"; cfg$period$days <- 13
  cfg$datasets$hh$flag_modules <- "HC*"; cfg$datasets$hh$flag_units <- "HC4U"
  cfg$results_dir <- file.path(dir, "results")
  list(cfg = cfg, res = run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE))
}

test_that("teams defined by ranges are labelled with the supervisor code, and recognised however they are written", {
  x <- synth060(); res <- x$res
  expect_identical(x$cfg$datasets$hh$supervisor, "HH4")
  expect_identical(res$team_info$team_label, c("T1 (10)", "T2 (20)", "T3 (30)"))
  labs <- res$team_info$team_label
  expect_identical(ipmR:::canon_team("T2", labs), "T2 (20)")
  expect_identical(ipmR:::canon_team("team 3", labs), "T3 (30)")
  expect_identical(ipmR:::canon_team("20", labs), "T2 (20)")
  expect_identical(ipmR:::canon_team("T9", labs), "T9")
  sc <- ipm_scorecard(res)
  for (q in c("What about team 2?", "Is T2 a problem?", "Supervisor 20: what to do?"))
    expect_identical(ipmR:::ai_ask_prompt(q, res, sc)$team, "T2 (20)")
  expect_length(ipmR:::ai_ask_prompt("Which team first?", res, sc)$team, 0)
  # the plan validation accepts the short label written by a model
  pl <- ipm_plan_rules(sc); pl$teams[[1]]$team <- sub(" .*", "", pl$teams[[1]]$team)
  v <- ipm_validate_plan(pl, sc, as.character(ipmR:::ai_json(ipm_ai_evidence(res, sc))))
  expect_true(v$teams[[1]]$team %in% labs)
})

test_that("compact evidence keeps only High/Medium teams and is smaller", {
  res <- synth060()$res; sc <- ipm_scorecard(res)
  full <- ipm_ai_evidence(res, sc); cmp <- ipm_ai_evidence(res, sc, compact = TRUE)
  expect_true(nchar(ipmR:::ai_json(cmp)) < nchar(ipmR:::ai_json(full)))
  expect_setequal(vapply(cmp$teams, `[[`, "", "team"), sc$teams[priority %in% c("High", "Medium"), team_label])
  st <- ipm_ai_settings("anthropic", api_key = "x")
  expect_true(ipm_ai_preview(res, sc, st, "plan", compact = TRUE)$chars < ipm_ai_preview(res, sc, st, "plan")$chars)
})

test_that("descriptive charts shade the weeks of the period of analysis, or show only them", {
  x <- synth060(); res <- x$res
  g <- res$outputs$quescrates$sections[[1]]$plots[[1]]
  expect_match(g$labels$caption, "Shaded: weeks of the period of analysis \\(weeks 3-4\\)")
  expect_true(any(vapply(g$layers, function(l) inherits(l$geom, "GeomRect"), logical(1))))
  cfg <- x$cfg; cfg$options$period_weeks_only <- TRUE
  cfg$outputs[] <- lapply(names(cfg$outputs), function(k) k == "quescrates")
  r2 <- run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE)
  g2 <- r2$outputs$quescrates$sections[[1]]$plots[[1]]
  expect_true(all(g2$data$woi %in% 3:4))
  expect_match(g2$labels$caption, "Only the weeks of the period of analysis")
  expect_true(nrow(r2$outputs$quescrates$data) > nrow(g2$data))        # the data (and the scorecard) keep all weeks
})

test_that("the summary no longer says 'x times chance' and reports records per household", {
  res <- synth060()$res
  b <- ipm_summary(res)$sections$overall$bullets
  expect_false(any(grepl("times what chance|times the chance", b)))
  expect_true(any(grepl("nominal level", b)))
  cl <- res$outputs$flaganal$clustering
  expect_equal(cl[dataset == "hh"]$per_household, 1)
  expect_false(grepl("times the .* expected by chance", ipm_plan_rules(ipm_scorecard(res))$overall_assessment))
})
