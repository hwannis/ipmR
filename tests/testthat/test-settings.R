# Analysis settings: issue rules from the options, presets, what differs from the defaults, settings per survey

test_that("every adjustable parameter has a default and every preset only names known parameters", {
  sp <- ipm_param_spec(); d <- ipm_defaults()
  expect_true(all(names(sp) %in% names(d)))
  for (p in ipm_presets()) expect_true(all(names(p$values) %in% names(sp)))
  expect_equal(nrow(ipm_settings_changed(list(options = d))), 0)
  expect_equal(ipmR:::match_preset(d), "standard")
  for (k in names(ipm_presets())) expect_equal(ipmR:::match_preset(ipmR:::preset_values(k)), k)
  chg <- ipm_settings_changed(list(options = list(rule_team_ratio = 2, stata_compat = TRUE)))
  expect_setequal(chg$option, c("rule_team_ratio", "stata_compat"))
  expect_equal(chg$value[chg$option == "stata_compat"], "on")
  expect_true(is.na(ipmR:::match_preset(utils::modifyList(d, list(rule_team_ratio = 1.7)))))
})

test_that("the issue rules follow the settings", {
  cfg <- mics_autoconfig(make_synth(), "Synthetic"); cfg$period$start <- "2026-01-05"; cfg$period$days <- 28
  res <- run_ipm(cfg, write = FALSE, progress = function(...) NULL, keep_data = FALSE)
  a <- ipm_scorecard(res)
  expect_equal(a$survey$recent_weeks, 3)
  # loose rules give at least as many issues, strict ones at most as many
  lo <- res; lo$cfg$options <- utils::modifyList(lo$cfg$options, list(rule_team_ratio = 1.01, rule_interv_ratio = 1.01, rule_p = 0.5, rule_prio_medium = 1))
  hi <- res; hi$cfg$options <- utils::modifyList(hi$cfg$options, list(rule_team_ratio = 50, rule_interv_ratio = 50, rule_p = 1e-12, rule_prio_medium = 99))
  sl <- ipm_scorecard(lo); sh <- ipm_scorecard(hi)
  n <- function(s, t) sum(s$signals$type %in% t)
  expect_gte(n(sl, c("team_flags", "interviewer_flags")), n(a, c("team_flags", "interviewer_flags")))
  expect_equal(n(sh, c("team_flags", "interviewer_flags")), 0)
  expect_gte(sum(sl$teams$priority != "Low"), sum(a$teams$priority != "Low"))
  w <- res; w$cfg$options$recent_weeks <- 2
  expect_equal(ipm_scorecard(w)$survey$recent_weeks, 2)
  expect_match(ipmR:::rule_text(lo$cfg$options), "1.01 times")
  # without options (old results) the defaults apply
  old <- res; old$cfg$options <- NULL
  expect_equal(nrow(ipm_scorecard(old)$signals), nrow(a$signals))
})

test_that("settings are remembered per survey name, without folders or dates", {
  op <- options(ipmR.history_dir = tempfile("hist"), ipmR.history = TRUE); on.exit(options(op), add = TRUE)
  cfg <- mics_autoconfig(make_synth(), "Synthetic"); cfg$period$start <- "2026-01-05"
  cfg$survey_name <- "Somewhere MICS7 2026"; cfg$options$rule_team_ratio <- 2
  f <- ipm_settings_save(cfg); expect_true(file.exists(f))
  s <- ipm_settings_load("somewhere mics7 2026")              # same survey, typed differently
  expect_equal(s$survey_name, "Somewhere MICS7 2026"); expect_equal(s$options$rule_team_ratio, 2)
  expect_null(s$data_dir); expect_null(s$period$start); expect_false(is.null(s$saved))
  expect_true("Somewhere MICS7 2026" %in% ipm_settings_list()$name)
  expect_null(ipm_settings_load("Other survey"))
  cfg$survey_name <- "MICS survey"; expect_null(ipm_settings_save(cfg))  # unnamed surveys are not remembered
  options(ipmR.history = FALSE); cfg$survey_name <- "X"; expect_null(ipm_settings_save(cfg))
})
