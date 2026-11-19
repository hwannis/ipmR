# Analysis settings: the adjustable parameters (with labels, groups and limits for the app), presets, what differs
# from the defaults (listed in the report), and the settings remembered per survey (next to the run history).

#' Adjustable analysis parameters
#'
#' One entry per option of [ipm_defaults()] that the app shows under "Advanced analysis": its group
#' (`flags` = flag tests, `rules` = issue rules, `outputs` = output details), label, type and limits, and a short help.
#' @return named list
#' @export
ipm_param_spec <- function() {
  p <- function(group, label, type = "num", min = NA, max = NA, step = NA, help = "", choices = NULL)
    list(group = group, label = label, type = type, min = min, max = max, step = step, help = help, choices = choices)
  list(
    levelsig = p("flags", "Significance level of a flag", "num", 0.00001, 0.05, 0.0005,
                 "A question is flagged for an interviewer when the answers differ from the team-mates' with p below this level. 0.001: about 1 test in 1,000 is flagged by chance."),
    test_min = p("flags", "Minimum answers per interviewer", "int", 2, 50, 1,
                 "Interviewers with fewer answers to a question are not tested on it (the team needs twice as many)."),
    test_min_nonmissing = p("flags", "Count only answered cases for the minimum", "lgl",
                            help = "On: the minimum counts the interviewer's answers to the question. Off: it counts the records (Stata IPM)."),
    max_uniq = p("flags", "Most answer categories for a chi-square test", "int", 2, 200, 1,
                 "Questions with more distinct values are treated as numbers (t-test)."),
    highly_flag = p("flags", "Highly flagged question (% of interviewers)", "num", 1, 100, 1,
                    "Questions flagged for at least this share of the interviewers tested are listed as highly flagged."),
    woi_anchor = p("flags", "Week 1 starts", "select", choices = c("Per dataset (Stata IPM)" = "dataset", "Same for all datasets (household questionnaire)" = "survey"),
                   help = "Week 1 of each questionnaire starts on its own first busy day, or on that of the household questionnaire for all."),
    stata_compat = p("flags", "Stata-compatible numbers", "lgl",
                     help = "Reproduces the Stata IPM exactly: the period covers one day more, records up to 6 days before week 1 count in week 1, and the minimum counts records."),
    recent_weeks = p("rules", "Weeks for the descriptive issues", "int", 1, 12, 1,
                     "Interview length, completion, 'don't know', household listing, birth history, anthropometry and FL issues use the last n weeks."),
    rule_sw_share = p("rules", "Questionnaire problem: share of teams", "num", 0.2, 1, 0.05,
                      "A question flagged in at least this share of the teams is a questionnaire problem: listed once and left out of the team and interviewer comparisons."),
    rule_sw_min_teams = p("rules", "Questionnaire problem: at least n teams", "int", 2, 20, 1,
                          "...and flagged in at least this many teams."),
    rule_p = p("rules", "Statistical threshold for an excess (p <)", "num", 0.0001, 0.1, 0.001,
               "A team, team module or interviewer excess of flags must have a binomial p below this."),
    rule_team_ratio = p("rules", "Whole-team excess from (x survey rate)", "num", 1.1, 5, 0.1,
                        "The team's flag rate must be at least this many times the survey rate (questionnaire problems left out)."),
    rule_team_high = p("rules", "Whole-team excess of high severity from", "num", 1.2, 10, 0.1, "Times the survey rate."),
    rule_module_ratio = p("rules", "Team module excess from (x other teams)", "num", 1.5, 10, 0.5,
                          "A module is flagged in a team when its rate is at least this many times the module's rate in the other teams (high severity from one more)."),
    rule_interv_ratio = p("rules", "Interviewer excess from (x survey rate)", "num", 1.1, 10, 0.1,
                          "The interviewer's flag rate must be at least this many times the survey rate."),
    rule_interv_high = p("rules", "Interviewer excess of high severity from", "num", 1.2, 20, 0.1, "Times the survey rate."),
    rule_prio_high = p("rules", "High priority: high-severity issues", "int", 1, 10, 1,
                       "A team is High priority when the whole team has an excess of flags, or it has at least this many high-severity issues."),
    rule_prio_medium = p("rules", "Medium priority: issues", "int", 1, 20, 1,
                         "A team is Medium priority with 1 high-severity issue, or at least this many issues."),
    period_weeks_only = p("outputs", "Charts show only the weeks of the period", "lgl",
                          help = "Descriptive charts (outputs 2-8 and FL) show only the weeks of the period of analysis; otherwise the whole fieldwork, with those weeks shaded."),
    maxquestime = p("outputs", "Longest interview counted (minutes)", "int", 10, 600, 5,
                    "Longer interview durations are counted as this many minutes (clock errors, interviews left open)."),
    exclude_height_codes = p("outputs", "Leave out height codes 900 and above", "lgl",
                             help = "Not-measured codes such as 999.6 are left out of the height heaping.")
  )
}

#' Presets of the analysis settings
#'
#' Each preset is a set of values for the options of [ipm_param_spec()]; options not named keep their default.
#' @return named list of list(label, help, values)
#' @export
ipm_presets <- function() list(
  standard = list(label = "Standard (recommended)", help = "The default settings: for the routine check every 1-2 weeks.", values = list()),
  early = list(label = "Early fieldwork", help = "First 2-3 weeks: the last 2 weeks only and lower thresholds, to catch problems while retraining is still easy. More issues, some of them false alarms.",
               values = list(recent_weeks = 2, rule_team_ratio = 1.3, rule_interv_ratio = 1.5, rule_prio_medium = 2)),
  strict = list(label = "Strict", help = "Fewer, stronger signals: for late fieldwork, large surveys or when teams are already well followed.",
                values = list(levelsig = 0.0001, rule_p = 0.001, rule_team_ratio = 2, rule_team_high = 3, rule_interv_ratio = 3, rule_interv_high = 5, rule_prio_medium = 4)),
  stata = list(label = "Stata-compatible", help = "Reproduces the numbers of the Stata IPM application (flag analysis); the issue rules are the standard ones.",
               values = list(stata_compat = TRUE, test_min_nonmissing = FALSE, woi_anchor = "dataset"))
)

# value of each adjustable option for a preset
preset_values <- function(preset) {
  d <- ipm_defaults()[names(ipm_param_spec())]
  utils::modifyList(d, (ipm_presets()[[preset]] %||% ipm_presets()$standard)$values)
}
# preset matching the options exactly, or NA
match_preset <- function(o) {
  for (k in names(ipm_presets())) { v <- preset_values(k)
    if (all(vapply(names(v), function(n) same_value(o[[n]], v[[n]]), logical(1)))) return(k) }
  NA_character_
}
same_value <- function(a, b) {
  if (is.null(a)) a <- NA
  if (is.numeric(a) && is.numeric(b)) isTRUE(all.equal(as.numeric(a), as.numeric(b))) else identical(as.character(a), as.character(b))
}

#' Analysis settings that differ from the defaults
#' @param cfg configuration (or its `options`)
#' @return data.frame(option, setting, value, default)
#' @export
ipm_settings_changed <- function(cfg) {
  o <- if (!is.null(cfg$options)) cfg$options else cfg
  o <- utils::modifyList(ipm_defaults(), o %||% list())
  sp <- ipm_param_spec(); d <- ipm_defaults()
  fmt <- function(x, s) if (s$type == "lgl") (if (isTRUE(x)) "on" else "off") else if (s$type == "select") names(s$choices)[match(x, s$choices)] %||% as.character(x) else format(x, scientific = FALSE, trim = TRUE)
  ch <- names(sp)[!vapply(names(sp), function(n) same_value(o[[n]], d[[n]]), logical(1))]
  data.frame(option = ch, setting = vapply(ch, function(n) sp[[n]]$label, ""), value = vapply(ch, function(n) fmt(o[[n]], sp[[n]]), ""),
             default = vapply(ch, function(n) fmt(d[[n]], sp[[n]]), ""), row.names = NULL, stringsAsFactors = FALSE)
}

# the priority rule in words, with the thresholds in use
rule_text <- function(o) {
  o <- utils::modifyList(ipm_defaults(), o %||% list())
  sprintf("High = the team as a whole has an excess of flags (at least %s times the survey rate, p < %s) or %d or more high-severity issues; Medium = 1 high-severity issue or %d or more issues; Low = otherwise",
          format(o$rule_team_ratio), format(o$rule_p, scientific = FALSE), as.integer(o$rule_prio_high), as.integer(o$rule_prio_medium))
}

# ---- settings remembered per survey (variable names and options only - no data) ----
survey_slug <- function(name) { s <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(ascii_fold(name %||% "")))); if (nzchar(s)) s else "survey" }
settings_dir <- function() file.path(ipm_history_dir(), "settings")

#' Remember the settings of a survey
#'
#' Saves the settings (datasets, variables, teams, modules, outputs, options - no data, no folders, no dates) under the
#' survey name, next to the run history, so that the next data export of the same survey can use them. Turned off
#' with the run history (`IPM_HISTORY=off`).
#' @param cfg configuration
#' @return the file, invisibly (NULL when not saved)
#' @export
ipm_settings_save <- function(cfg) {
  nm <- trimws(cfg$survey_name %||% "")
  if (!history_on() || !nzchar(nm) || identical(nm, "MICS survey")) return(invisible(NULL))
  d <- settings_dir(); dir.create(d, recursive = TRUE, showWarnings = FALSE)
  cfg$data_dir <- NULL; cfg$results_dir <- NULL; cfg$fieldwork <- NULL; cfg$period$start <- NULL
  cfg$saved <- format(Sys.time(), "%Y-%m-%d %H:%M"); cfg$ipmR_version <- IPMR_VERSION
  f <- file.path(d, paste0(survey_slug(nm), ".yml")); write_config(cfg, f)
  invisible(f)
}

#' Settings remembered for a survey
#' @param name survey name
#' @return the configuration, or NULL
#' @export
ipm_settings_load <- function(name) {
  f <- file.path(settings_dir(), paste0(survey_slug(name), ".yml"))
  if (!nzchar(trimws(name %||% "")) || !file.exists(f)) return(NULL)
  tryCatch(yaml::read_yaml(f), error = function(e) NULL)
}

#' Surveys with remembered settings
#' @return data.frame(name, saved)
#' @export
ipm_settings_list <- function() {
  fs <- list.files(settings_dir(), pattern = "[.]yml$", full.names = TRUE)
  x <- lapply(fs, function(f) tryCatch(yaml::read_yaml(f)[c("survey_name", "saved")], error = function(e) NULL))
  x <- Filter(function(z) !is.null(z$survey_name), x)
  data.frame(name = vapply(x, function(z) z$survey_name, ""), saved = vapply(x, function(z) z$saved %||% "", ""), stringsAsFactors = FALSE)
}
