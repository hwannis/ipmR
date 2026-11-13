# Run history: each run keeps a small snapshot of the issues found - team and interviewer codes, figures and
# headlines, no records - so that the next run of the same survey can say what is new, what persists and what was
# resolved. One folder per survey (name + date of the first interview), one file per data export (date of the last
# interview): re-running the same export replaces its snapshot.

#' Folder where the run history is kept
#'
#' `options(ipmR.history_dir = ...)` or the environment variable `IPM_HISTORY_DIR`; otherwise the user data folder
#' of ipmR. `options(ipmR.history = FALSE)` or `IPM_HISTORY=off` turns the history off.
#' @export
ipm_history_dir <- function() {
  d <- getOption("ipmR.history_dir", Sys.getenv("IPM_HISTORY_DIR", ""))
  if (!nzchar(d)) d <- file.path(tools::R_user_dir("ipmR", "data"), "history")
  d
}
history_on <- function() !isFALSE(getOption("ipmR.history", TRUE)) && !tolower(Sys.getenv("IPM_HISTORY", "on")) %in% c("off", "false", "0", "no")

history_dates <- function(res) {
  f <- suppressWarnings(as.Date(res$data_summary$first_day)); l <- suppressWarnings(as.Date(res$data_summary$last_day))
  list(first = if (any(!is.na(f))) min(f, na.rm = TRUE) else as.Date(NA), last = if (any(!is.na(l))) max(l, na.rm = TRUE) else as.Date(NA))
}
history_survey_id <- function(res) {
  nm <- gsub("^-+|-+$", "", gsub("[^a-z0-9]+", "-", tolower(ascii_fold(res$cfg$survey_name %||% "survey"))))
  first <- history_dates(res)$first
  paste0(if (nzchar(nm)) nm else "survey", "_", if (is.na(first)) "nodate" else format(first, "%Y%m%d"))
}
issue_key <- function(s) ifelse(s$level == "survey", paste("survey", s$type, s$dataset, s$module, s$question, sep = "|"),
                                paste(s$level, s$area, s$team, ifelse(is.na(s$interv), "", fmt_int(s$interv)), sep = "|"))

ipm_snapshot <- function(res, sc) {
  s <- copy(sc$signals); s[, key := issue_key(s)]
  keep <- intersect(c("key", "level", "type", "area", "team", "team_label", "interv", "dataset", "module", "question", "severity", "value", "benchmark", "headline"), names(s))
  list(survey = history_survey_id(res), name = res$cfg$survey_name %||% "", data_last = history_dates(res)$last, saved = Sys.time(),
       version = IPMR_VERSION, period = sc$survey$period %||% NA_character_, issues = s[, keep, with = FALSE],
       teams = sc$teams[, intersect(c("team", "team_label", "priority", "points", "signals"), names(sc$teams)), with = FALSE])
}

#' Earlier runs of a survey
#' @param x a result of [run_ipm()] or a survey id
#' @export
ipm_history <- function(x, dir = ipm_history_dir()) {
  id <- if (is.character(x)) x else history_survey_id(x)
  fs <- sort(list.files(file.path(dir, id), pattern = "^[0-9]{8}[.]rds$", full.names = TRUE))
  Filter(Negate(is.null), lapply(fs, function(f) tryCatch(readRDS(f), error = function(e) NULL)))
}

#' Compare the issues of this run with the earlier runs of the same survey
#'
#' Each issue gets a status: `first` (no earlier run), `new`, or `persisting` with the number of runs in a row
#' (`runs`), the date of the data export where it first appeared (`since`) and its previous headline (`was`).
#' Issues of the last run that are gone are listed in `resolved` (unless another issue of the same team and area
#' replaces them, e.g. an interviewer's short interviews that became the whole team's).
#' @param data_last date of the last interview of this run; only earlier exports are compared
#' @export
ipm_compare <- function(sc, hist, data_last) {
  s <- copy(sc$signals); s[, key := issue_key(s)]
  teams <- copy(sc$teams)
  prev <- Filter(function(h) !is.na(h$data_last) && !is.na(data_last) && h$data_last < data_last, hist)
  if (!length(prev)) {
    s[, `:=`(status = "first", runs = 1L, since = as.Date(NA), was = NA_character_)]
    teams[, prev_priority := NA_character_]
    return(list(issues = s, resolved = s[0], previous = NULL, teams = teams, runs = 0L))
  }
  n <- nrow(s); runs <- rep(1L, n); alive <- rep(TRUE, n); since <- rep(as.Date(data_last), n)
  for (h in rev(prev)) { inn <- alive & s$key %in% h$issues$key; runs <- runs + inn; since[inn] <- h$data_last; alive <- inn }
  last <- prev[[length(prev)]]
  s[, `:=`(runs = runs, since = since, status = fifelse(runs > 1L, "persisting", "new"), was = last$issues$headline[match(key, last$issues$key)])]
  akey <- function(a) sub("^hhm_.*", "hhm", a)
  now <- paste(s$team, akey(s$area))
  resolved <- last$issues[!key %in% s$key & !(level != "survey" & paste(team, akey(area)) %in% now)]
  teams[, prev_priority := last$teams$priority[match(team, last$teams$team)]]
  list(issues = s, resolved = resolved, previous = list(data_last = last$data_last, saved = last$saved, period = last$period, teams = last$teams,
                                                       issues = last$issues), teams = teams, runs = length(prev))
}

#' Compare a run with the history of its survey, then add it to the history
#' @return the comparison (see [ipm_compare()]), with `saved` = the snapshot file (NULL if the history is off)
#' @export
ipm_history_update <- function(res, sc, dir = ipm_history_dir(), save = history_on()) {
  last <- history_dates(res)$last
  hist <- if (save) tryCatch(ipm_history(res, dir), error = function(e) list()) else list()
  cmp <- ipm_compare(sc, hist, last)
  cmp$saved <- NULL
  if (save && !is.na(last)) cmp$saved <- tryCatch({
    snap <- ipm_snapshot(res, sc); d <- file.path(dir, snap$survey); dir.create(d, recursive = TRUE, showWarnings = FALSE)
    f <- file.path(d, paste0(format(last, "%Y%m%d"), ".rds")); saveRDS(snap, f); f
  }, error = function(e) { message("Run history not saved: ", conditionMessage(e)); NULL })
  cmp$survey <- history_survey_id(res)
  cmp
}

#' Forget the run history of a survey
#' @export
ipm_history_forget <- function(x, dir = ipm_history_dir()) {
  id <- if (is.character(x)) x else history_survey_id(x)
  unlink(file.path(dir, id), recursive = TRUE)
}

# The 8 areas of the team health grid, from the area of each issue
health_areas <- c(flags = "Flags", duration = "Interview length", completion = "Completion", hhm = "Household listing",
                  dk = "Don't know", births = "Birth history", anthropometry = "Anthropometry", fl = "FL assessment")
health_area <- function(area) { a <- sub("^hhm_.*", "hhm", sub("^module .*", "flags", area)); ifelse(a %in% names(health_areas), a, NA_character_) }
