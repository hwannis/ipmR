# Action follow-up: for each issue, who it was assigned to, whether it was done or escalated, with a note.
# Kept next to the run history of the survey (one file, every change appended), so that the next run can say
# "done on 12 Nov but still found".

action_states <- c(open = "Open", assigned = "Assigned", done = "Done", escalated = "Escalated")
action_file <- function(x, dir) file.path(dir, if (is.character(x)) x else history_survey_id(x), "actions.rds")
empty_actions <- function() setnames(data.table(issue_key = character(), status = character(), who = character(), note = character(),
                                                updated = as.POSIXct(character()), data_last = as.Date(character()), headline = character()), "issue_key", "key")

#' Action follow-up of the issues of a survey
#'
#' `ipm_actions()` reads the follow-up of a survey: the latest status of each issue (or every change with `log = TRUE`).
#' `ipm_action_set()` records a change. `ipm_action_status()` adds the follow-up to a table of issues, and marks the
#' issues that were set to done before this data export but are still found.
#' @param x a result of `run_ipm()` or a survey id
#' @param dir folder of the run history
#' @param log every change instead of the latest status of each issue
#' @export
ipm_actions <- function(x, dir = ipm_history_dir(), log = FALSE) {
  f <- action_file(x, dir)
  a <- if (file.exists(f)) tryCatch(as.data.table(readRDS(f)), error = function(e) empty_actions()) else empty_actions()
  if (log || !nrow(a)) return(a)
  a[order(updated)][, .SD[.N], by = key]
}

#' @rdname ipm_actions
#' @param key issue key (column `key` of the issues of `ipm_compare()`)
#' @param status one of "open", "assigned", "done", "escalated"
#' @param who person the action is assigned or escalated to
#' @param note free text
#' @param data_last date of the last interview of the data export the change was made on
#' @param headline headline of the issue, kept for the record
#' @export
ipm_action_set <- function(x, key, status = c("open", "assigned", "done", "escalated"), who = "", note = "",
                           data_last = if (is.character(x)) as.Date(NA) else history_dates(x)$last, headline = NA_character_, dir = ipm_history_dir()) {
  status <- match.arg(status)
  f <- action_file(x, dir); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  a <- ipm_actions(x, dir, log = TRUE)
  row <- data.table(issue_key = key, status = status, who = trimws(who %||% ""), note = trimws(note %||% ""), updated = Sys.time(),
                    data_last = as.Date(data_last), headline = headline)
  setnames(row, "issue_key", "key")   # data.table(key = ...) would set a sort key
  a <- rbind(a, row, fill = TRUE)
  tmp <- paste0(f, ".tmp"); saveRDS(a, tmp); file.rename(tmp, f)
  invisible(row)
}

#' @rdname ipm_actions
#' @param issues issues with a `key` column (from `ipm_compare()` or `ipm_history_update()`)
#' @param actions result of `ipm_actions()`
#' @param data_last date of the last interview of this run
#' @return `ipm_action_status()`: the issues with `action` (open / assigned / done / escalated), `action_who`,
#'   `action_note`, `action_updated` and `still_found` (set to done on an earlier export, still found now)
#' @export
ipm_action_status <- function(issues, actions, data_last = as.Date(NA)) {
  s <- copy(issues)
  if (!"key" %in% names(s)) s[, key := issue_key(s)]
  m <- match(s$key, actions$key)
  s[, `:=`(action = fifelse(is.na(m), "open", actions$status[m]), action_who = actions$who[m], action_note = actions$note[m],
           action_updated = actions$updated[m], action_export = actions$data_last[m])]
  s[, still_found := action == "done" & !is.na(action_export) & !is.na(data_last) & action_export < as.Date(data_last)]
  s[, action_export := NULL][]
}
