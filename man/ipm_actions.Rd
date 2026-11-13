% Please edit documentation in R/actions.R
\name{ipm_actions}
\alias{ipm_actions}
\alias{ipm_action_set}
\alias{ipm_action_status}
\title{Action follow-up of the issues of a survey}
\usage{
ipm_actions(x, dir = ipm_history_dir(), log = FALSE)

ipm_action_set(x, key, status = c("open", "assigned", "done", "escalated"), who = "", note = "",
  data_last = if (is.character(x)) as.Date(NA) else history_dates(x)$last, headline = NA_character_, dir = ipm_history_dir())

ipm_action_status(issues, actions, data_last = as.Date(NA))
}
\arguments{
\item{x}{a result of \code{run_ipm()} or a survey id}

\item{dir}{folder of the run history}

\item{log}{every change instead of the latest status of each issue}

\item{key}{issue key (column \code{key} of the issues of \code{ipm_compare()})}

\item{status}{one of "open", "assigned", "done", "escalated"}

\item{who}{person the action is assigned or escalated to}

\item{note}{free text}

\item{data_last}{date of the last interview of the data export (of the change for \code{ipm_action_set()}, of this run for \code{ipm_action_status()})}

\item{headline}{headline of the issue, kept for the record}

\item{issues}{issues with a \code{key} column (from \code{ipm_compare()} or \code{ipm_history_update()})}

\item{actions}{result of \code{ipm_actions()}}
}
\value{
\code{ipm_actions()}: a table with \code{key}, \code{status}, \code{who}, \code{note}, \code{updated}, \code{data_last} and \code{headline}.
\code{ipm_action_status()}: the issues with \code{action}, \code{action_who}, \code{action_note}, \code{action_updated} and
\code{still_found} (set to done on an earlier data export and still found now).
}
\description{
For each issue: who it was assigned to, whether it was done or escalated, with a note. The follow-up is kept next to the
run history of the survey (every change appended), so that the next run can show an issue set to done that is still found.
\code{ipm_history_forget()} deletes it with the history.
}
