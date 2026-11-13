% Please edit documentation in R/history.R
\name{ipm_history}
\alias{ipm_history}
\alias{ipm_history_dir}
\alias{ipm_compare}
\alias{ipm_history_update}
\alias{ipm_history_forget}
\title{Run history: compare a run with the earlier runs of the same survey}
\usage{
ipm_history_dir()

ipm_history(x, dir = ipm_history_dir())

ipm_compare(sc, hist, data_last)

ipm_history_update(res, sc, dir = ipm_history_dir(), save = history_on())

ipm_history_forget(x, dir = ipm_history_dir())
}
\arguments{
\item{x}{a result of \code{run_ipm()} or a survey id}

\item{dir}{folder where the history is kept}

\item{sc}{result of \code{ipm_scorecard()}}

\item{hist}{earlier runs, from \code{ipm_history()}}

\item{data_last}{date of the last interview of this run; only earlier exports are compared}

\item{res}{result of \code{run_ipm()}}

\item{save}{add this run to the history}
}
\value{
\code{ipm_history()}: the snapshots of the earlier runs. \code{ipm_compare()} and \code{ipm_history_update()}: a list with
\code{issues} (the issues of this run with \code{status} new / persisting / first, \code{runs} in a row, \code{since} and the previous headline \code{was}),
\code{resolved} (issues of the last run that are gone), \code{teams} (with \code{prev_priority}), \code{previous} and \code{runs}.
}
\description{
Each run keeps a small snapshot of the issues found - team and interviewer codes, figures and headlines, no records - in one folder
per survey (survey name and date of the first interview), one file per data export (date of the last interview). The next run says
which issues are new, which persist and which were resolved.

The folder is \code{options(ipmR.history_dir = ...)} or the environment variable \code{IPM_HISTORY_DIR}, otherwise the user data folder
of ipmR. \code{options(ipmR.history = FALSE)} or \code{IPM_HISTORY=off} turns the history off.
}
