% Please edit documentation in R/briefing_html.R
\name{ipm_briefing_html}
\alias{ipm_briefing_html}
\title{Write the briefing as one HTML page to share}
\usage{
ipm_briefing_html(res, sc, file, cmp = NULL, actions = NULL)
}
\arguments{
\item{res}{result of \code{run_ipm()}}

\item{sc}{result of \code{ipm_scorecard()}}

\item{file}{the .html file to write}

\item{cmp}{result of \code{ipm_compare()} or \code{ipm_history_update()}, or NULL}

\item{actions}{result of \code{ipm_actions()}, or NULL (no follow-up shown)}
}
\value{
the file name, invisibly
}
\description{
The briefing of the app on one self-contained page that opens offline in any browser and prints on A4: the top numbers,
the teams to act on with their first action and follow-up, the interviewers to observe, the team health grid, the
questionnaire problems and what was resolved. Codes and figures only, no records.
}
