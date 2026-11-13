% Please edit documentation in R/briefing.R
\name{ipm_briefing}
\alias{ipm_briefing}
\title{Contents of the briefing page}
\usage{
ipm_briefing(sc, cmp = NULL)
}
\arguments{
\item{sc}{result of \code{ipm_scorecard()}}

\item{cmp}{result of \code{ipm_compare()} or \code{ipm_history_update()}; NULL when there is no history}
}
\value{
a list: \code{kpi} (now and at the last run), \code{teams} (one item per High/Medium team), \code{interviewers}
(one item per interviewer with a high-severity issue), \code{questionnaire}, \code{resolved}, \code{grid} (team x area),
\code{previous}, \code{runs} and \code{survey}
}
\description{
What to act on this week, built from the scorecard and its comparison with the last run of the same survey.
}
