% Please edit documentation in R/questionnaire.R
\name{ipm_question_heat}
\alias{ipm_question_heat}
\title{Questionnaire heat map: questions (or modules) x teams}
\usage{
ipm_question_heat(res, sc, by = c("question", "module"), top = 40, hide_survey_wide = FALSE)
}
\arguments{
\item{res}{result of \code{run_ipm()}}

\item{sc}{result of \code{ipm_scorecard()}}

\item{by}{"question" or "module"}

\item{top}{number of rows (the questions or modules with the most flags)}

\item{hide_survey_wide}{leave out the questions (modules) flagged in most teams, to see the team problems}
}
\value{
a list: \code{rows} (one per question or module: dataset, module, question, description, tests, flags, rate, teams_flagged,
teams, survey_wide), \code{cells} (row x team: tests, flags, rate, interviewers flagged) and \code{teams} (team, team_label,
priority), or NULL without a flag analysis
}
\description{
Share of the flag tests of each question (or module) that are flagged in each team, for the questions with the most
flags. A question flagged in most teams is a questionnaire problem (wording, translation, CAPI routing); one flagged in a
single team is the team's.
}
