% Please edit documentation in R/profile.R
\name{ipm_team_profile}
\alias{ipm_team_profile}
\alias{ipm_interviewer_profile}
\alias{ipm_trend_plot}
\alias{ipm_supervisor_brief}
\title{Team and interviewer pages, supervisor brief}
\usage{
ipm_team_profile(res, sc, team, cmp = NULL)

ipm_interviewer_profile(res, sc, interv, cmp = NULL)

ipm_trend_plot(tr, title = NULL)

ipm_supervisor_brief(res, sc, file, teams = sc$teams[priority \%in\% c("High", "Medium"), team], cmp = NULL)
}
\arguments{
\item{res}{result of \code{run_ipm()}}

\item{sc}{result of \code{ipm_scorecard()}}

\item{team}{team number}

\item{cmp}{result of \code{ipm_compare()} or \code{ipm_history_update()}, or NULL}

\item{interv}{interviewer code}

\item{tr}{one item of \code{trends} from \code{ipm_team_profile()} or \code{ipm_interviewer_profile()}}

\item{title}{chart title}

\item{file}{the .docx to write}

\item{teams}{team numbers, one page each (default: High and Medium priority teams)}
}
\value{
\code{ipm_team_profile()}: a list with \code{team} (one row of the team scorecard), \code{issues} (team issues first, then interviewers),
\code{interviewers} (one row per interviewer: flag rate, interview length, completion, issues), \code{modules} (modules where the team stands out),
\code{trends} (weekly series, team vs the other teams) and \code{questionnaire} (the questionnaire used for length and completion).

\code{ipm_interviewer_profile()}: a list with \code{interviewer} (flag rate vs team and survey), \code{issues}, \code{figures}
(interview length and completion per questionnaire vs the team and the survey), \code{modules} (modules with the most flags,
questionnaire problems left out) and \code{trends}.

\code{ipm_trend_plot()}: a ggplot. \code{ipm_supervisor_brief()}: the file name, invisibly.
}
\description{
Everything about one team or one interviewer, from the run and its scorecard: the issues with their figures and actions, the
interviewers, weekly trends and the modules that stand out. \code{ipm_supervisor_brief()} writes one page per team for the
supervisor: what to do this week, the team issues, the interviewers to observe and a trend chart.

Interview length and completion use the household questionnaire when there is one, otherwise the questionnaire with the most interviews.
}
