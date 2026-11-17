% Please edit documentation in R/ai.R
\name{ipm_ai_estimate}
\alias{ipm_ai_estimate}
\title{Size, cost and time of an AI action plan, before it is sent}
\usage{
ipm_ai_estimate(res, sc = ipm_scorecard(res), st = ipm_ai_settings(), language = "English",
  max_teams = 30, compact = FALSE, priorities = NULL)
}
\arguments{
\item{res, sc, st, language, max_teams, compact, priorities}{as in \code{ipm_ai_plan()}}
}
\value{
a list: \code{teams} (number of teams in the plan), \code{input_tokens}, \code{output_tokens} (likely),
\code{output_max} (if the model thinks at length), \code{usd} (likely and high cost at list prices; NA when the
price of the model is not known), \code{minutes} (likely range) and \code{local} (TRUE for a local model: no cost)
}
\description{
Estimates what an AI action plan request will send and cost, so the user can choose fewer teams or the compact
evidence first. Token counts are approximate (about 3.2 characters per token for the evidence); prices are the
list prices of Claude models and are a guide only.
}
