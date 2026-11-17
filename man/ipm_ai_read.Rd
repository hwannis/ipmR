% Please edit documentation in R/ai_read.R
\name{ipm_ai_read}
\alias{ipm_ai_read}
\title{AI reading of one table or one graph}
\usage{
ipm_ai_read(title, x, how = NULL, st = ipm_ai_settings(), language = "English", max_rows = 300)
}
\arguments{
\item{title}{title of the table or graph}

\item{x}{a data frame, a list of data frames, or a ggplot (its data, without plotting positions; with many flag tests, the flagged ones)}

\item{how}{how-to-read note shown with the table or graph}

\item{st}{AI settings from \code{ipm_ai_settings()}}

\item{language}{answer language}

\item{max_rows}{rows sent per table}
}
\value{
list(answer (markdown), usage, model, sent (characters sent))
}
\description{
Sends only the figures behind the table or graph (aggregated, never the records) with its title and how-to-read note,
and returns a short reading: what stands out and what to check.
}
