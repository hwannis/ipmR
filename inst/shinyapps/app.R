# IPM - Interviewer Performance Monitoring for MICS
# Stand-alone Shiny app for shinyapps.io / Posit Connect.
# The analysis functions are in R/ (sourced automatically by Shiny); the user interface is in app_core.R.
library(shiny)
library(bslib)
library(DT)
library(data.table)
library(haven)
library(ggplot2)
library(officer)
library(flextable)
library(openxlsx)
library(yaml)
library(tidyselect)
library(scales)
library(ragg)
library(jsonlite)
library(httr2)        # AI features (Claude API / OpenAI-compatible endpoints)
library(commonmark)   # renders the AI answers
source("app_core.R", local = TRUE)
shinyApp(ipm_ui, ipm_server)
