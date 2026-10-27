IPM - Interviewer Performance Monitoring for MICS (web app)
=============================================================

Stand-alone Shiny app for shinyapps.io or Posit Connect (no package installation needed).

  app.R        loads the libraries and starts the app
  app_core.R   user interface and server logic
  R/           analysis functions (sourced automatically by Shiny)
  deploy.R     deployment script for shinyapps.io

TO DEPLOY
1. Unzip this folder on your computer.
2. Open deploy.R in RStudio, set the working directory to this folder (Session > Set Working Directory > To Source File Location).
3. Paste your shinyapps.io token line (Account > Tokens > Show) in place of setAccountInfo(...) and run the whole script.
4. On shinyapps.io > Applications > mics-ipm > Settings:
   - General: Instance size "xlarge" (2 GB) for complete national datasets; "large" (1 GB) during fieldwork.
   - Advanced: Max worker processes > 1 (one user's run blocks others on the same worker); Connection / Read timeout 3600.
   - Users: make the app private and invite users (paid plans).

TO TEST LOCALLY FIRST
  shiny::runApp(".")

DATA PROTECTION
Free-text/name variables are dropped when files are read. All files of a session (uploads, results, reports) are deleted when the tab is closed, after 30 min of inactivity, or with the "Delete my data now" button; verified on the server file system.
Survey microdata are nonetheless uploaded to a third-party server: check the NSO MoU / data-sharing terms first.

AI ACTION PLAN (optional)
Results > Team scorecard works without AI. Results > Action plan (AI) needs a model:
- shinyapps.io has no protected environment variables: leave the server without a key and paste your own Anthropic (or OpenAI-compatible) key in the app; it is kept in that session's memory only.
- Posit Connect / own server: set ANTHROPIC_API_KEY and/or IPM_AI_OLLAMA_URL (local models) or IPM_AI_OPENAI_URL as environment variables; users choose the connection and the model in the app.
Only aggregated statistics are sent (use 'Preview data sent to the AI' to see them); no record-level data or names.
