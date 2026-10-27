# ipmR 0.4.4

* Fix: uploading datasets whose variable labels (or formats) are stored as several strings failed with "values must be length 1". Read errors now name the file.

# ipmR 0.4.3

* App: asking a question or generating a plan before running the analysis now shows a message instead of doing nothing; progress messages for local models.

# ipmR 0.4.2

* AI with Ollama: "thinking" of reasoning models (gemma4, qwen3, deepseek-r1) switched off by default (`IPM_AI_THINK`), with automatic fallback for models that do not accept the setting. Ollama error messages are shown in full.

# ipmR 0.4.1

* Several AI connections at once (Claude, Ollama, any OpenAI-compatible server); connection and model chosen in the app, with model listing and a connection test (`ipm_ai_connections()`, `ipm_ai_models()`, `ipm_ai_test()`).
* Native Ollama API: context window sized to the evidence, JSON schema enforced.
* Users can add their own connection for their session (`IPM_AI_ALLOW_CUSTOM`).

# ipmR 0.4.0

* Team scorecard (`ipm_scorecard()`): priority per team from flag excess, module hot spots, interviewer signals and the descriptive warning signs of the last weeks, each with evidence and a standard action.
* Rule-based (`ipm_plan_rules()`) and AI (`ipm_ai_plan()`) team action plans, AI questions on the results (`ipm_ai_ask()`), Word export (`ipm_plan_docx()`). Only aggregated statistics are sent; plans are checked against the scorecard.
* Report: team scorecard section, `team_action_plan.docx`, scorecard CSVs. A failing output no longer stops the whole run.

# ipmR 0.3.0

* Web app for shinyapps.io and Docker: folder upload first, explanation boxes on every page, narrative summary and priorities, team x module heatmap, flag explorer with one-line readings, flag tables by team; session data deleted on close, idle timeout or on request.
* Memory: slim loading and in-place preparation.

# ipmR 0.2.0

* `mics_autoconfig()` works on any MICS survey whatever the language of the labels (MICS variable-name structure first, then EN/FR/ES/PT labels, then value labels).

# ipmR 0.1.0

* R port of the Stata IPM application v7.0.0: the nine IPM outputs, the Word report, flag tables, anthropometry Excel listing and tidy CSVs; Shiny app replacing the Stata dialog.
