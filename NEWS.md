# ipmR 0.6.0

Second batch from the backlog (BACKLOG.md items in brackets).

**Long tasks and several users**
* The analysis, the report build and the AI calls (plan, question, connection test) run in a separate R process: one user's task no longer blocks the other users of the same server, and the app stays usable while it runs (#14, #15). Needs the callr package (in the Docker image); without it the tasks run as before. A task still running is stopped when its session ends.

**Teams**
* Teams defined by interviewer ranges are labelled with the supervisor / team leader code the field teams use, e.g. T3 (208); the Teams page shows the code of each team (#25). The AI plan check and the questions recognise "T3", "team 3" or "208".

**Figures**
* Graphs of flags by module: point at (or tap) a circle to see the flag table of that interviewer and question (#12).
* Each graph can be downloaded as SVG (sharp at any size) or as a high-resolution PNG; all the graphs of an output can be shown on one scrolling page; larger sample-size numbers below the dots (#11).
* Charts of outputs 2-8 keep the whole fieldwork and shade the weeks of the period of analysis; new option to show only those weeks (`period_weeks_only`) (#22).

**Anthropometry**
* Heaping on height by measurer (AN7), whole fieldwork and period of analysis: table in the report, measurers named in the summary, the scorecard evidence and the action (#28).

**Reading the flag rate**
* The summary, the rule-based plan and the AI instructions no longer say "x times what chance would produce": the % flagged is compared with the nominal 0.1%, with an explanation of why it is not a measure of size (#30).
* Files with several records per household (members, nets, births...) are named in the summary with their records per household: their tests are not on independent units and they flag more easily (#27).
* Sparse chi-square tables (#26): checked by simulation - with many answer categories and few cases the test is conservative (fewer false flags than 0.1%), and pooling rare categories does not reduce flags; no change made.

**AI**
* Compact evidence for local models: only High/Medium priority teams, 3 readings per team, fewer interviewers, modules and survey-wide signals (about a third of the size); ticked by default for Ollama / OpenAI-compatible connections (#18).

**Other**
* Fix: the app did not start on a server with no AI connection configured ("attempt to set an attribute on NULL") (#32).
* Fix: "RHS of == is length 0" under the flag settings check when no module was selected; same guard fixed in the team preview, flag explorer and flag tables by team (#33).
* The app header and the Guide show the ipmR version (#31).
* Flag settings: why ignoring CAPI-filled, derived and identifier variables matters for the flag rates (#7).

# ipmR 0.5.0

First batch release from the backlog (BACKLOG.md items in brackets); most items come from Pierre Martel's review.

**Data and datasets**
* Linked datasets (bh, hl, mm, tn...) show what they take from the parent: "Interviewer: taken from wm (WM5). Date of interview: taken from wm (WM6D/WM6M/WM6Y)" (#1).
* The Data page summary follows the settings as they are edited, e.g. a changed interviewer variable (#2).
* First and last interview for each questionnaire, not only the household one (#3).

**Flag settings**
* The initial module of each questionnaire (HH*, WM*, MWM*, UF*, FS*) is now tested: consent, result of interview, language, translator, privacy, phone-survey consent. Its administrative variables are ignored automatically: identifiers, interviewer and supervisor codes, interview dates and times, geography, CAPI selections and rosters, notes, counts and derived variables (#4). Identifier and staff-code variables are also ignored inside other modules (e.g. WQ4), and name variables (#7, part).
* "Variables to ignore" are picked from the list of the dataset's variables (with labels); names and ranges can still be typed (#5).
* New "Check flag settings": every variable of every module with its label and status (tested, ignored, unit + number, text), the modules without variables and the unknown variables to ignore, and a CSV of the full list (`ipm_flag_check()`) (#6).

**Summary**
* List of highly flagged questions, as in the Stata report, right after the overall level (#8).
* Modules: questions flagged / questions tested and the number of highly flagged questions; long modules with several highly flagged questions are listed even when their overall % is low (#9).
* Teams: questions to target (with the number of interviewers flagged on each); section renamed "Teams to observe / re-interview, and questions to target"; caution that an interviewer can be flagged because team-mates record the wrong pattern (#10).

**AI**
* The preview shows exactly what is sent to the model (instructions, evidence, request, answer format, sizes), for the plan and for a question (`ipm_ai_preview()`) (#16).
* A model name from the previous connection is never sent after switching connection (#17).
* Plan checks: ranges such as "5-16%" are no longer read as negative numbers (#19); "hh EU hh EU" normalised (#20); High/Medium teams missing from the plan are listed (#21).

**Long tasks**
* The idle timeout no longer deletes the session while a run, report build or AI call is in progress (#23).
* Report build shows progress inside the flag section (graphs, then flag tables per dataset and per team) (#24).

# ipmR 0.4.5

* Fieldwork dates: the app shows the first and last interview found in the household questionnaire (Data and Settings pages, results, report), so the period of analysis can be set knowingly.
* Settings: buttons to set the period to the last 7, 14 or 21 days or the whole fieldwork, and a line showing the exact dates analysed (and any days left out at the end).
* Clarified that the proposed period is the last 21 days up to the last interview (or the whole fieldwork if shorter).

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
