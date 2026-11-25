# ipmR (development version)

**Loading the data (0.9.0.9002)**
* **What was found** now says what each file is, recognised from its content (household questionnaire, household members, women, birth history, maternal mortality ...) and what it is linked to, and lists the usual MICS7 files not loaded with what is lost without them (e.g. "bh.sav (birth history): without it, no children ever born, birth history and under-5 deaths (F10, F11)"); files that are only used in some surveys (tn, fg, mm, mn) are listed apart (#86).
* Only a linked file uploaded (e.g. bh.sav alone): instead of "No MICS questionnaire detected", the app says what the file is and what to add ("bh.sav is the birth history. Add wm.sav (women's questionnaire) and hh.sav (household questionnaire) ..."). A linked file whose questionnaire is missing (bh.sav without wm.sav) is flagged (#86).
* The same questionnaire uploaded twice (hh.sav and "hh (1).sav", hh.sav and an older hh export, hh.sav and hh.dta): one is used - the one with the latest interview, then the most records, then the plain name - and the other is listed as ignored, with why. Before, both were read (#86).
* Dropping more files now adds them to those already loaded (a file with the same name is replaced), so a missing file can be added afterwards; **Clear the data** starts again with a new export. Before, each drop replaced all the files (#86).

**Field check tables (0.9.0.9001)**
* F8Q: the blank test columns count only the households selected for the blank test - E. coli detected, E. coli not detected, households selected for blank testing and % of blank tests with E. coli; the "Test not conducted" column (households not selected) is removed, as in the MICS7 table (#85).

# ipmR 0.9.0

Two new pages and a simpler setup: the MICS7 Field Check Tables produced in the app by team or interviewer, a Data explorer to look at any variable, a setup in three steps with all analysis settings in one panel and remembered per survey, a report built by parts, and the question and module flag summaries to share.

**Field check tables**
* New **Field check tables** page (before the Data explorer): the standard MICS7 Field Check Tables of the central office (SPSS syntaxes 'MICS7 - Table F1...F20' of the MICS7 CAPI application 7.2.1) produced from the data loaded under Setup - response rates (F1, F3W/M/U/C, F3C2, F3Q), eligible members per household (F2W, F2M, F2UC), age distribution and displacement (F4, F5), completeness of dates of birth (F6), key answers (F7), observation of documents, nets, handwashing places and water samples (F8), anthropometry and heaping (F9), children ever born and birth history (F10, F11A-E), maternal mortality (F12), privacy in the mental health and violence modules (F13), food insecurity (F14) and MICS Plus consent (F20). One row per team (or per interviewer), for the whole fieldwork, the period of analysis or chosen dates, and chosen teams (#83).
* **Summary**: a grid of the tables with a target and the teams, red where the target is not met; click a table to open it. **Tables**: each table as in the FCT workbook (with its column groups), the cells where the target is not met in red, and one or two graphs (stacked bars of the result codes, bars per team with the target line, a heatmap of % interviewed by age group, single years of age, births by year); "Read with AI". **Performance overview**: as in the MICS7 Performance syntax and the Overview sheets, each team's interviews split in two halves - mean household size (urban, rural), children ever born, anthropometry out of range, blank tests with E. coli and female and male age ratios - with a graph per indicator (#83).
* **Parameters** tab: the targets of 'targets.sps' (expected eligible women, men, children under 5 and 5-17 per household in urban and rural areas, % of the expected number, age ratios, children ever born in the previous survey) and the thresholds of the table syntaxes (% interviewed by age group, dates of birth, documents seen, heaping, age at death, maternal mortality), the urban and rural codes of HH6 (read from the value labels if not set) and the survey name in the titles. "Apply and remember" keeps them with the survey settings (#83).
* Download of all tables in one Excel workbook: one sheet per table as in 'MICS7 Field Check Tables.xlsx', targets not met in red, with a contents sheet, the targets grid, the overview and the parameters (#83).
* The datasets are recognised by their variables (HH46, HL4/HL6, WM17, MWM17, UF17, FS17, BH4Y, MM15, TN3), only the variables of the tables are read (names never), and the tables whose questions are not in the survey are listed as not produced, with the reason. In R: `ipm_fct_data()`, `ipm_fct()`, `ipm_fct_plot()`, `ipm_fct_xlsx()`, `ipm_fct_defaults()`, `ipm_fct_catalog()` (#83).

**Data explorer**
* New **Data explorer** page: choose a dataset and any variable to see its distribution - frequencies with value labels, % and cumulative % (don't know, missing, refused and "other" marked), or for numbers the summary (mean, SD, percentiles), histogram and **last digit** (heaping on 0 and 5). Break it down by team, interviewer, week of fieldwork or another variable, with a heatmap or box plots, the table and a test of the differences (chi-square or Kruskal-Wallis). Filters: completed interviews only, blanks (question not asked) left out, whole fieldwork / period of analysis / chosen dates, teams, interviewers. Download the tables (Excel) or "Read with AI" (the table only). Only the variables chosen are read from the files, with the interviewer, team and date (from the parent questionnaire for linked files); names and other text variables are never read (#82).
* A break-down variable that is a number with many values (a wealth score, an age) is grouped in quintiles (or quartiles, terciles, deciles), with the range of each group, instead of one group per value - which stopped the page with "missing value where TRUE/FALSE needed" (Madagascar mm.sav, HH6 by WSCORER). "Swap the two variables" shows instead the mean, SD, minimum, percentiles and maximum of the number for each answer. When no test can be computed (one group or one answer), the page says so. The last digit is shown for whole numbers only (#82).
* **Weight** dropdown in the Data explorer, offered when the dataset has sample weights (HHWEIGHT, WMWEIGHT, CHWEIGHT, FSWEIGHT, MNWEIGHT...; anthropometric weights are not offered): % , means, SD, percentiles, the histogram, box plots and quintiles of a break-down score become weighted (population figures); N stays the number of records, records without a weight are left out (and counted), and the tests and the last digit stay on the records. No weight by default, as for checking the interviewers' work each record counts once. `ipm_tabulate(weight = )` (#82).
* "Open in Data explorer" on the Questionnaire page (each question) and under the flag tables of the Flag explorer and Figures (#82).
* In R: `ipm_explore_data()`, `ipm_tabulate()`, `ipm_tab_plot()` (#82).

**Setup in three steps, settings remembered**
* Setup is one page in three steps instead of six: **Data** (drop the data, name the survey; a checklist shows what was found), **Review** (survey and period, outputs, datasets, teams, questions tested - each with a summary and a status OK / Check / Fix; only the parts that need a look are open) and **Run** (a summary of what will run). A bar at the bottom says what still needs a look, with a link to it, and has the Run button; the help of each step is behind "About this page" (#78).
* **Advanced analysis** panel (button at the top of Setup): the flag test parameters, the **issue rules** - until now fixed in the code - and output details, each with its help. Presets: Standard (recommended), Early fieldwork (last 2 weeks, lower thresholds), Strict (fewer, stronger signals) and Stata-compatible. The settings changed from the defaults are listed in the panel (each with a reset), in the run summary and in the Word report (#79).
* Issue rules as settings: weeks for the descriptive issues (`recent_weeks`), questionnaire problem (`rule_sw_share`, `rule_sw_min_teams`), excess thresholds for whole teams, team modules and interviewers (`rule_team_ratio`, `rule_team_high`, `rule_module_ratio`, `rule_interv_ratio`, `rule_interv_high`, `rule_p`) and the team priority (`rule_prio_high`, `rule_prio_medium`); the defaults give the same results as before. The priority rule written in the report, the scorecard and the AI evidence uses the thresholds in use. `ipm_param_spec()`, `ipm_presets()`, `ipm_settings_changed()` (#79).
* Settings remembered per survey: when you run, the settings (variables, teams, modules, outputs, options - no data, no folders, no dates) are kept under the survey name next to the run history. With the next data export, type the same name (it is suggested) and choose **Use them**. `ipm_settings_save()`, `ipm_settings_load()`, `ipm_settings_list()`; turned off with the run history (`IPM_HISTORY=off`) (#80).

**Report built by parts**
* Downloads: choose the sections of the Word report (key findings and scorecard, then each output) and the companion files (flag tables per dataset and team, team action plan, anthropometry case listing, CSV data); only the parts ticked are built, so a smaller report builds faster. The report's cover lists the sections left out. `ipm_report(sections = , files = )`, `ipm_report_parts()` (#81).
* When the report is ready a **Report complete** window opens with the download buttons (also when you were on another page), and Downloads shows "Report complete (built at ..., in ...)"; the download buttons appear only once there is a report to download, and a note says when the ticked parts have changed since it was built (#81).

**Sharing the question and module flags**
* **Questionnaire** page: two downloads to share the question and module flag summaries, as for the briefing - **Flag summary to share (HTML)**: one page that opens offline in any browser with the heat maps of the flagged modules and of the questions with the most flags (as many as chosen on the page) by team, coloured as in the app, with the figures and interviewers flagged on each cell, and the questionnaire problems; **Question and module flags (Excel)**: every flagged question and module with the % flagged in each team (coloured), one row per flagged question and team with the interviewers flagged (with filters), and the questionnaire problems with their action. Codes and figures only, no records. In R: `ipm_question_html()`, `ipm_question_xlsx()` (#84).

**Fixes**
* The app no longer fails with "could not find function progression" when the R workspace has an object called `p` (e.g. a progress counter from progressr restored from .RData): it wrote its paragraphs with `p()`, now `tags$p()` (#77).
* One flag rate for teams and interviewers: the scorecard ("% flagged", "x survey rate", "Flags (expected)"), the scorecard's interviewer table, the rule-based and Word action plans and the AI evidence now leave out the questions flagged in most teams, as the issues already did; before, a team could show two rates (e.g. 5.0% in the scorecard and 4.3% in "whole team has an excess of flags"). The overall survey rate and the flag analysis summary ("Flag rate by interviewer" in the report) still count all questions; the AI evidence also gives the survey rate without questionnaire problems. The figures on all questions are kept in the scorecard as `rate_all`, `ratio_all`, `tests_all`, `flags_all` (#76).

# ipmR 0.8.0

A shareable briefing, AI readings of the graphs and tables, and a safer, clearer AI action plan: before it is sent you see its size, cost and time, choose the teams and get a model recommendation.

**Sharing**
* "Briefing to share (HTML)" on the briefing: one self-contained page to email or put on a shared drive for managers who do not use the app - top numbers, teams to act on with the first action and the follow-up, interviewers to observe, team health grid, questionnaire problems, resolved issues. Opens offline in any browser and prints on A4; codes and figures only, no records. `ipm_briefing_html()` (#69).
* GitHub check uses actions/checkout@v5; the backlog lists #44-#68.

**AI reading of graphs and tables**
* "Read this graph with AI" under each graph in Results > Figures, and "Read these tables with AI" in Data & questionnaires: a short reading of what stands out and what to check. Only the figures behind that graph or table are sent (aggregated counts, rates and test results; for the flag-distribution graphs, the flagged tests only), never the records. `ipm_ai_read()` (#13).

**AI action plan and connections**
* Claude (Anthropic API): AI readings and questions were cut off ("output limit reached") because current Claude models think before answering and the thinking counts against the output limit; Claude requests now allow 16,000 output tokens, and the AI action plan 32,000 (a plan for 12 teams with a careful model such as Opus went past 16,000); Claude may take up to 15 minutes before the app gives up (was 5); the plan asks for one short sentence per issue and action (#70).
* Before the AI action plan is sent, a window shows its size (tokens sent and expected back), the approximate cost at list prices and the time, and lets you choose the teams: High and Medium priority (default), High only (default when more than 10 teams are High or Medium) or all teams; a warning when the request is large. `ipm_ai_estimate()`; `ipm_ai_plan(priorities = )` (#72).
* Model advice under the model box and in that window: claude-sonnet-5-5 recommended (good plans, fast, low cost), claude-opus-5-5 for the most careful plan, claude-haiku-5-5 the cheapest; for local models, compact evidence and 14B+ models (#73).
* Action plan (app and Word): an issue ending with a full stop is no longer followed by ": " (".:") (#74).
* Action plan page and Guide: which plan to use - rule-based for the routine weekly check and the complete list of everything flagged, AI for a readable briefing to supervisors and managers, both to brief people with the full checklist behind it (#75).
* "My own connection": the endpoint is filled in when you choose the provider (Claude: https://api.anthropic.com, Ollama: http://localhost:11434, OpenAI: https://api.openai.com/v1); it can still be changed, and an address you typed is kept when you switch (#71).

# ipmR 0.7.0

A new app built around what to act on. After a run it opens on a **Briefing** (teams to act on this week, interviewers to observe, questionnaire problems, team health grid), compares the run with the earlier runs of the same survey (new, persisting, resolved), has a page per team and per interviewer, a questionnaire heat map, a one-page supervisor brief per team and the follow-up of each issue. All of it reads the same list of issues (one rule engine). On a server, keep the run history in a persistent folder: set `IPM_HISTORY_DIR` to a folder mapped to a volume (otherwise it is lost when the container is rebuilt).

**New app (phase 4a: briefing and run history)**
* New home page after a run, "Briefing": what to act on this week - teams to act on (one line each with the first action), interviewers to observe now, questionnaire problems, a team health grid (each team x 8 areas: flags, interview length, completion, household listing, "don't know", birth history, anthropometry, FL) and the evidence of each issue in one click (#55).
* Run history: each run keeps the list of issues found (codes, figures and headlines - no records) per survey and data export, so the next run shows what is new, what persists ("3rd run in a row, since 27 Nov") and what was resolved, and how the counts changed. `ipm_history_update()`, `ipm_compare()`, `ipm_briefing()`; folder set by `IPM_HISTORY_DIR`, turned off by `IPM_HISTORY=off`; "Forget this survey's history" in the app (#56).
* The 6 setup steps are grouped under one "Setup" menu; after a run the app opens on the briefing (#57).

**Briefing, survey name and dates**
* Briefing: High priority teams are shown in full; Medium priority teams are one line each, under "N Medium priority teams" (click to open). The "Teams to act on" number says how many are high and medium (#58).
* The survey name is asked on the data page as well as on the settings page, and is empty until typed (it names the briefing, the reports and the run history); without a name the survey is called "MICS survey" (#59).
* The period of analysis is written "14 Nov - 04 Dec 2018" everywhere (was "14Nov2018-04Dec2018") (#60).

**New app (phase 4b: team and interviewer pages, supervisor brief)**
* New "Teams" page: one team at a time - priority, every issue with its figures, a bar comparing the team with its benchmark and the action; its interviewers (flags, interview length, completion, main issue); week-by-week charts of interview length, completion and children under 5 listed (team vs the other teams); and the modules where the team stands out (#61).
* New "Interviewers" page: the interviewer's issues, interview length and completion per questionnaire vs the team and the survey, a week-by-week chart and the modules with the most flags (questionnaire problems left out) (#62).
* Supervisor brief: one page per team for the supervisor - what to do this week, the team issues, the interviewers to observe and a trend chart. "Supervisor brief (Word)" on the team page; "Supervisor briefs (Word)" on the briefing gives all High and Medium teams. The full team action plan stays under Results (#63).
* Briefing: click a team or interviewer to open its page; the evidence window shows the same bars and has an "Open the team page" button. `ipm_team_profile()`, `ipm_interviewer_profile()`, `ipm_trend_plot()`, `ipm_supervisor_brief()` (#64).

**New app (phase 4c: follow-up, questionnaire page, AI on each page, polish)**
* Follow-up of each issue: "Follow up" sets it to assigned (to whom), done or escalated, with a note. It is kept with the run history of the survey and shown on the issue at the next runs; an issue set to done that is still found at the next export is shown as "Done on 12 Nov but still found", and listed on the briefing. The briefing shows the follow-up of each team. `ipm_actions()`, `ipm_action_set()`, `ipm_action_status()` (#65).
* New "Questionnaire" page: questions (or modules) x teams, coloured by the share of tests flagged - a row dark in most teams is a questionnaire problem, a row dark in one or two teams is a team problem; point at a cell for the figures and the interviewers flagged, click to open the team. Option to hide the questions flagged in most teams. `ipm_question_heat()` (#66).
* Ask the AI from the team, interviewer and questionnaire pages: the question is asked about that team, interviewer or the questionnaire, with suggested questions; answers stay on the page. Uses the AI connection set under Results > Action plan (AI) (#67).
* Dark mode (switch in the top bar); phone layout (short title, cards two by two, tables scroll); one colour for medium priority everywhere; "Run now with these settings" on the data page, so a routine run is: drop the data, check the name, run (#68).

**One rule engine (phase 3)**
* The summary, the team scorecard, the rule-based action plan, the AI evidence and the report now all read the same list of issues (ipm_scorecard()), so they can no longer contradict each other (#49).
* Every issue is a questionnaire problem (a question or module flagged in most teams), a whole-team issue or an interviewer issue, with a one-line headline, the numbers, a short action, owner and timing (#50).
* Questions flagged in most teams are questionnaire problems: listed once, and left out when teams and interviewers are compared (Somalia: 88 questions; flag rate without them 4.0% instead of 7.1%) (#51).
* New team vs rest-of-survey comparison, for problems the within-team flag test cannot see (the whole team doing the same thing): interview length, completion, share of "don't know" answers, household members listed, FL assessment. Reported only when most of the team's interviewers are on the same side and the difference is large enough to matter (e.g. 5+ minutes, 3+ points of "don't know") (#52).
* One issue per interviewer and area (e.g. the 6 FL checks, or short interviews in several questionnaires); no interviewer issue when the whole team has the same one (#53).
* Team priority: High = team-wide excess of flags or 2+ high-severity issues; Medium = 1 high-severity issue or 3+ issues; Low otherwise. Low is shown in grey, not green (#54).

# ipmR 0.6.2

**Speed (phase 2)** - Somalia (10 questionnaires, 52,808 tests, 2,453 flag tables): full run with report from about 6-7 minutes to about 1 minute; peak memory from 8 GB to under 1 GB. Same numbers as before (all tests, flags, readings, flag tables and the scorecard are identical).
* Flag tests: one grouped pass per block of questions instead of one test at a time, with an exact closed-form chi-square: 60 s to 5 s (#45).
* Graphs no longer keep a hidden copy of the data they were built from (ggplot environments): stored outputs from 1.1 GB to 0.26 GB, which also makes the app's background runs and downloads lighter (#46).
* Flag tables documents and the report's large tables are written as Word XML and put into the saved file in one step, instead of one element at a time: the 27 flag tables files from about 4 minutes to 8 s (#47).
* Graphs of the report are drawn on several processes (forked on Linux, background R sessions on Windows; option `workers`, "auto" = cores - 1, at most 6 and one per 0.6 GB of free memory). A job that fails in a worker is redone in the main process; without the installed package (shinyapps.io bundle) everything runs in one process (#48).

**Other**
* One box to load the data: drop the export folder, the .sav / .dta files or a .zip on it, or click it to choose; only data files are sent (other files in the folder are ignored). Replaces the three separate options; reading a folder in place (local app only) is folded under "Very large files..." (#44).
* Fix: the app and reports showed version 0.6.0 in release 0.6.1; a test now checks that the version shown matches DESCRIPTION (#43).

# ipmR 0.6.1

Work on the dev branch since 0.6.0 (BACKLOG.md items in brackets).

**Foundational learning**
* New output `fl`: checks of the foundational learning assessment of the 5-17 questionnaire, per interviewer and week - FL completed (FL28), median duration from the start with the child (FL2) to the end of the interview, children who do not want to read (no at FL10A/B and not willing to try at FL10C), assessments stopped at the reading practice (FL14/FL15/FL17), reading story reached (FL19), numeracy stopped after number reading (FL23 check: skip to FL27) (#29). Checked against the Somalia 5-17 questionnaire: children 10-14 attending school go straight to the story without the practice (FL12), which is expected.
* Interviewers far from team-mates (or with very short assessments) appear in the summary and the scorecard, with the action "observe the interviewer administering the assessment"; the variables are found automatically for MICS7 and can be changed on the Settings page (Variables for specific outputs).

**Correctness (review of Nov 2026)** - numbers change slightly; the setting `stata_compat` (Flag settings > "Stata-compatible numbers") reproduces the Stata IPM where its behaviour differs
* Interviewers are tested on their answers to each question, not on their number of records (`test_min_nonmissing` now TRUE by default): no more flags from 1-4 answers, and flag rates are no longer diluted by near-empty tests. Somalia 21 Oct: 52,808 tests instead of 68,111, 3,726 flags instead of 3,809, flag rate 7.1% instead of 5.6%, same priority teams (#37).
* The period of analysis covers exactly the number of days asked (#38); records before week 1 are no longer counted in week 1 (#39).
* One impossible date no longer blanks every date of a dataset (#36).
* Linked files keep their team when teams are defined by a supervisor variable (#40).
* An empty file or variable is a warning and the dataset is skipped, instead of stopping the run (#41).
* Missing, reversed or overlapping team ranges are reported as errors (#42).

**Other**
* Fix: the report and the rule-based plan failed when the flag analysis was not part of the run (#34).
* Fix: loading only an individual questionnaire (e.g. wm.sav) without the household file stopped with "argument is of length zero"; the fieldwork dates are then taken from that questionnaire (#35).

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
