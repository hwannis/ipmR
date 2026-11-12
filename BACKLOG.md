# ipmR backlog

Bugs and improvements are logged here and released in batches of about 10 or more, not one release per fix.
Work happens on the `dev` branch; a batch is merged into `main` with a new version number, which publishes the release.

Status: `open`, `done (dev)` = fixed on the dev branch and waiting for the next release, `done (x.y.z)` = released in that version.
Source: PM = Pierre Martel's review (Oct 2026), HW = Hrair, CL = found while testing.

## Data and datasets

| # | Issue | Source | Status |
|---|---|---|---|
| 1 | Datasets page: show the variables inherited from the parent dataset (e.g. "from parent: WM6D/M/Y", "from parent: HH3") instead of leaving the Interviewer/Day boxes looking empty; test the linking on a MICS6 survey (Lesotho 2018). | PM | done (0.5.0) |
| 2 | Data page "What was found" does not update when the interviewer (or other) variable is changed on the Datasets page. | PM | done (0.5.0) |
| 3 | Fieldwork dates are read from the household questionnaire only; also show first/last interview per questionnaire (WM, MN, CH...) on the Data page. | HW | done (0.5.0) |

## Flag settings

| # | Issue | Source | Status |
|---|---|---|---|
| 4 | Keep the initial module of each questionnaire (HH*, WM*, MN*, UF*, FS*): consent, result of interview, MICS Plus phone numbers... are important for IPM. | PM | done (0.5.0) |
| 5 | Choose "variables to ignore" from a list of the existing variables (with labels), not only by typing names. | PM | done (0.5.0) |
| 6 | Check of the flag settings per module (variables selected / ignored, with labels), as in the Stata application, before running. | PM | done (0.5.0) |
| 7 | Better default list of variables to ignore (CAPI-filled, derived, identifiers, non-questions); explain in the app why this choice matters for the flag rates. | PM | done (0.6.0) |

## Results and summary

| # | Issue | Source | Status |
|---|---|---|---|
| 8 | List of highly flagged questions, as in the Stata report, at the top of the summary. | PM | done (0.5.0) |
| 9 | Modules: show questions flagged / questions tested (e.g. 12/150) and include modules with many flagged questions even when the % is low (long modules are diluted). | PM | done (0.5.0) |
| 10 | Teams: "questions to target" instead of modules; caution that an interviewer can be flagged because team-mates are the ones doing poor work (consider "teams to observe/re-interview"). | PM | done (0.5.0) |
| 11 | Figures: sharp when enlarged (vector/SVG, readable numbers below the dots), and all figures viewable on one scrolling page. | PM | done (0.6.0) |
| 12 | Interactive flag plots: hovering a dot shows the flag table (answers of all interviewers of the team). | PM | done (0.6.0) |
| 22 | Descriptive charts (outputs 2-8) cover the whole fieldwork by week, as in the Stata IPM; shade the weeks of the period of analysis on each chart, and offer an option to show only those weeks. | HW | done (0.6.0) |
| 13 | AI-assisted reading of the Data & questionnaires table and other outputs. | PM | open |

## Downloads and performance

| # | Issue | Source | Status |
|---|---|---|---|
| 14 | "Build report and files" is cut by the connection on shinyapps.io (limited account); make the build robust (background task, progress, partial downloads). | PM | done (0.6.0) |
| 23 | The idle timeout counts a long report build or run as inactivity: if the user does not touch the page for 30 min while it runs, the session (and the finished report) is deleted when the build ends. Treat a running task as activity. | CL | done (0.5.0) |
| 24 | Report build: the progress bar stays on "Writing report: flaganal" for many minutes with no detail; show progress inside the flag section (dataset, team, number of tables written). | HW | done (0.5.0) |
| 15 | A long run or AI call blocks every other user of the same server (single R process): run them as background tasks. | CL | done (0.6.0) |

## AI

| # | Issue | Source | Status |
|---|---|---|---|
| 16 | Preview only showed the evidence block, not the full request: now shows the exact request (endpoint, model, instructions, evidence, request, answer format, sizes, Ollama context window) for the plan and for a question, downloadable as JSON. | HW | done (0.5.0) |
| 17 | Switching connection (e.g. Claude to Ollama) can keep the previous connection's model name until the model list loads; reset the model at once and never send a model name from another connection. | CL | done (0.5.0) |
| 18 | Compact evidence mode for local models (High/Medium teams only, fewer readings): about a third of the size. | HW | done (0.6.0) |
| 19 | Number check gives false alarms on ranges: "5-16%" is read as the numbers 5 and -16 ("number(s) -16, -95 not found"). Read ranges as two positive numbers. | CL (gemma4 test) | done (0.5.0) |
| 20 | Plan display shows "hh EU hh EU" when the model puts the module in both the dataset and module fields; normalise before display. | CL (gemma4 test) | done (0.5.0) |
| 21 | Check that every High/Medium team of the scorecard is in the AI plan, and list the ones the model left out. | CL (gemma4 test) | done (0.5.0) |

## From a country IPM review (Oct 2026)

| # | Issue | Source | Status |
|---|---|---|---|
| 25 | Team labels T1-T17 instead of the supervisor codes (HH4) the field teams use; label teams by supervisor code when it is known. | HW | done (0.6.0) |
| 26 | Chi-square on sparse, many-category variables (e.g. BH4D day of birth, 31 values) gives false flags; add a minimum-expected-count rule or collapse rare categories (optional, keeping Stata parity by default). | HW | closed: simulation shows sparse chi-square is conservative and pooling does not reduce flags |
| 27 | Net-level (tn) records are not independent (several nets per household): test at household level or flag the caveat. | HW | done (0.6.0) |
| 28 | Height heaping by measurer (AN7), not only by team: that is what teams act on. | HW | done (0.6.0) |
| 29 | Foundational learning (FL) checks: assessment duration, no attempt, story reached, practice-question exits. | HW | done (dev) |
| 30 | The "x times what chance would produce" headline overstates the effect (sparse cells, non-independent tests); reword and explain. | HW | done (0.6.0) |
| 31 | The app does not show its version: add "ipmR x.y.z" to the header or footer so users can check which release the server runs. | HW | done (0.6.0) |
| 32 | The app does not start when no AI connection is configured on the server ("attempt to set an attribute on NULL"); since 0.4.0. | CL | done (0.6.0) |
| 33 | Flag settings check: "RHS of == is length 0" under the module table when no module is selected (req(length(x)) does not stop on 0; same pattern in 4 other places). | HW | done (0.6.0) |
| 36 | One impossible or missing date on the first record (e.g. 31 Feb) blanked every date of the dataset (outputs empty, no period proposed). | CL (review Nov 2026) | done (dev) |
| 37 | Interviewers were tested on their number of records, not on their answers to the question: flags from 1-4 answers (83 of 3,809 in Somalia) and 22% of tests on interviewers with < 5 answers, diluting the flag rate. Default now counts answers. | CL (review) | done (dev) |
| 38 | The period of analysis covered one day more than asked (21 days = 22; label ended the day after the last interview). | CL (review) | done (dev) |
| 39 | Records up to 6 days before week 1 were counted in week 1 (truncation instead of floor). | CL (review) | done (dev) |
| 40 | With teams defined by a supervisor variable, linked files without that variable (hl, bh, mm, tn) lost their team and dropped out of every output; they now take the team of the parent record. | CL (review) | done (dev) |
| 41 | One empty file or variable (common early in fieldwork) stopped the whole run; now a warning and the dataset is skipped. | CL (review) | done (dev) |
| 42 | Missing, reversed or overlapping team ranges were accepted without error. | CL (review) | done (dev) |
| 35 | Loading only an individual questionnaire (e.g. wm.sav, Madagascar MICS6) without the household file stops with "argument is of length zero" (no household questionnaire was taken as NA); fieldwork dates now come from the questionnaire loaded. | HW | done (dev) |
| 34 | The report and the rule-based plan fail ("object 'rate' not found", "argument is of length zero") when the flag analysis is not part of the run. | CL (FL test on Somalia data) | done (dev) |
