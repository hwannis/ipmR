# ipmR backlog

Bugs and improvements are logged here and released in batches of about 10 or more, not one release per fix.
Work happens on the `dev` branch; a batch is merged into `main` with a new version number, which publishes the release.

Status: `open`, `done (dev)` = fixed on the dev branch and waiting for the next release.
Source: PM = Pierre Martel's review (Oct 2026), HW = Hrair, CL = found while testing.

## Data and datasets

| # | Issue | Source | Status |
|---|---|---|---|
| 1 | Datasets page: show the variables inherited from the parent dataset (e.g. "from parent: WM6D/M/Y", "from parent: HH3") instead of leaving the Interviewer/Day boxes looking empty; test the linking on a MICS6 survey (Lesotho 2018). | PM | open |
| 2 | Data page "What was found" does not update when the interviewer (or other) variable is changed on the Datasets page. | PM | open |
| 3 | Fieldwork dates are read from the household questionnaire only; also show first/last interview per questionnaire (WM, MN, CH...) on the Data page. | HW | open |

## Flag settings

| # | Issue | Source | Status |
|---|---|---|---|
| 4 | Keep the initial module of each questionnaire (HH*, WM*, MN*, UF*, FS*): consent, result of interview, MICS Plus phone numbers... are important for IPM. | PM | open |
| 5 | Choose "variables to ignore" from a list of the existing variables (with labels), not only by typing names. | PM | open |
| 6 | Check of the flag settings per module (variables selected / ignored, with labels), as in the Stata application, before running. | PM | open |
| 7 | Better default list of variables to ignore (CAPI-filled, derived, identifiers, non-questions); explain in the app why this choice matters for the flag rates. | PM | open |

## Results and summary

| # | Issue | Source | Status |
|---|---|---|---|
| 8 | List of highly flagged questions, as in the Stata report, at the top of the summary. | PM | open |
| 9 | Modules: show questions flagged / questions tested (e.g. 12/150) and include modules with many flagged questions even when the % is low (long modules are diluted). | PM | open |
| 10 | Teams: "questions to target" instead of modules; caution that an interviewer can be flagged because team-mates are the ones doing poor work (consider "teams to observe/re-interview"). | PM | open |
| 11 | Figures: sharp when enlarged (vector/SVG, readable numbers below the dots), and all figures viewable on one scrolling page. | PM | open |
| 12 | Interactive flag plots: hovering a dot shows the flag table (answers of all interviewers of the team). | PM | open |
| 13 | AI-assisted reading of the Data & questionnaires table and other outputs. | PM | open |

## Downloads and performance

| # | Issue | Source | Status |
|---|---|---|---|
| 14 | "Build report and files" is cut by the connection on shinyapps.io (limited account); make the build robust (background task, progress, partial downloads). | PM | open |
| 15 | A long run or AI call blocks every other user of the same server (single R process): run them as background tasks. | CL | open |

## AI

| # | Issue | Source | Status |
|---|---|---|---|
| 16 | Preview only showed the evidence block, not the full request: now shows the exact request (endpoint, model, instructions, evidence, request, answer format, sizes, Ollama context window) for the plan and for a question, downloadable as JSON. | HW | done (dev) |
| 17 | Switching connection (e.g. Claude to Ollama) can keep the previous connection's model name until the model list loads; reset the model at once and never send a model name from another connection. | CL | open |
| 18 | Compact evidence mode for local models (High/Medium teams only, fewer readings): about a third of the size. | HW | open |
| 19 | Number check gives false alarms on ranges: "5-16%" is read as the numbers 5 and -16 ("number(s) -16, -95 not found"). Read ranges as two positive numbers. | CL (gemma4 test) | open |
| 20 | Plan display shows "hh EU hh EU" when the model puts the module in both the dataset and module fields; normalise before display. | CL (gemma4 test) | open |
| 21 | Check that every High/Medium team of the scorecard is in the AI plan, and list the ones the model left out. | CL (gemma4 test) | open |

