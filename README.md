# ipmR: Interviewer Performance Monitoring for MICS, in R

This is an R port of the Stata **IPM application v7.0.0** (Pierre Martel, 2024). It also includes a Shiny front-end that replaces the Stata dialog.

You don't need Stata. The package reads the SPSS (`.sav`) or Stata (`.dta`) fieldwork exports and produces the same nine IPM outputs:

| # | Output | Function |
|---|---|---|
| 1 | Questionnaires per team and per day | `out_quesperday()` |
| 2 | Completion rate per interviewer and week | `out_quescrates()` |
| 3 | Median interview duration per interviewer and week | `out_quesmtimes()` |
| 4 | Mean number of household members (8 age/sex groups) | `out_meannohhm()` |
| 5 | Mean number of children ever born (birth history) | `out_everborn()` |
| 6 | % of under-5 deaths in birth histories | `out_u5deaths()` |
| 7 | Height heaping (.0/.5) per team and week | `out_ant_heap()` |
| 8 | Out-of-range / flagged anthropometry, plus an Excel case listing | `out_ant_oor()` |
| 9 | Flag analysis of interviewer bias (each interviewer vs the rest of the team, every question) | `flag_analysis()` |

## Files produced

The run writes these to the results folder:

- `IPM_report.docx`, plus a copy with a time stamp
- `flag_results/flags_dataset_<ds>.docx` and `flags_team_<t>.docx`: one crosstab per flag
- `anthro_report_<date>.xlsx`: flagged anthropometry cases in the period of analysis
- `data/*.csv`: tidy tables (one per output, plus `flag_tests.csv` and `flag_scores.csv`) for Power BI or Fabric

## Install

Latest release from GitHub:

```r
install.packages("remotes")
remotes::install_github("hwannis/ipmR")                 # latest version on main
remotes::install_github("hwannis/ipmR@v0.4.4")          # or a given release
```

The app also needs `install.packages(c("shiny", "bslib", "DT", "ragg", "httr2", "commonmark"))`; the last two are only needed for the AI features.

To install without GitHub access, download `ipmR_<version>.tar.gz` from the [Releases](https://github.com/hwannis/ipmR/releases) page, then:

```r
install.packages("ipmR_0.4.4.tar.gz", repos = NULL, type = "source")
```

Every release also carries `ipm_shinyapp.zip`, the stand-alone bundle for shinyapps.io. To run the app on your own server with Docker (OMV, Proxmox, cloud VM), see [`deploy/README.md`](deploy/README.md). Changes are listed in [`NEWS.md`](NEWS.md).

## Team scorecard and action plans (AI optional)

- **Team scorecard** (`ipm_scorecard()`). One priority level per team, with the evidence behind it:
  - flags compared with the number expected at the survey rate;
  - modules where the team is far above the other teams;
  - interviewers with many flags;
  - short interviews, low completion, omitted members, births or deaths, and height heaping or out-of-range anthropometry in the last 3 weeks.

  Every warning sign comes with a standard action, an owner and a timing.
- **Rule-based action plan** (`ipm_plan_rules()`). Uses no AI.
- **AI action plan and questions** (`ipm_ai_plan()`, `ipm_ai_ask()`):
  - Works with Claude (Anthropic API), local models through Ollama, or any OpenAI-compatible server (vLLM, LM Studio, OpenAI). The connection and the model are chosen in the app.
  - Only aggregated statistics are sent: no record-level data or names. The app has a preview button showing the exact payload.
  - Every plan is checked against the scorecard: unknown teams are removed, and interviewer codes and numbers not found in the data are flagged.

Configure the connections with environment variables (`ANTHROPIC_API_KEY`, `IPM_AI_OLLAMA_URL`, `IPM_AI_OPENAI_URL`, `IPM_AI_PROVIDER`, `IPM_AI_MODEL`); see `?ipm_ai_connections` and [`deploy/README.md`](deploy/README.md).

```r
Sys.setenv(IPM_AI_OLLAMA_URL = "http://localhost:11434", IPM_AI_PROVIDER = "ollama", IPM_AI_MODEL = "qwen2.5:14b")
ipmR::run_ipm_app()
```

## Use

**Shiny app.** This replaces `db ipm_settings`:

```r
ipmR::run_ipm_app("C:/MICS7/<country>/fieldwork_export")
```

The tabs follow the Stata dialog: Main, Datasets, Teams, Outputs, Flags, Run & results. Notes on each:

- **Main.** "Auto-configure" works on any MICS survey (MICS5–7), whatever the questionnaire language and file names. See *Auto-configuration* below.
- **Main.** "Check" replaces the dialog's "Check" buttons.
- **Main.** Settings save to and load from a YAML file, which replaces `ipm_settings.do` and "USE SETTINGS from folder".
- **Run & results.** You can preview figures, the highly flagged questions and the flag scores, then download the report or a zip of all results.

**Script / scheduled job:**

```r
library(ipmR)
cfg <- mics_autoconfig("data", survey_name = "<Country> MICS7")    # or read_config("config.yml")
cfg$period$start <- "2026-09-20"; cfg$period$days <- 21
write_config(cfg, "config.yml")      # edit by hand if needed
res <- run_ipm(cfg)                  # all outputs + report files
```

**Microsoft Fabric.** See `inst/fabric/ipm_fabric_notebook.ipynb`:

- Upload the tarball, the `config.yml` and the `.sav` files to the Lakehouse.
- Run the notebook. The R analysis runs on the driver.
- A PySpark cell publishes the CSVs as Delta tables (`ipm_<country>_<output>`) for a Power BI dashboard.
- The Shiny app is for desktop use, RStudio, or Posit Connect. Fabric does not host Shiny apps.

## Auto-configuration (any MICS survey, any language)

`mics_autoconfig()` builds the settings from the files alone:

1. **Questionnaires.** These are datasets with a day/month/year triplet of the form `XnD XnM XnY` that also has a result variable and an interviewer variable in the same panel. Panels are HH, WM, MWM, UF and FS, or any other prefix.
   - Standard MICS names come first: HH3/HH46, WM5/WM17, MWM5/MWM17, UF5/UF17, FS5/FS17.
   - Next come variable labels in English, French, Spanish or Portuguese, compared with accents removed.
   - Last comes the value labels: a code 1 labelled "Completed/Complete/Rempli/Completa" marks the result variable.
2. **Interview times.** These are the hour/minute pairs (`XnH XnM`) of the panel.
3. **Other datasets** (household members, birth history, maternal mortality, FGM, nets...). Each is linked to its parent questionnaire: the panel whose ID or `*INT` variables it contains, otherwise the household questionnaire. Keys are HH1/HH2(/LN).
4. **Roles** are found by content, not file name:
   - household members: HL1/HL4/HL6;
   - birth history: BH5 with BH9C, or with BH9U/BH9N converted to months;
   - anthropometry: AN11, plus the HAZ/WAZ/WHZ flags if present.
5. **Teams.** Interviewer ranges come from the supervisor or team-leader variable (HH4...).
6. **Flag modules** follow the MICS naming convention. A module that is only a copy of another dataset's module (HL/ED in ch, TN in hh...) is left to its home dataset. Line numbers, IDs and derived `_FIRST/_LAST` variables are ignored.

Tested on two MICS7 surveys: a fieldwork export with English labels, and a full dataset with French labels (about 20,000 households, 25 teams). Anything it gets wrong can be corrected in the app or the YAML file.

## Web app (shinyapps.io / Posit Connect)

The release asset `ipm_shinyapp.zip` (built from `inst/shinyapps/` by `deploy/shinyapps/make_bundle.R`) is a stand-alone version of the app. It does not need the package to be installed: the analysis functions are in `R/`, the interface is in `app_core.R`.

**Deploy:**

1. Create an account at shinyapps.io.
2. Copy your token line from Account > Tokens.
3. Open `deploy.R` from the bundle folder in RStudio, paste the token line and run the script.
4. On the shinyapps.io dashboard, adjust the settings for the app (Applications > mics-ipm > Settings):
   - **Instance size.** Use "large" (1 GB, the default) for fieldwork-size data and "xlarge" (2 GB) for complete national datasets. Peak memory was measured at 0.34 GB for a fieldwork export of 15,500 records and 0.86 GB for a full survey of about 20,000 households, not counting the report.
   - **Worker processes.** One user's run blocks other users of the same worker. Raise the maximum number of worker processes, or allow 1 connection per worker, if several people will use the app at the same time.
   - **Timeouts.** Raise the connection and read timeouts to 3600 seconds. The full Word report takes several minutes to build.
   - **Authentication.** Make the app private and invite users. This needs a paid plan. See the data protection points below.

**The pages:**

1. **Data.** Select the folder with the `.sav` files (or the files, or a `.zip`). Everything is then recognised automatically.
2. **Settings & outputs.** Survey name, period of analysis, outputs to produce, and the variables for the household members, birth history and anthropometry outputs.
3. **Datasets.** Check the variables mapped for each file.
4. **Teams.** Check which interviewers are in which team.
5. **Flag settings.** Modules to test and variables to ignore.
6. **Run.** Runs everything. You can also save or load the settings file.
7. **Results:**
   - **Summary & priorities:** automatic narrative covering overall flag level, most problematic modules and questions, teams to target and on which modules, interviewers to observe with examples, and other warning signs. It also shows the team × module heatmap and the flag rates by team and by interviewer.
   - **Figures:** all graphs, each with a "How to read" note.
   - **Flag explorer:** every flagged test with a one-line reading, for example "Interviewer 102 recorded '1. YES' for 91% of cases (31/34) vs 7% for the rest of team T1 (4/59)". Click a row to see the full crosstab.
   - **Data & questionnaires:** records, dates and interviewers per dataset.
   - **Downloads:** Word report (key findings first, then all outputs with reading notes), flag tables, anthropometry Excel listing, and CSVs.

Every page opens with a box explaining what the page does and what the user needs to do. A **Guide** tab explains the method.

**Data protection (verified on the file system).**
- Each user session works in its own private temporary folder. Nothing is shared between sessions, and there is no page that lists or opens files on the server.
- Name and other free-text variables are dropped as soon as the files are read.
- Shiny's own temporary copies of the uploaded files are deleted as soon as they have been copied into the session folder.
- The temporary files written while building the Word report are deleted when the build finishes.
- Everything of a session (uploaded files, results, report, zip) is deleted from the server:
  - when the browser tab is closed;
  - after 30 minutes without activity (option `ipmR.idle_minutes`);
  - immediately with the **Delete my data now** button.
- Session folders left behind by a crash are removed when the next session starts (after 3 hours), and shinyapps.io instance disks are wiped when an instance is restarted.
- **Test:** the server's temporary storage was checked before upload, after upload, after the run, after the report build and download, after closing the tab, after "Delete my data now", and after the idle timeout. In every end state it returned to the exact pre-upload baseline, with no data, report or result file left.

**Limits of what the app can guarantee:**
- The data still transit through, and are briefly processed on, servers run by Posit (shinyapps.io), over HTTPS.
- Check that the NSO MoU and data-sharing terms allow this before uploading country data.
- Otherwise run the app locally with `ipmR::run_ipm_app()`, or on an internal Posit Connect server.

## Configuration (YAML)

```yaml
survey_name: <Country> MICS7
data_dir: data
results_dir: data/ipm_results
period: {start: '2026-09-20', days: 21}          # anthropometry listing + flag analysis
teams:
  ranges: [102-106, 202-206, 209-212]             # T1, T2, T3 ... (or: variable: HH4)
outputs: {quesperday: yes, quescrates: yes, quesmtimes: yes, meannohhm: yes, everborn: yes,
          u5deaths: yes, ant_heap: yes, ant_oor: yes, flaganal: yes}
extras:
  hhm: {dataset: hl, sex: HL4, age: HL6}
  bh:  {dataset: bh, death: BH5, age_death: BH9C}
  ant: {dataset: ch, height: AN11, flags: [HAZFLAG, WAZFLAG, WHZFLAG]}
datasets:
  hh: {file: hh.sav, interviewer: HH3, result: HH46, day: HH5D, start_hour: HH11H, end_hour: HH13H,
       flag_modules: [HC*, EU*, WS*, HW*], flag_ignore: [], flag_units: []}
  hl: {file: hl.sav, parent: hh, keys: [HH1, HH2], day: HH5D, flag_modules: [HL*, ED*],
       flag_ignore: [HL1-HL2, HL8-HL10]}
  bh: {file: bh.sav, parent: wm, keys: [HH1, HH2, LN], flag_modules: [BH*], flag_units: [BH9U]}
options: {levelsig: 0.001, test_min: 5, max_uniq: 34, highly_flag: 10}   # see ipm_defaults()
```

As in IPM, month and year are the two variables after `day`, and minutes is the variable after each hour variable. You can set them explicitly with `month`, `year`, `start_min` and `end_min`.

## Fidelity to the Stata application

These Stata behaviours are kept so results match:

- **Week 1:** the first day with 20 or more records, set per dataset, and using `int()` truncation.
- **Teams:** assigned from interviewer ranges. If ranges overlap, the last range wins.
- **Test eligibility:** an interviewer needs at least 5 records and the team at least 10. These counts use all the team's records, not only those that answered the question.
- **Variable classification:**
  - more than 34 distinct values → continuous, tested with a pooled t-test;
  - otherwise → categorical, tested with a Pearson chi-square (no continuity correction);
  - 1-character strings → categorical, with missing coded 0;
  - longer strings → not tested.
- **Recodes for continuous variables:** 9..6/7/8/9 become -5/-4/-3/-2, and missing becomes -1.
- **Unit/value merging:** uses the same DK/NR heuristic.
- **Scores:** flag scores by dataset, module and variable, and the "highly flagged" list (10% or more).

**Scale.** A full MICS7 survey (about 20,000 households, 85,000 household members, 125 interviewers):
- loading, outputs and 81,749 flag tests: about 4 minutes;
- peak memory: about 2.6 GB.

**Check against an independent implementation.** On a 10-dataset MICS7 fieldwork export (35,319 tests), the vectorised engine matches a test-by-test implementation using `stats::chisq.test` / `stats::t.test` (`flag_tests_naive()`):

- maximum p-value difference: 9e-14;
- flag agreement: 100%.

**Deviations, each one an option in `ipm_defaults()`:**

- Height heaping uses the configured height variable. Stata hard-coded `AN11`, which was a bug. Heights of 900 or more (not-measured codes such as 999.6) are excluded (`exclude_height_codes`).
- Datasets without their own interviewer or date (hl, bh, mm...) are linked to their parent questionnaire (`parent`, `keys`). Stata simply excluded them. Linked records are kept only when the parent interview is completed (`filter_parent_result`).
- Optional improvements, off by default:
  - `woi_anchor: survey`: one week 1 for all datasets;
  - `test_min_nonmissing: yes`: minimum observations counted on answered cases.
- When the module is `*`, internal variables (doi, woi, interv, team...) are excluded.

**Not yet validated against Stata output.** To validate, run Stata IPM v7 on the same data and compare its `flag_scores.dta` with `data/flag_scores.csv`.
