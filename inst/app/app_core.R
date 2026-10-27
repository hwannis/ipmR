# ipmR Shiny application (shared by the installed package and the shinyapps.io bundle)
# Requires: shiny, bslib, DT, data.table and the ipmR functions to be available.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && (is.na(a) || identical(a, "")))) b else a
.int <- ipm_internals()
output_names <- .int$output_names
hhm_defs <- .int$hhm_defs
cyan <- "#1CABE2"; dark <- "#374EA2"
options(shiny.maxRequestSize = 400 * 1024^2)
LOCAL_MODE <- isTRUE(getOption("ipmR.local", FALSE))

theme <- bs_theme(version = 5, primary = cyan, secondary = dark,
                  base_font = font_collection("Arial", "Helvetica", "sans-serif"),
                  "border-radius" = "0", "btn-border-radius" = "0", "card-border-radius" = "0")

explain <- function(what, todo) div(class = "ipm-explain",
  div(tags$b("What this page does. "), what), div(style = "margin-top:4px", tags$b("What you need to do. "), todo))

var_choices <- function(meta, nm) {
  if (is.null(meta[[nm]])) return(c("(none)" = ""))
  v <- names(meta[[nm]]$varlab); l <- meta[[nm]]$varlab
  c("(none)" = "", stats::setNames(v, ifelse(nzchar(l), paste0(v, " - ", substr(l, 1, 50)), v)))
}
sel <- function(id, label, choices, selected, multiple = FALSE)
  selectizeInput(id, label, choices = choices, selected = selected %||% "", multiple = multiple, width = "100%", options = list(placeholder = "(none)"))
ds_fields <- c(interviewer = "Interviewer no.", result = "Result (1 = completed)", day = "Day of interview", start_hour = "Hour begins", end_hour = "Hour ends")

folder_input <- function(id, label) {
  fi <- fileInput(id, label, multiple = TRUE, buttonLabel = "Select folder...", placeholder = "No folder selected", width = "100%")
  htmltools::tagQuery(fi)$find("input")$filter(function(x, i) identical(x$attribs$type, "file"))$addAttrs(webkitdirectory = NA, directory = NA, mozdirectory = NA)$allTags()
}

howto <- ipm_howto()
AI_CONNS <- tryCatch(ipm_ai_connections(), error = function(e) { message("AI settings: ", conditionMessage(e)); list() })   # server connections (keys stay on the server)
AI_CUSTOM <- ipm_ai_allow_custom()
AI_CHOICES <- c(stats::setNames(names(AI_CONNS), vapply(AI_CONNS, function(x) x$label, "")),
                if (AI_CUSTOM) c("My own connection (this session only)" = "custom"))
if (!length(AI_CHOICES)) AI_CHOICES <- c("No AI model configured on this server" = "")
fmt_i <- function(x) format(x, scientific = FALSE, trim = TRUE)
prio_col <- c(High = "#E2231A", Medium = "#F26A21", Low = "#00833D")
md_html <- function(x) {
  x <- gsub("<", "&lt;", x %||% "", fixed = TRUE)                     # model output is never trusted as HTML
  if (requireNamespace("commonmark", quietly = TRUE)) HTML(commonmark::markdown_html(x, extensions = TRUE)) else tags$pre(style = "white-space:pre-wrap", x)
}

# ---------------- data protection helpers ----------------
IDLE_MINUTES <- getOption("ipmR.idle_minutes", as.numeric(Sys.getenv("IPM_IDLE_MINUTES", "30")))          # session closed (and its data deleted) after this much inactivity
STALE_HOURS  <- 3           # session folders older than this are removed at every new session (crash safety)
in_tempdir <- function(path) startsWith(normalizePath(path, mustWork = FALSE), normalizePath(tempdir(), mustWork = FALSE))
wipe <- function(paths) { td <- normalizePath(tempdir(), mustWork = FALSE)
  for (p in paths) if (length(p) && !is.na(p) && file.exists(p) && in_tempdir(p) && normalizePath(p, mustWork = FALSE) != td) unlink(p, recursive = TRUE, force = TRUE) }
sweep_stale <- function() {
  d <- list.files(tempdir(), pattern = "^ipm_", full.names = TRUE)
  old <- d[difftime(Sys.time(), file.info(d)$mtime, units = "hours") > STALE_HOURS]
  wipe(old)
}
idle_js <- sprintf("
  (function(){ var t; function reset(){ clearTimeout(t); t = setTimeout(function(){ Shiny.setInputValue('idle_timeout', Date.now()); }, %d); }
    ['mousemove','keydown','click','scroll','touchstart'].forEach(function(e){ document.addEventListener(e, reset, true); });
    $(document).on('shiny:connected', reset); })();", as.integer(round(IDLE_MINUTES * 60 * 1000)))


ipm_ui <- page_navbar(
  title = tags$span(tags$b("IPM"), " Interviewer Performance Monitoring for MICS"),
  theme = theme, id = "nav", fillable = FALSE, bg = cyan, inverse = TRUE,
  header = tags$head(tags$script(HTML(idle_js)), tags$style(HTML("
    .navbar .navbar-brand, .navbar .nav-link {color:white !important}
    .navbar .nav-link.active {border-bottom:3px solid white}
    .ipm-explain {border-left:5px solid #1CABE2; background:#F2FAFD; padding:10px 14px; margin:6px 0 14px 0; font-size:0.93em}
    .howto {border-left:4px solid #374EA2; background:#F5F6FB; padding:8px 12px; margin:6px 0 10px 0; font-size:0.9em}
    .dsrow {border-bottom:1px solid #ddd; padding:6px 0}
    .dsname {font-weight:bold; font-size:1.1em; color:#1CABE2; padding-top:28px}
    .small-note {font-size:0.85em; color:#555}
    .bignum {font-size:1.6em; font-weight:bold; color:#1CABE2}
    .narr h5 {color:#1CABE2; margin-top:16px; font-weight:bold}
    .narr li {margin-bottom:6px}
    .pattern {font-weight:bold; color:#374EA2; margin:8px 0}
    pre.log {max-height:220px; overflow:auto; font-size:0.8em; background:#f6f6f6}
    .selectize-input {min-height:31px; padding:4px 8px}
    .selectize-input > .item {white-space:nowrap; overflow:hidden; text-overflow:ellipsis; max-width:92%}
    .selectize-control.multi .selectize-input > .item {max-width:100%; font-size:0.85em}
    .prio {display:inline-block; color:white; font-weight:bold; padding:1px 8px; font-size:0.8em; letter-spacing:0.04em}
    .teamplan {border-left:6px solid #1CABE2; padding:6px 14px 10px 14px; margin:14px 0; background:white; border-bottom:1px solid #e3e3e3}
    .teamplan h5 {margin:4px 0 6px 0; font-weight:bold}
    .teamplan table {font-size:0.88em; margin:6px 0}
    .teamplan th {background:#1CABE2; color:white; padding:3px 10px}
    .teamplan td {padding:3px 10px; border-bottom:1px solid #eee; vertical-align:top}
    .aiwarn {border-left:5px solid #F26A21; background:#FFF6EE; padding:8px 12px; margin:8px 0; font-size:0.88em}
    .aimeta {font-size:0.82em; color:#555; margin-bottom:8px}
    .chatq {font-weight:bold; color:#374EA2; margin-top:12px}
    .chata {border-left:3px solid #1CABE2; padding:4px 12px; margin:4px 0 10px 0; font-size:0.93em}
    h5.sch {color:#1CABE2; font-weight:bold; margin-top:22px}
    pre.json {max-height:60vh; overflow:auto; font-size:0.75em; background:#f6f6f6}"))),

  # ---------------- 1. DATA ----------------
  nav_panel("1. Data", value = "data",
    explain("Loads the survey data files exported from CSPro/SPSS (hh, hl, wm, bh, ch, fs, mn ... in .sav or .dta format) and automatically recognises the MICS structure: which file is which questionnaire, the interviewer, result, date and time variables, the links between files, the teams and the modules to test. It works for any MICS survey (MICS5-MICS7) whatever the language of the labels.",
            "Click 'Select folder...' and choose the folder that contains the .sav files of your latest data export (or select the files themselves, or a .zip of the folder). Wait for the green confirmation, check the table, then go to the next step."),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Load the survey datasets"),
        folder_input("up_folder", "Option A - the folder with the data files"),
        fileInput("up_files", "Option B - the data files (.sav / .dta) or a .zip", multiple = TRUE, accept = c(".sav", ".dta", ".zip"), width = "100%"),
        if (LOCAL_MODE) textInput("data_dir", "Option C - path of a folder on this computer", value = getOption("ipmR.data_dir", ""), width = "100%"),
        if (LOCAL_MODE) actionButton("scan", "Read folder", class = "btn-primary"),
        div(class = "howto", tags$b("Your data. "), sprintf("Files are processed only within your own session and are never shared with other users. Name and other free-text variables are removed when the files are read. All uploaded files, results and reports are deleted from the server when you close the browser tab, after %s minutes of inactivity, or immediately with the button below. Do not upload data you are not authorised to process on this server.", format(IDLE_MINUTES))),
        actionButton("wipe", "Delete my data now", class = "btn-outline-danger")),
      card(card_header("What was found"), uiOutput("found_ui"), tableOutput("found_tbl"))),
    div(style = "text-align:right", actionButton("go2", HTML("Next: survey settings &rarr;"), class = "btn-primary"))),

  # ---------------- 2. SETTINGS & OUTPUTS ----------------
  nav_panel("2. Settings & outputs", value = "settings",
    explain("Sets the survey name, the period of analysis and the outputs to produce, and identifies the variables needed for the household members, birth history and anthropometry outputs.",
            "Check the survey name. The period of analysis (21 days recommended, normally the most recent 3 weeks) is used for the flag analysis and the anthropometry case listing; all other outputs use the whole fieldwork. Untick outputs you do not need, and check the variables in the boxes on the right (they were filled in automatically)."),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Survey and period"),
        textInput("survey_name", "Name of the survey", width = "100%"),
        fluidRow(column(6, dateInput("period_start", "Period of analysis: first day", value = Sys.Date() - 21)),
                 column(6, numericInput("period_days", "Extent (days)", value = 21, min = 1))),
        checkboxGroupInput("outputs", "Outputs", choices = stats::setNames(names(output_names), output_names), width = "100%"),
        checkboxGroupInput("hhm_groups", "Household member groups (output 4)", inline = TRUE,
                           choices = stats::setNames(names(hhm_defs), vapply(hhm_defs, `[[`, "", "label")))),
      card(card_header("Variables for specific outputs"), uiOutput("extras_ui"))),
    div(style = "text-align:right", actionButton("go3", HTML("Next: datasets &rarr;"), class = "btn-primary"))),

  # ---------------- 3. DATASETS ----------------
  nav_panel("3. Datasets", value = "datasets",
    explain("Shows, for each data file, the variables used to identify the interviewer, the result of the interview (1 = completed), the date and the start/end time of the interview. Files that are only part of a questionnaire (household members, birth history, maternal mortality, bednets...) have no interviewer or date of their own: they are linked to their parent questionnaire by key variables (cluster, household, line number).",
            "Normally nothing: everything was filled in automatically. Correct a variable only if it is wrong, untick 'include' for files you do not want to analyse, then click 'Check settings'. Month and year are taken from the two variables after the day, and minutes from the variable after each hour (MICS convention)."),
    actionButton("check", "Check settings against the data"), tableOutput("issues"),
    uiOutput("ds_ui"),
    div(style = "text-align:right", actionButton("go4", HTML("Next: teams &rarr;"), class = "btn-primary"))),

  # ---------------- 4. TEAMS ----------------
  nav_panel("4. Teams", value = "teams",
    explain("Assigns each interviewer to a team. All comparisons are made within teams (an interviewer is compared with team-mates working in the same clusters), so correct teams are essential. Teams were derived automatically from the supervisor / team leader number.",
            "Check in the right-hand table that each team contains the right interviewers and that no interviewer is 'NOT ASSIGNED'. If needed, edit the ranges (one line per team, e.g. 102-106) or use the team-leader variable instead, then click 'Refresh'."),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Team definition"),
        radioButtons("team_method", NULL, c("Interviewer number ranges (as in IPM)" = "ranges", "Team = value of a variable (e.g. supervisor number)" = "variable")),
        conditionalPanel("input.team_method == 'ranges'",
          textAreaInput("team_ranges", "One range per team (T1, T2, ...)", rows = 12, width = "100%"),
          fluidRow(column(4, textInput("pat_first", "Team 1 range", "10-19")), column(4, numericInput("pat_n", "No. of teams", 20, 1, 90)), column(4, numericInput("pat_step", "Step", 10, 1))),
          actionButton("pat_apply", "Apply pattern of team 1 to all teams")),
        conditionalPanel("input.team_method == 'variable'", uiOutput("team_var_ui"))),
      card(card_header("Interviewers by team (from the data)"), actionButton("team_preview", "Refresh"), tableOutput("team_tbl"))),
    div(style = "text-align:right", actionButton("go5", HTML("Next: flag analysis settings &rarr;"), class = "btn-primary"))),

  # ---------------- 5. FLAGS ----------------
  nav_panel("5. Flag settings", value = "flags",
    explain("Defines which questions enter the flag analysis. Modules follow the MICS naming (HC* = all variables starting with HC). Identification variables (line numbers, IDs) are ignored. 'Unit' variables (e.g. PN3U days/weeks) are combined with the number that follows (PN3N) before testing.",
            "Normally nothing. Remove modules you do not want to test, or add variables to ignore (e.g. HL1-HL2 HL8-HL10). The advanced parameters reproduce the original IPM defaults; change them only if you know why."),
    uiOutput("flag_ui"),
    card(card_header("Advanced parameters"),
      layout_columns(col_widths = c(2, 2, 2, 2, 2, 2),
        numericInput("o_levelsig", "Significance level", 0.001, 0, 1, 0.0005),
        numericInput("o_test_min", "Min. obs per interviewer", 5, 1),
        numericInput("o_max_uniq", "Max. distinct values (categorical)", 34, 2),
        numericInput("o_highly_flag", "Highly flagged (%)", 10, 1),
        selectInput("o_woi_anchor", "Week 1 anchor", c("Per dataset (IPM)" = "dataset", "Survey-wide (hh)" = "survey")),
        checkboxInput("o_test_min_nonmissing", "Min. obs counted on answered cases", FALSE))),
    div(style = "text-align:right", actionButton("go6", HTML("Next: run &rarr;"), class = "btn-primary"))),

  # ---------------- 6. RUN ----------------
  nav_panel("6. Run", value = "run",
    explain("Runs all selected outputs. Computation takes from a few seconds (early fieldwork) to 2-3 minutes (complete national survey).",
            "Click RUN and wait for the progress bar to finish; you are then taken to the results. You can also save your settings (YAML) to reuse them next time with 'Load settings'."),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Run the analysis"),
        actionButton("run", "RUN", class = "btn-primary btn-lg", width = "100%"),
        verbatimTextOutput("log", placeholder = TRUE) |> tagAppendAttributes(class = "log")),
      card(card_header("Settings file"),
        p(class = "small-note", "Save the settings to reuse them with the next data export of the same survey (load them after uploading the data)."),
        downloadButton("cfg_download", "Save settings (YAML)"),
        fileInput("cfg_upload", "Load settings (YAML)", accept = c(".yml", ".yaml"), width = "100%")))),

  # ---------------- 7. RESULTS ----------------
  nav_panel("7. Results", value = "results",
    navset_tab(id = "restabs",
      nav_panel("Summary & priorities", value = "summary",
        explain("Automatic reading of all results: overall level of interviewer effects, most problematic modules and questions, teams and interviewers that may need refresher training, supervision or observation, and other warning signs (short interviews, low completion, omitted household members or deaths, height heaping).",
                "Use it as the agenda for the debrief with the survey coordinators and supervisors; confirm each point with the flag tables (tab 'Flag explorer') before acting."),
        uiOutput("kpis"), uiOutput("narrative"),
        plotOutput("team_module", height = "560px"), div(class = "howto", tags$b("How to read: "), howto$team_module),
        layout_columns(col_widths = c(5, 7),
          card(card_header("Flag rate by team"), DT::DTOutput("teams_dt")),
          card(card_header("Flag rate by interviewer"), DT::DTOutput("interv_dt")))),
      nav_panel("Team scorecard", value = "scorecard",
        explain("One line per team with a priority level computed from all warning signs: excess of flags compared with the survey rate, modules where the team is far above the other teams, interviewers with many flags, very short interviews, low completion, omitted household members, births or deaths, height heaping and out-of-range anthropometry (last 3 weeks for the descriptive signs). Every sign is listed below with its evidence and the standard action.",
                "Start with the High-priority teams. Click a team in the first table to filter the list of warning signs. The same evidence is used by the action plan (next tab)."),
        uiOutput("sc_kpis"),
        tags$h5(class = "sch", "Teams (highest priority first)"), DT::DTOutput("sc_teams"),
        div(class = "howto", tags$b("How to read: "), "Points = 3 per high-severity sign and 1 per medium sign. High priority = 9 points or more, or a team-wide excess of flags (at least 1.5 times the survey rate, binomial p < 0.01); Medium = 3 to 8 points. 'x survey rate' compares the team's % of flagged tests with the survey as a whole (1 = average)."),
        tags$h5(class = "sch", "Warning signs with evidence and standard action"), DT::DTOutput("sc_signals"),
        tags$h5(class = "sch", "Interviewers"), DT::DTOutput("sc_interv")),
      nav_panel("Action plan (AI)", value = "ai",
        explain(tagList("Writes a team-by-team action plan (issues with evidence, actions with owner and timing, interviewers to observe, modules to retrain) and answers questions about the results. ",
                        tags$b("Only aggregated statistics are sent to the AI model"), " - the team scorecard, flag rates per module and question, one-line readings of flagged questions, interviewer codes. No record-level data, household identifiers or names leave the server; click 'Preview data sent' to see exactly what is sent. Without AI, the rule-based plan gives the same structure from the standard action library."),
                "Click 'Generate AI action plan' (30 seconds to 2 minutes) or 'Rule-based plan'. Check the plan against the flag tables before sharing it, especially any line marked 'check'. Download it in Word for the supervisors' debrief."),
        layout_columns(col_widths = c(4, 8),
          card(card_header("AI model and action plan"),
            selectInput("ai_conn", "Connection", AI_CHOICES, width = "100%", selectize = FALSE),
            conditionalPanel("input.ai_conn == 'custom'",
              radioButtons("u_prov", NULL, c("Claude (Anthropic API)" = "anthropic", "Local model (Ollama)" = "ollama",
                                             "OpenAI-compatible (vLLM, LM Studio, OpenAI...)" = "openai")),
              textInput("u_base", "Endpoint URL", width = "100%", placeholder = "Ollama: http://192.168.1.10:11434  |  vLLM: http://host:8000/v1"),
              passwordInput("u_key", "API key (not needed for Ollama)", width = "100%"),
              actionButton("u_set", "Connect", width = "100%")),
            selectizeInput("ai_model", "Model", choices = NULL, width = "100%",
                           options = list(create = TRUE, placeholder = "List the models or type a name")),
            layout_columns(col_widths = c(6, 6), actionButton("ai_models", "List models", width = "100%"), actionButton("ai_test", "Test", width = "100%")),
            uiOutput("ai_status"),
            tags$hr(style = "margin:4px 0"),
            selectInput("ai_lang", "Language of the plan", c("English", "French", "Spanish", "Portuguese", "Arabic"), width = "100%"),
            actionButton("ai_go", "Generate AI action plan", class = "btn-primary", width = "100%"),
            actionButton("rules_go", "Rule-based plan (no AI)", width = "100%"),
            actionButton("ai_preview", "Preview data sent to the AI", width = "100%"),
            downloadButton("plan_dl", "Download action plan (Word)", style = "width:100%")),
          card(card_header("Ask a question about the results"),
            uiOutput("chat"),
            textAreaInput("ai_q", NULL, width = "100%", rows = 2,
                          placeholder = "e.g. Why is team T4 flagged so much? Which interviewers should the supervisor of T1 observe first? Is the IC module a translation problem?"),
            actionButton("ai_ask", "Ask", class = "btn-primary"))),
        uiOutput("plan_ui")),
      nav_panel("Figures", value = "figures",
        explain("All graphs of the IPM report, one at a time.", "Choose a graph in the list; read the 'How to read' note below it."),
        uiOutput("fig_select"), uiOutput("fig_howto"), plotOutput("fig", height = "650px")),
      nav_panel("Flag explorer", value = "explorer",
        explain("Lists every flagged test (one interviewer x one question) with a one-line reading of the difference with team-mates. Click a row to see the full table of answers for the team.",
                "Filter by team, interviewer or module using the boxes above the columns. Typical uses: prepare a supervision visit (filter a team), check a suspicious interviewer, or check whether a question is flagged in many teams (question problem)."),
        DT::DTOutput("flags_dt"), uiOutput("flag_detail")),
      nav_panel("Flag tables by team", value = "teamtables",
        explain("Shows the flag tables of one team, as in the Word files 'flags_team_X' of the report: one table per flagged question with the answers recorded by every interviewer of the team. Columns marked * are the flagged interviewer(s), with a one-line reading for each. When two or more interviewers of the same team are flagged on the same question, their answers differ from each other - often a sign that the question is asked or understood differently within the team.",
                "Choose a team (and optionally a dataset) to prepare a supervision visit or a debrief with that team; download the Word version to share with the supervisor."),
        fluidRow(column(3, uiOutput("tt_team_ui")), column(3, uiOutput("tt_ds_ui")),
                 column(4, div(style = "padding-top:32px", downloadButton("tt_dl", "Download this team's flag tables (Word)")))),
        uiOutput("tt_tables")),
      nav_panel("Data & questionnaires", value = "dsum",
        explain("Records, dates and interviewers per dataset, and completed questionnaires per team and day.", "Check that all datasets were analysed and that the dates and number of interviewers look right."),
        tableOutput("dsum"), tableOutput("qpd")),
      nav_panel("Downloads", value = "downloads",
        explain("Builds the full IPM report (Word) with all graphs and the key findings, the flag tables per dataset and per team (Word), the anthropometry case listing (Excel) and tidy CSV files for dashboards.",
                "Click 'Build report' (1 to 10 minutes depending on the size of the survey), then download the report or the zip with everything."),
        actionButton("build", "Build report and files", class = "btn-primary"),
        br(), br(),
        downloadButton("dl_report", "Download IPM_report.docx"), downloadButton("dl_zip", "Download all results (.zip)")))),

  nav_panel("Guide", value = "guide",
    card(card_header("About the Interviewer Performance Monitoring (IPM)"),
      p("The IPM complements the MICS field check tables. While fieldwork is going on, it compares the work of each interviewer with that of his/her team-mates, who work in the same clusters at the same time and should therefore record similar answers. Large differences point to interviewers who may need support (or to questions that are not well understood) while there is still time to fix them."),
      tags$ol(
        tags$li(tags$b("Data: "), "upload the latest export of the fieldwork data (.sav)."),
        tags$li(tags$b("Settings, datasets, teams, flags: "), "checked automatically - review them the first time, then save the settings."),
        tags$li(tags$b("Run: "), "computes everything."),
        tags$li(tags$b("Results: "), "start with 'Summary & priorities' and 'Team scorecard' (priority of each team with the evidence), then 'Action plan (AI)' for a team-by-team plan and questions; check with the figures and the flag explorer; download the Word report and action plan to share.")),
      p("Recommended rhythm: every 1-2 weeks from the 2nd-3rd week of fieldwork, with the last 2-3 weeks as period of analysis; share a limited number of priorities with the field teams each time."),
      tags$h5("How to read each output"),
      tags$dl(lapply(names(output_names), function(id) tagList(tags$dt(output_names[[id]]), tags$dd(howto[[id]] %||% ""))))),
    p(class = "small-note", "ipmR - R port of the Stata IPM application v7.0.0 developed by Pierre Martel for UNICEF MICS."))
)

ipm_server <- function(input, output, session) {
  sweep_stale()
  message("session ", substr(session$token, 1, 6), " started; tempdir ", tempdir())
  sdir <- file.path(tempdir(), paste0("ipm_", session$token)); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  upload_dirs <- character()                       # Shiny's own temporary copies of uploaded files
  rv <- reactiveValues(cfg = NULL, light = NULL, res = NULL, log = character(), report = NULL, zip = NULL, n_files = 0,
                       sc = NULL, plan = NULL, chat = list(), ai_user = NULL, ai_test = NULL)
  cleanup <- function() {
    wipe(c(sdir, upload_dirs))
    isolate({ rv$cfg <- NULL; rv$light <- NULL; rv$res <- NULL; rv$report <- NULL; rv$zip <- NULL
              rv$sc <- NULL; rv$plan <- NULL; rv$chat <- list(); rv$ai_user <- NULL; rv$ai_test <- NULL })
    invisible(gc(verbose = FALSE))
  }
  session$onSessionEnded(function() cleanup())
  observeEvent(input$idle_timeout, {
    cleanup()
    showModal(modalDialog(title = "Session closed", sprintf("No activity for %s minutes: your data and results have been deleted from the server. Reload the page to start again.", format(IDLE_MINUTES)), footer = NULL))
    session$close()
  })
  observeEvent(input$wipe, {
    cleanup()
    showModal(modalDialog(title = "Data deleted", "All uploaded files, results and reports of this session have been deleted from the server. The page will now reload.", footer = NULL))
    Sys.sleep(2); session$reload()
  })
  logm <- function(...) rv$log <- c(rv$log, paste(format(Sys.time(), "%H:%M:%S"), ...))
  if (!is.null(pre <- getOption("ipmR.preload"))) isolate({   # testing only: preload a saved result
    rv$res <- readRDS(pre); rv$sc <- tryCatch(ipm_scorecard(rv$res), error = function(e) NULL) })

  nav_to <- function(v) bslib::nav_select("nav", v)
  observeEvent(input$go2, nav_to("settings")); observeEvent(input$go3, nav_to("datasets"))
  observeEvent(input$go4, nav_to("teams")); observeEvent(input$go5, nav_to("flags")); observeEvent(input$go6, nav_to("run"))

  # ---------- data loading ----------
  ingest <- function(paths, names) {
    on.exit(wipe(unique(dirname(paths))), add = TRUE)   # remove Shiny's temporary upload copies immediately
    upload_dirs <<- unique(c(upload_dirs, dirname(paths)))
    dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
    ddir <- file.path(sdir, "data"); unlink(ddir, recursive = TRUE); dir.create(ddir)
    for (i in seq_along(paths)) {
      if (grepl("\\.zip$", names[i], ignore.case = TRUE)) {
        ex <- file.path(sdir, "unz"); unlink(ex, recursive = TRUE); utils::unzip(paths[i], exdir = ex)
        fs <- list.files(ex, pattern = "\\.(sav|dta)$", ignore.case = TRUE, recursive = TRUE, full.names = TRUE)
        file.copy(fs, file.path(ddir, basename(fs)), overwrite = TRUE); unlink(ex, recursive = TRUE)
      } else if (grepl("\\.(sav|dta)$", names[i], ignore.case = TRUE)) file.copy(paths[i], file.path(ddir, names[i]), overwrite = TRUE)
    }
    configure(ddir)
  }
  configure <- function(ddir) {
    n <- length(list.files(ddir, pattern = "\\.(sav|dta)$", ignore.case = TRUE))
    if (!n) { showNotification("No .sav or .dta file found in what was uploaded.", type = "error"); return() }
    withProgress(message = "Reading and recognising the datasets...", value = 0.3, {
      cfg <- tryCatch(mics_autoconfig(ddir), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
      req(cfg)
      cfg$results_dir <- file.path(sdir, "results"); cfg$options$drop_long_strings <- TRUE; cfg$options$fig_dpi <- 120
      cfg$survey_name <- "MICS survey"
      apply_cfg(cfg, keep_paths = TRUE)
    })
    rv$n_files <- n
    showNotification(sprintf("%d datasets read: %d questionnaires recognised, %d teams.", n,
                             sum(vapply(rv$cfg$datasets, function(d) !is.null(d$result), logical(1))), length(rv$cfg$teams$ranges)), type = "message", duration = 8)
  }
  observeEvent(input$up_folder, ingest(input$up_folder$datapath, input$up_folder$name))
  observeEvent(input$up_files, ingest(input$up_files$datapath, input$up_files$name))
  observeEvent(input$scan, { req(dir.exists(input$data_dir)); configure(input$data_dir) })

  apply_cfg <- function(cfg, keep_paths = FALSE) {
    cfg <- .int$normalise(cfg)
    if (!keep_paths && !is.null(rv$cfg)) { cfg$data_dir <- rv$cfg$data_dir; cfg$results_dir <- rv$cfg$results_dir }
    cfg$options$drop_long_strings <- TRUE
    rv$cfg <- cfg
    rv$light <- tryCatch(ipm_load(cfg, quiet = TRUE, n_max = 400), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    updateTextInput(session, "survey_name", value = cfg$survey_name)
    if (!is.null(cfg$period$start)) updateDateInput(session, "period_start", value = as.Date(cfg$period$start))
    updateNumericInput(session, "period_days", value = cfg$period$days %||% 21)
    updateRadioButtons(session, "team_method", selected = if (!is.null(cfg$teams$variable)) "variable" else "ranges")
    updateTextAreaInput(session, "team_ranges", value = paste(cfg$teams$ranges, collapse = "\n"))
    updateCheckboxGroupInput(session, "outputs", selected = names(cfg$outputs)[unlist(cfg$outputs)])
    updateCheckboxGroupInput(session, "hhm_groups", selected = cfg$hhm_groups)
    o <- cfg$options
    updateNumericInput(session, "o_levelsig", value = o$levelsig); updateNumericInput(session, "o_test_min", value = o$test_min)
    updateNumericInput(session, "o_max_uniq", value = o$max_uniq); updateNumericInput(session, "o_highly_flag", value = o$highly_flag)
    updateSelectInput(session, "o_woi_anchor", selected = o$woi_anchor); updateCheckboxInput(session, "o_test_min_nonmissing", value = o$test_min_nonmissing)
  }
  observeEvent(input$cfg_upload, {
    req(rv$cfg); cfg <- read_config(input$cfg_upload$datapath)
    keep <- intersect(names(cfg$datasets), names(rv$cfg$datasets))
    if (!length(keep)) { showNotification("The settings file does not match the uploaded datasets.", type = "error"); return() }
    cfg$datasets <- cfg$datasets[keep]; apply_cfg(cfg)
    showNotification("Settings loaded.", type = "message")
  })

  output$found_ui <- renderUI({
    cfg <- rv$cfg
    if (is.null(cfg)) return(p(class = "small-note", "No data loaded yet."))
    q <- names(cfg$datasets)[vapply(cfg$datasets, function(d) !is.null(d$result), logical(1))]
    div(div(class = "bignum", sprintf("%d files, %d questionnaires, %d teams", length(cfg$datasets), length(q), length(cfg$teams$ranges %||% character()))),
        p(sprintf("Period of analysis proposed: %s (+%s days). Outputs available: %d of 9.", cfg$period$start %||% "-", cfg$period$days, sum(unlist(cfg$outputs)))))
  })
  output$found_tbl <- renderTable({
    cfg <- rv$cfg; req(cfg)
    rbindlist(lapply(names(cfg$datasets), function(n) { d <- cfg$datasets[[n]]
      data.table(file = d$file, type = if (!is.null(d$result)) "questionnaire" else if (!is.null(d$parent)) paste("linked to", d$parent) else "not used",
                 interviewer = d$interviewer %||% "(from parent)", result = d$result %||% "", date = d$day %||% "(from parent)",
                 `modules to test` = paste(head(d$flag_modules, 8), collapse = " "))
    }))
  })

  # ---------- datasets ----------
  output$ds_ui <- renderUI({
    cfg <- rv$cfg; req(cfg, rv$light); meta <- rv$light$meta
    tagList(lapply(names(cfg$datasets), function(nm) {
      d <- cfg$datasets[[nm]]; ch <- var_choices(meta, nm)
      div(class = "dsrow", fluidRow(
        column(1, div(class = "dsname", nm), checkboxInput(paste0("inc_", nm), "include", TRUE)),
        lapply(names(ds_fields), function(f) column(if (f %in% c("start_hour", "end_hour")) 1 else 2, sel(paste0("ds_", nm, "_", f), ds_fields[[f]], ch, d[[f]]))),
        column(1, selectInput(paste0("ds_", nm, "_parent"), "Parent", c("(none)" = "", setdiff(names(cfg$datasets), nm)), d$parent %||% "")),
        column(1, sel(paste0("ds_", nm, "_keys"), "Link keys", ch[-1], d$keys, multiple = TRUE))))
    }))
  })
  outputOptions(output, "ds_ui", suspendWhenHidden = FALSE)

  output$team_var_ui <- renderUI({
    req(rv$light); v <- Reduce(intersect, lapply(rv$light$meta, function(m) names(m$varlab)))
    selectInput("team_var", "Variable present in all datasets", v, selected = rv$cfg$teams$variable %||% (if ("HH4" %in% v) "HH4" else v[1]))
  })
  outputOptions(output, "team_var_ui", suspendWhenHidden = FALSE)
  observeEvent(input$pat_apply, {
    ab <- as.numeric(strsplit(gsub("\\s", "", input$pat_first), "-")[[1]]); req(length(ab) == 2)
    r <- vapply(seq_len(input$pat_n) - 1, function(k) sprintf("%g-%g", ab[1] + k * input$pat_step, ab[2] + k * input$pat_step), "")
    updateTextAreaInput(session, "team_ranges", value = paste(r, collapse = "\n"))
  })

  output$extras_ui <- renderUI({
    cfg <- rv$cfg; req(cfg, rv$light); meta <- rv$light$meta; e <- cfg$extras %||% list(); dsn <- c("(none)" = "", names(cfg$datasets))
    mk <- function(key, title, fields) {
      ds <- e[[key]]$dataset %||% ""
      div(style = "border-bottom:1px solid #ddd; margin-bottom:8px", tags$b(title),
        selectInput(paste0("ex_", key, "_dataset"), "Dataset", dsn, ds),
        lapply(names(fields), function(f) sel(paste0("ex_", key, "_", f), fields[[f]], var_choices(meta, ds), e[[key]][[f]], multiple = f == "flags")))
    }
    tagList(
      mk("hhm", "Household members list (sex 1=M 2=F; age in years)", list(sex = "Sex", age = "Age")),
      mk("bh", "Birth history (death coded 2; age at death in months)", list(death = "Living status", age_death = "Age at death (months, or number if a unit is given)", age_death_unit = "Age at death unit (optional)")),
      mk("ant", "Anthropometry (height in cm; flags coded 1)", list(height = "Height / length", flags = "Out-of-range / flag variables")))
  })
  outputOptions(output, "extras_ui", suspendWhenHidden = FALSE)
  for (k in c("hhm", "bh", "ant")) local({
    key <- k
    observeEvent(input[[paste0("ex_", key, "_dataset")]], {
      ds <- input[[paste0("ex_", key, "_dataset")]]; req(rv$light)
      flds <- switch(key, hhm = c("sex", "age"), bh = c("death", "age_death", "age_death_unit"), ant = c("height", "flags"))
      for (f in flds) updateSelectizeInput(session, paste0("ex_", key, "_", f), choices = var_choices(rv$light$meta, ds),
                                           selected = if (identical(ds, rv$cfg$extras[[key]]$dataset)) rv$cfg$extras[[key]][[f]] else "")
    }, ignoreInit = TRUE)
  })

  output$flag_ui <- renderUI({
    cfg <- rv$cfg; req(cfg, rv$light); meta <- rv$light$meta
    tagList(lapply(names(cfg$datasets), function(nm) {
      d <- cfg$datasets[[nm]]
      div(class = "dsrow", fluidRow(
        column(1, div(class = "dsname", nm)),
        column(5, textInput(paste0("fm_", nm), "Modules to test", paste(d$flag_modules, collapse = " "), width = "100%")),
        column(4, textInput(paste0("fi_", nm), "Variables to ignore", paste(d$flag_ignore, collapse = " "), width = "100%")),
        column(2, sel(paste0("fu_", nm), "Unit variables", var_choices(meta, nm)[-1], d$flag_units, multiple = TRUE))))
    }))
  })
  outputOptions(output, "flag_ui", suspendWhenHidden = FALSE)

  # ---------- collect configuration from inputs ----------
  collect_cfg <- function() {
    cfg <- rv$cfg; req(cfg)
    g <- function(id, default = NULL) { v <- input[[id]]; if (is.null(v)) default else v }
    nz <- function(v) if (is.null(v) || !length(v) || identical(v, "")) NULL else v
    split_ws <- function(s) { s <- unlist(strsplit(trimws(s %||% ""), "[[:space:]]+")); s[nzchar(s)] }
    cfg$survey_name <- g("survey_name", cfg$survey_name)
    cfg$period <- list(start = if (length(input$period_start) && !is.na(input$period_start)) format(input$period_start) else cfg$period$start, days = g("period_days", 21))
    ds <- list()
    for (nm in names(cfg$datasets)) {
      if (isFALSE(input[[paste0("inc_", nm)]])) next
      d <- cfg$datasets[[nm]]
      for (f in c(names(ds_fields), "parent")) d[[f]] <- nz(g(paste0("ds_", nm, "_", f), d[[f]]))
      d$keys <- g(paste0("ds_", nm, "_keys"), d$keys) %||% character()
      d$flag_modules <- split_ws(g(paste0("fm_", nm), paste(d$flag_modules, collapse = " ")))
      d$flag_ignore <- split_ws(g(paste0("fi_", nm), paste(d$flag_ignore, collapse = " ")))
      d$flag_units <- g(paste0("fu_", nm), d$flag_units) %||% character()
      ds[[nm]] <- d
    }
    cfg$datasets <- ds
    if (identical(input$team_method, "variable")) cfg$teams <- list(variable = g("team_var", "HH4"))
    else { r <- trimws(unlist(strsplit(g("team_ranges", paste(cfg$teams$ranges, collapse = "\n")), "\n"))); cfg$teams <- list(ranges = r[nzchar(r)]) }
    sel_out <- g("outputs", names(cfg$outputs)[unlist(cfg$outputs)])
    cfg$outputs <- as.list(stats::setNames(names(output_names) %in% sel_out, names(output_names)))
    cfg$hhm_groups <- g("hhm_groups", cfg$hhm_groups)
    for (k in c("hhm", "bh", "ant")) {
      ds_k <- nz(g(paste0("ex_", k, "_dataset"), cfg$extras[[k]]$dataset))
      flds <- switch(k, hhm = c("sex", "age"), bh = c("death", "age_death", "age_death_unit"), ant = c("height", "flags"))
      if (is.null(ds_k)) { cfg$extras[[k]] <- NULL; next }
      e <- list(dataset = ds_k); for (f in flds) e[[f]] <- nz(g(paste0("ex_", k, "_", f), cfg$extras[[k]][[f]]))
      cfg$extras[[k]] <- e
    }
    for (k in c("levelsig", "test_min", "max_uniq", "highly_flag", "woi_anchor", "test_min_nonmissing")) cfg$options[[k]] <- g(paste0("o_", k), cfg$options[[k]])
    cfg
  }
  output$cfg_download <- downloadHandler(filename = function() "ipm_settings.yml", content = function(f) {
    cfg <- collect_cfg(); cfg$data_dir <- NULL; cfg$results_dir <- NULL; write_config(cfg, f) })

  observeEvent(input$check, {
    cfg <- collect_cfg(); iss <- validate_config(cfg, rv$light)
    output$issues <- renderTable(if (nrow(iss)) iss else data.frame(result = "No problems found. You can continue."))
  })

  output$team_tbl <- renderTable({
    input$team_preview; cfg <- isolate(collect_cfg())
    rows <- list()
    for (n in names(cfg$datasets)) { d <- cfg$datasets[[n]]; if (is.null(d$interviewer)) next
      cols <- c(d$interviewer, cfg$teams$variable)
      x <- tryCatch(.int$read_cols(file.path(cfg$data_dir, d$file), cols), error = function(e) NULL); if (is.null(x) || !d$interviewer %in% names(x)) next
      iv <- as.numeric(x[[d$interviewer]]); sup <- if (!is.null(cfg$teams$variable) && cfg$teams$variable %in% names(x)) as.numeric(x[[cfg$teams$variable]]) else NULL
      rows[[n]] <- data.table(interv = iv, sup = if (is.null(sup)) NA_real_ else sup) }
    req(length(rows)); a <- unique(rbindlist(rows))[!is.na(interv)]
    lv <- if (!is.null(cfg$teams$variable)) sort(unique(a$sup)) else NULL
    a[, team := .int$assign_teams(interv, if (is.null(lv)) NULL else sup, cfg, lv)]
    a <- unique(a[, .(interv, team)])
    a[order(team, interv), .(interviewers = paste(sort(unique(interv)), collapse = ", "), n = uniqueN(interv)),
      by = .(team = ifelse(is.na(team), "NOT ASSIGNED (excluded)", paste0("T", team)))]
  })

  # ---------- run ----------
  observeEvent(input$run, {
    req(rv$cfg); cfg <- collect_cfg(); rv$log <- character(); rv$report <- NULL; rv$zip <- NULL
    rv$res <- NULL; gc(verbose = FALSE)
    withProgress(message = "Running the IPM", value = 0, {
      prog <- function(m) { logm(m); incProgress(1 / 25, detail = m) }
      res <- tryCatch(run_ipm(cfg, write = FALSE, progress = prog, keep_data = FALSE),
                      error = function(e) { logm("ERROR:", conditionMessage(e)); showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
      if (!is.null(res)) { incProgress(0, detail = "Writing summary"); res$summary <- tryCatch(ipm_summary(res), error = function(e) NULL) }
      rv$sc <- if (!is.null(res)) tryCatch(ipm_scorecard(res), error = function(e) { logm("Scorecard:", conditionMessage(e)); NULL })
      rv$plan <- NULL; rv$chat <- list()
      rv$res <- res
    })
    if (!is.null(rv$res)) { logm("Done."); nav_to("results") }
  })
  output$log <- renderText(paste(rev(rv$log), collapse = "\n"))

  # ---------- results: summary ----------
  output$kpis <- renderUI({
    res <- rv$res; req(res); fa <- res$outputs$flaganal
    k <- function(v, l) div(style = "display:inline-block; margin-right:40px", div(class = "bignum", v), div(class = "small-note", l))
    div(k(format(sum(res$data_summary$records), big.mark = ","), "records analysed"),
        k(sum(!is.na(unique(res$team_info$team))), "teams defined"),
        if (!is.null(fa$tests)) k(format(nrow(fa$tests), big.mark = ","), "flag tests"),
        if (!is.null(fa$tests)) k(sprintf("%.1f%%", mean(fa$tests$flag) * 100), "of tests flagged (0.1% expected by chance)"),
        if (!is.null(fa$period)) k(fa$period, "period of analysis"))
  })
  output$narrative <- renderUI({
    s <- rv$res$summary; req(s)
    div(class = "narr", lapply(s$sections, function(sec) tagList(tags$h5(sec$title), tags$ul(lapply(sec$bullets, tags$li)))))
  })
  output$team_module <- renderPlot({ s <- rv$res$summary; req(s$plots$team_module); print(s$plots$team_module) }, res = 96)
  output$teams_dt <- DT::renderDT({ s <- rv$res$summary; req(s$tables$teams)
    DT::datatable(s$tables$teams[, .(team, `% flagged` = round(rate, 1), tests, flags)], rownames = FALSE, options = list(pageLength = 15, dom = "tp")) })
  output$interv_dt <- DT::renderDT({ s <- rv$res$summary; req(s$tables$interviewers)
    DT::datatable(s$tables$interviewers[, .(team, interviewer = interv, `% flagged` = round(rate, 1), tests, flags)], rownames = FALSE, filter = "top", options = list(pageLength = 15)) })


  # ---------- results: team scorecard ----------
  output$sc_kpis <- renderUI({ sc <- rv$sc; req(sc)
    k <- function(v, l, col = cyan) div(style = "display:inline-block; margin-right:40px", div(class = "bignum", style = paste0("color:", col), v), div(class = "small-note", l))
    div(k(sum(sc$teams$priority == "High"), "teams of high priority", prio_col[["High"]]), k(sum(sc$teams$priority == "Medium"), "teams of medium priority", prio_col[["Medium"]]),
        k(nrow(sc$signals[level == "survey"]), "survey-wide question/module problems"), k(nrow(sc$signals[level == "interviewer"]), "interviewer warning signs"))
  })
  sc_team_tab <- reactive({ sc <- rv$sc; req(sc)
    sc$teams[, .(Team = team_label, Priority = priority, Points = points, Signs = signals, `% flagged` = round(rate, 1), `x survey rate` = round(ratio, 1),
                 `Flags (expected)` = ifelse(is.na(flags), "", sprintf("%d (%.0f)", flags, expected)), `Main modules` = top_modules, `Interviewers of concern` = interviewers_of_concern)] })
  output$sc_teams <- DT::renderDT({
    DT::datatable(sc_team_tab(), rownames = FALSE, selection = "single", fillContainer = FALSE, options = list(pageLength = 30, dom = "tp", scrollX = TRUE)) |>
      DT::formatStyle("Priority", color = "white", fontWeight = "bold", backgroundColor = DT::styleEqual(names(prio_col), unname(prio_col)))
  })
  output$sc_signals <- DT::renderDT({ sc <- rv$sc; req(sc)
    s <- sc$signals; i <- input$sc_teams_rows_selected
    if (length(i)) s <- s[team_label %in% sc_team_tab()$Team[i]]
    DT::datatable(s[, .(Level = level, Team = team_label, Interviewer = ifelse(is.na(interv), "", fmt_i(interv)), Sign = gsub("_", " ", type), Severity = severity,
                        Module = ifelse(is.na(module), "", paste(dataset, module)), Evidence = evidence, Action = action, Owner = owner, When = timing)],
                  rownames = FALSE, filter = "top", fillContainer = FALSE, options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatStyle("Severity", color = DT::styleEqual(c("high", "medium"), c(prio_col[["High"]], prio_col[["Medium"]])), fontWeight = "bold")
  })
  output$sc_interv <- DT::renderDT({ sc <- rv$sc; req(sc$interviewers)
    DT::datatable(sc$interviewers[, .(Team = team_label, Interviewer = fmt_i(interv), Tests = tests, Flags = flags, `% flagged` = round(rate, 1),
                                      `x survey rate` = round(ratio, 1), `Team % flagged` = round(team_rate, 1), Signs = signals, `Sign types` = signal_types)],
                  rownames = FALSE, filter = "top", fillContainer = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  # ---------- results: AI action plan and questions ----------
  conn <- reactive({ id <- input$ai_conn %||% ""
    if (identical(id, "custom")) rv$ai_user else AI_CONNS[[id]] })
  ai_st <- reactive({
    c0 <- conn(); if (is.null(c0)) return(ipm_ai_settings("none"))
    m <- input$ai_model %||% ""
    ipm_ai_settings(c0$provider, if (nzchar(m)) m else c0$model, c0$api_key, c0$base_url, c0$label)
  })
  list_models <- function(quiet = FALSE) {
    c0 <- conn(); if (is.null(c0)) return(invisible())
    ms <- tryCatch(ipm_ai_models(c0), error = function(e) { if (!quiet) showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
    if (length(ms)) {
      cur <- isolate(input$ai_model) %||% c0$model
      sel <- if (nzchar(cur %||% "") && cur %in% ms) cur else if (nzchar(c0$model) && c0$model %in% ms) c0$model else ms[1]
      updateSelectizeInput(session, "ai_model", choices = unique(c(ms, cur[nzchar(cur %||% "")])), selected = sel)
      if (!quiet) showNotification(sprintf("%d model(s) available.", length(ms)), type = "message")
    } else if (!quiet && !is.null(ms)) showNotification("The endpoint returned no model. On Ollama: ollama pull <model>.", type = "warning")
  }
  observeEvent(conn(), {
    c0 <- conn(); rv$ai_test <- NULL
    updateSelectizeInput(session, "ai_model", choices = if (nzchar(c0$model)) c0$model else character(), selected = c0$model)
    if (c0$provider %in% c("ollama", "openai")) list_models(quiet = TRUE)        # local endpoints: list the installed models straight away
  })
  observeEvent(input$ai_models, list_models())
  observeEvent(input$u_set, {
    base <- trimws(input$u_base %||% "")
    rv$ai_user <- tryCatch(ipm_ai_settings(input$u_prov, model = NULL, api_key = input$u_key %||% "", base_url = if (nzchar(base)) base else NULL,
                                           label = "My own connection"), error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
    updateTextInput(session, "u_key", value = "")
  })
  observeEvent(input$ai_test, {
    st <- ai_st()
    if (!isTRUE(st$ready)) { rv$ai_test <- list(ok = FALSE, message = "Not ready: choose a connection and a model (and a key for Claude)."); return() }
    withProgress(message = "Testing the model", detail = if (st$provider == "ollama") "the first call loads the model: up to a minute" else NULL, value = 0.5,
                 rv$ai_test <- ipm_ai_test(st))
  })
  output$ai_status <- renderUI({
    st <- ai_st(); tr <- rv$ai_test
    tagList(
      div(class = "small-note", style = "margin:4px 0",
        if (isTRUE(st$ready)) tagList(tags$b("Ready: "), sprintf("%s, model %s", st$label, st$model),
                                      if (st$provider %in% c("ollama", "openai")) sprintf(" at %s", st$base_url),
                                      if (identical(input$ai_conn, "custom")) " - your settings are kept in this session's memory only.")
        else if (identical(st$provider, "none")) "No AI connection: the rule-based plan is available."
        else "Choose a model (click 'List models' or type its name)."),
      if (!is.null(tr)) div(class = if (isTRUE(tr$ok)) "small-note" else "aiwarn", style = if (isTRUE(tr$ok)) "color:#00833D" else NULL,
                            if (isTRUE(tr$ok)) paste("Test OK:", tr$message) else paste("Test failed:", tr$message)),
      if (isTRUE(st$ready) && st$provider %in% c("ollama", "openai"))
        div(class = "small-note", "Local models: use 14B parameters or more for usable plans; smaller models give generic or wrong plans (the automatic checks below each plan will show it)."))
  })
  observeEvent(input$ai_preview, {
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (tab 6. Run): the AI works from its results.", type = "warning", duration = 8); return() }
    ev <- ipm_ai_evidence(rv$res, rv$sc)
    n <- nchar(jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
    j <- jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = TRUE)
    showModal(modalDialog(title = "Data sent to the AI model", size = "l", easyClose = TRUE,
      p(class = "small-note", sprintf("%s characters (about %s tokens). Aggregated statistics only: no record-level data, no household identifiers, no names. Plus a fixed instruction text describing the task.",
                                      format(n, big.mark = ","), format(n %/% 4, big.mark = ","))),
      tags$pre(class = "json", j), footer = modalButton("Close")))
  })
  observeEvent(input$ai_go, {
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (tab 6. Run): the AI works from its results.", type = "warning", duration = 8); return() }
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model ready: choose a connection and a model, or use the rule-based plan.", type = "warning"); return() }
    withProgress(message = sprintf("%s is writing the action plan", st$model), detail = if (st$provider == "ollama") "local models can take several minutes" else "30 seconds to 2 minutes", value = 0.3, {
      p <- tryCatch(ipm_ai_plan(rv$res, rv$sc, st, language = input$ai_lang), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
    })
    if (!is.null(p)) { rv$plan <- p; r <- rv$res; r$action_plan <- p; rv$res <- r }
  })
  observeEvent(input$rules_go, { if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (tab 6. Run): the AI works from its results.", type = "warning", duration = 8); return() }; rv$plan <- ipm_plan_rules(rv$sc); r <- rv$res; r$action_plan <- NULL; rv$res <- r })
  output$plan_ui <- renderUI({
    p <- rv$plan; req(p)
    li <- function(x) tags$ul(lapply(x, tags$li))
    team_block <- function(t) {
      col <- prio_col[[t$priority %||% "Low"]] %||% cyan
      div(class = "teamplan", style = paste0("border-left-color:", col),
        tags$h5(sprintf("Team %s ", t$team), span(class = "prio", style = paste0("background:", col), toupper(t$priority))),
        div(t$headline),
        if (length(t$issues)) li(lapply(t$issues, function(x) tagList(tags$b(x$issue), ": ", x$evidence))),
        if (length(t$actions)) tags$table(tags$tr(tags$th("Action"), tags$th("Owner"), tags$th("When")),
                                          lapply(t$actions, function(a) tags$tr(tags$td(a$action), tags$td(a$owner), tags$td(a$timing)))),
        if (length(t$interviewers_to_observe)) div(tags$b("Interviewers to observe: "),
          li(lapply(t$interviewers_to_observe, function(x) tagList(tags$b(x$interviewer), ": ", x$reason, if (isTRUE(x$unverified)) tags$span(style = "color:#E2231A", " [not found in the data - check]"))))),
        if (length(t$modules_to_retrain)) div(tags$b("Modules to retrain: "), paste(unlist(t$modules_to_retrain), collapse = ", ")),
        if (length(t$unverified)) div(class = "aiwarn", sprintf("Check before use: number(s) %s not found in the scorecard.", paste(t$unverified, collapse = ", "))))
    }
    tagList(
      div(class = "aimeta", sprintf("%s - generated %s%s%s", if (identical(p$source, "rules")) "Rule-based plan (IPM action library)" else paste("AI plan, model", p$model),
                                    p$generated %||% "", if (!is.null(p$seconds)) sprintf(" in %s s", p$seconds) else "",
                                    if (!is.null(p$usage$input)) sprintf("; %s input / %s output tokens", format(p$usage$input, big.mark = ","), format(p$usage$output, big.mark = ",")) else "")),
      if (length(p$validation)) div(class = "aiwarn", tags$b("Automatic checks against the data: "), li(p$validation)),
      tags$h5(style = "color:#1CABE2; font-weight:bold", "Overall assessment"), p(p$overall_assessment),
      if (length(p$survey_wide_issues)) tagList(tags$h5(style = "color:#1CABE2; font-weight:bold", "Survey-wide issues (question, translation, training)"),
        div(class = "teamplan", tags$table(tags$tr(tags$th("Issue"), tags$th("Evidence"), tags$th("Action"), tags$th("Owner")),
          lapply(p$survey_wide_issues, function(x) tags$tr(tags$td(x$issue), tags$td(x$evidence), tags$td(x$action), tags$td(x$owner)))))),
      if (length(p$most_problematic_modules)) tagList(tags$h5(style = "color:#1CABE2; font-weight:bold", "Most problematic modules"),
        li(lapply(p$most_problematic_modules, function(x) tagList(tags$b(paste(x$dataset, x$module)), ": ", x$why, " ", tags$i(x$action))))),
      tags$h5(style = "color:#1CABE2; font-weight:bold", "Teams"),
      lapply(p$teams, team_block),
      if (length(p$caveats)) tagList(tags$h5(style = "color:#1CABE2; font-weight:bold", "Caveats"), li(p$caveats)))
  })
  output$plan_dl <- downloadHandler(filename = function() sprintf("team_action_plan_%s.docx", format(Sys.Date(), "%Y%m%d")),
    content = function(f) { req(rv$sc); p <- rv$plan %||% ipm_plan_rules(rv$sc)
      ipm_plan_docx(p, f, rv$sc, rv$res$cfg$survey_name, rv$sc$survey$period) })
  observeEvent(input$ai_ask, {
    q <- trimws(input$ai_q %||% ""); if (!nzchar(q)) return()
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (tab 6. Run): the AI works from its results.", type = "warning", duration = 8); return() }
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model configured.", type = "warning"); return() }
    withProgress(message = sprintf("%s is answering", st$model), detail = if (st$provider == "ollama") "local models re-read the whole evidence: 1-5 minutes" else NULL, value = 0.4, {
      a <- tryCatch(ipm_ai_ask(q, rv$res, rv$sc, st, history = rv$chat, language = input$ai_lang), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
    })
    if (!is.null(a)) { rv$chat <- c(rv$chat, list(list(q = q, a = a$answer, team = a$team))); updateTextAreaInput(session, "ai_q", value = "") }
  })
  output$chat <- renderUI({
    ch <- rv$chat; if (!length(ch)) return(p(class = "small-note", "Answers use the same aggregated evidence as the plan (run the analysis first); when you name a team (e.g. 'T4' or 'team 4'), its flag tables (answer counts per interviewer) are added. Local models take 1-5 minutes per answer: watch the progress bar at the bottom right."))
    tagList(lapply(rev(ch), function(x) tagList(div(class = "chatq", x$q, if (!is.na(x$team %||% NA)) span(class = "small-note", sprintf(" (with the flag tables of %s)", x$team))), div(class = "chata", md_html(x$a)))))
  })

  # ---------- results: figures ----------
  figs <- reactive({
    res <- rv$res; req(res); out <- list()
    for (id in names(res$outputs)) {
      r <- res$outputs[[id]]
      if (id == "flaganal") { for (i in seq_along(r$plots)) { z <- r$plots[[i]]
          nm <- sprintf("Flags | %s", switch(z$type, summary = "summary by dataset and module", dataset = paste(z$dataset, "- by interviewer"), module = paste(z$dataset, "- module", sub("\\*$", "", z$module), i)))
          out[[nm]] <- list(plot = z$plot, how = howto[[paste0("flag_", z$type)]]) } }
      else for (s in r$sections) for (j in seq_along(s$plots))
        out[[sprintf("%s | %s%s%s", output_names[[id]], s$dataset %||% "", if (!is.null(s$group)) paste0(" - ", hhm_defs[[s$group]]$label) else "", if (length(s$plots) > 1) paste0(" (page ", j, ")") else "")]] <- list(plot = s$plots[[j]], how = howto[[id]])
    }
    out
  })
  output$fig_select <- renderUI(selectInput("fig_pick", NULL, names(figs()), width = "100%"))
  output$fig_howto <- renderUI({ req(input$fig_pick); f <- figs()[[input$fig_pick]]; req(f); div(class = "howto", tags$b("How to read: "), f$how) })
  output$fig <- renderPlot({ req(input$fig_pick); f <- figs()[[input$fig_pick]]; req(f); print(f$plot) }, res = 96)

  # ---------- results: flag explorer ----------
  flagged <- reactive({
    fa <- rv$res$outputs$flaganal; req(fa$tests)
    t <- fa$tests[flag == 1]
    t <- merge(t, rv$res$team_info, by = "team", all.x = TRUE)
    t[order(team, interv, dataset, variable), .(team = team_label, interviewer = interv, dataset, module = sub("\\*$", "", module), question = varname,
                                               description = vardesc, test, p = signif(p, 2), reading = pattern)]
  })
  output$flags_dt <- DT::renderDT(DT::datatable(flagged(), rownames = FALSE, filter = "top", selection = "single",
                                                options = list(pageLength = 15, scrollX = TRUE, columnDefs = list(list(width = "35%", targets = 8)))))
  output$flag_detail <- renderUI({
    i <- input$flags_dt_rows_selected; req(i); f <- flagged()[i]
    tb <- Filter(function(z) z$dataset == f$dataset && z$variable == f$question && z$interv == f$interviewer, rv$res$outputs$flaganal$tables)
    req(length(tb)); z <- tb[[1]]
    card(card_header(z$title), p(class = "small-note", z$desc), div(class = "pattern", z$pattern), renderTable(z$table, digits = 0))
  })

  # ---------- results: flag tables by team ----------
  df_html <- function(df) {
    tags$table(class = "table table-sm table-striped", style = "width:auto !important; min-width:420px; font-size:0.88em",
      tags$thead(tags$tr(lapply(seq_along(df), function(j) { n <- names(df)[j]; tags$th(n, style = paste0(if (endsWith(n, "*")) "background:#374EA2;" else "background:#1CABE2;", " color:white; padding:2px 14px;", if (j > 1) " text-align:right" else "")) }))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j) tags$td(as.character(df[i, j]), style = if (j > 1) "text-align:right; padding:2px 14px" else "padding:2px 14px")))))) }
  fgroups <- reactive({ t <- rv$res$outputs$flaganal$tables; req(length(t)); group_flag_tables(t) })
  output$tt_team_ui <- renderUI({
    g <- fgroups(); tm <- vapply(g, `[[`, 0, "team"); ti <- rv$res$team_info
    n <- tapply(vapply(g, function(x) length(x$interviewers), 0), tm, sum)
    lab <- ti$team_label[match(as.integer(names(n)), ti$team)]
    selectInput("tt_team", "Team", stats::setNames(names(n), sprintf("%s (%d flags)", lab, as.integer(n))), selectize = FALSE)
  })
  output$tt_ds_ui <- renderUI({ g <- fgroups(); selectInput("tt_ds", "Dataset", c("All datasets" = "", unique(vapply(g, `[[`, "", "dataset"))), selectize = FALSE) })
  tt_sel <- reactive({ req(input$tt_team); g <- fgroups()
    g[vapply(g, function(x) x$team == as.integer(input$tt_team) && (is.null(input$tt_ds) || !nzchar(input$tt_ds) || x$dataset == input$tt_ds), logical(1))] })
  output$tt_tables <- renderUI({
    g <- tt_sel(); req(length(g)); shown <- head(g, 80)
    tagList(
      p(class = "small-note", sprintf("%d flagged questions%s.", length(g), if (length(g) > 80) " - first 80 shown; download the Word file for all" else "")),
      lapply(shown, function(x) card(card_header(x$title),
        p(class = "small-note", x$desc),
        lapply(seq_along(x$readings), function(k) div(class = "pattern", sprintf("p=%s: %s", signif(x$p[k], 2), x$readings[k]))),
        if (length(x$interviewers) > 1) p(class = "small-note", "Several interviewers of this team are flagged on this question: their answers differ from each other (question possibly asked or understood differently within the team)."),
        df_html(x$table))))
  })
  output$tt_dl <- downloadHandler(
    filename = function() { ti <- rv$res$team_info; sprintf("flags_team_%s.docx", ti$team_label[match(as.integer(input$tt_team), ti$team)]) },
    content = function(f) { g <- tt_sel(); ti <- rv$res$team_info
      .int$flag_doc(g, f, sprintf("Flag tables: team %s", ti$team_label[match(as.integer(input$tt_team), ti$team)]),
                    rv$res$outputs$flaganal$period, rv$res$cfg$survey_name) })

  # ---------- results: data summary ----------
  output$dsum <- renderTable({ req(rv$res); rv$res$data_summary })
  output$qpd <- renderTable({ r <- rv$res$outputs$quesperday; req(r, length(r$tables)); r$tables[[1]] }, na = "")

  # ---------- downloads ----------
  observeEvent(input$build, {
    res <- rv$res; req(res)
    before <- list.files(tempdir(), full.names = TRUE)
    on.exit({ new <- setdiff(list.files(tempdir(), full.names = TRUE), before); wipe(new[!grepl("^ipm_", basename(new))]) }, add = TRUE)
    unlink(res$cfg$results_dir, recursive = TRUE)
    withProgress(message = "Building the report", value = 0, {
      prog <- function(m) incProgress(1 / 15, detail = m)
      rv$report <- tryCatch(ipm_report(res, res$cfg$results_dir, progress = prog), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = NULL); NULL })
      if (!is.null(rv$report)) {
        z <- file.path(sdir, "IPM_results.zip"); unlink(z)
        old <- setwd(res$cfg$results_dir); on.exit(setwd(old))
        utils::zip(z, list.files(".", recursive = TRUE), flags = "-r9Xq"); rv$zip <- z
      }
    })
    if (!is.null(rv$report)) showNotification("Report ready - use the download buttons.", type = "message", duration = 8)
  })
  output$dl_report <- downloadHandler(filename = function() sprintf("IPM_report_%s.docx", format(Sys.Date(), "%Y%m%d")),
                                      content = function(f) { req(rv$report); file.copy(rv$report, f) })
  output$dl_zip <- downloadHandler(filename = function() sprintf("IPM_results_%s.zip", format(Sys.Date(), "%Y%m%d")),
                                   content = function(f) { req(rv$zip); file.copy(rv$zip, f) })
}
