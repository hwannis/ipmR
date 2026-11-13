# ipmR Shiny application (shared by the installed package and the shinyapps.io bundle)
# Requires: shiny, bslib, DT, data.table and the ipmR functions to be available.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && (is.na(a) || identical(a, "")))) b else a
.int <- ipm_internals()
output_names <- .int$output_names
extra_fields <- list(hhm = c("sex", "age"), bh = c("death", "age_death", "age_death_unit"), ant = c("height", "flags", "measurer"),
                     fl = c("result", "start_hour", "end_hour", "read_agree", "read_try", "practice", "story", "numeracy_first", "numeracy_next"))
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

# One drop zone for everything: a folder, .sav/.dta files or a .zip, dropped or chosen.
# The files are filtered in the browser (only .sav/.dta/.zip are sent) and handed to one hidden Shiny fileInput,
# whose progress bar stays visible under the zone.
data_drop <- function(id) {
  js <- sprintf("
(function(){
  var ok = /\\.(sav|dta|zip)$/i;
  function zone(){ return document.getElementById('%1$s_zone'); }
  function say(t){ var s = document.getElementById('%1$s_status'); if (s) s.textContent = t; }
  function send(files){
    var keep = files.filter(function(f){ return ok.test(f.name); });
    if (!keep.length) { say('No .sav, .dta or .zip file in what you chose (' + files.length + ' other files).'); return; }
    var mb = keep.reduce(function(a, f){ return a + f.size; }, 0) / 1048576;
    var other = files.length - keep.length;
    say('Sending ' + keep.length + ' data file' + (keep.length > 1 ? 's' : '') + ' (' + mb.toFixed(1) + ' MB)' +
        (other ? '; ' + other + ' other file' + (other > 1 ? 's' : '') + ' ignored.' : '.'));
    var dt = new DataTransfer(); keep.forEach(function(f){ dt.items.add(f); });
    var inp = document.getElementById('%1$s'); inp.files = dt.files;
    inp.dispatchEvent(new Event('change', {bubbles: true}));
  }
  function walk(entry, out){ return new Promise(function(res){
    if (entry.isFile) entry.file(function(f){ out.push(f); res(); }, function(){ res(); });
    else if (entry.isDirectory) { var r = entry.createReader(), all = [];
      (function next(){ r.readEntries(function(es){ if (!es.length) Promise.all(all.map(function(e){ return walk(e, out); })).then(res);
                                                    else { all.push.apply(all, es); next(); } }, function(){ res(); }); })(); }
    else res(); }); }
  document.addEventListener('dragover', function(e){ if (e.target.closest && e.target.closest('#%1$s_zone')) { e.preventDefault(); zone().classList.add('over'); } });
  document.addEventListener('dragleave', function(e){ if (e.target.closest && e.target.closest('#%1$s_zone')) zone().classList.remove('over'); });
  document.addEventListener('drop', function(e){
    if (!(e.target.closest && e.target.closest('#%1$s_zone'))) return;
    e.preventDefault(); zone().classList.remove('over');
    var items = e.dataTransfer.items, entries = [];
    if (items) for (var i = 0; i < items.length; i++) { var en = items[i].webkitGetAsEntry && items[i].webkitGetAsEntry(); if (en) entries.push(en); }
    if (!entries.length) { send(Array.prototype.slice.call(e.dataTransfer.files)); return; }
    say('Reading what was dropped...');
    var out = []; Promise.all(entries.map(function(en){ return walk(en, out); })).then(function(){ send(out); });
  });
  document.addEventListener('click', function(e){
    if (e.target.closest && e.target.closest('#%1$s_folder_link')) { e.preventDefault(); e.stopPropagation(); document.getElementById('%1$s_pick_folder').click(); return; }
    if (e.target.closest && e.target.closest('#%1$s_zone')) document.getElementById('%1$s_pick_files').click();
  });
  document.addEventListener('change', function(e){
    if (e.target.id === '%1$s_pick_files' || e.target.id === '%1$s_pick_folder') { send(Array.prototype.slice.call(e.target.files)); e.target.value = ''; }
  });
})();", id)
  div(id = paste0(id, "_wrap"),
    tags$style(HTML(sprintf("
      #%1$s_zone {border:2px dashed #1CABE2; border-radius:10px; padding:26px 14px; text-align:center; cursor:pointer; background:#f4fbfe; transition:background .15s}
      #%1$s_zone:hover, #%1$s_zone.over {background:#dff3fb}
      #%1$s_zone .big {font-size:1.05em; font-weight:600; color:#0b5a75}
      #%1$s_zone .small {font-size:0.85em; color:#555; margin-top:6px}
      #%1$s_status {font-size:0.85em; margin-top:8px; min-height:1.2em; color:#0b5a75}
      #%1$s_wrap .input-group, #%1$s_wrap .control-label {display:none}
      #%1$s_wrap .shiny-input-container {margin-bottom:0}", id))),
    div(id = paste0(id, "_zone"), role = "button", tabindex = "0", `aria-label` = "Add the survey data",
      div(class = "big", icon("cloud-arrow-up"), " Drop your data here, or click to choose"),
      div(class = "small", "The export folder, the .sav / .dta files, or a .zip - all work. ",
          tags$a(href = "#", id = paste0(id, "_folder_link"), "Choose a folder instead"))),
    div(id = paste0(id, "_status")),
    tags$input(type = "file", id = paste0(id, "_pick_files"), multiple = NA, accept = ".sav,.dta,.zip", style = "display:none"),
    tags$input(type = "file", id = paste0(id, "_pick_folder"), multiple = NA, webkitdirectory = NA, directory = NA, style = "display:none"),
    fileInput(id, NULL, multiple = TRUE, width = "100%"),
    tags$script(HTML(js)))
}

howto <- ipm_howto()
fmt_d <- function(x) format(as.Date(x), "%d %b %Y")
fw_text <- function(fw) if (is.null(fw)) NULL else
  sprintf("Fieldwork in the data (%s): first interview %s, last interview %s - %d days, %s interviews.",
          fw$source %||% "household questionnaire", fmt_d(fw$first), fmt_d(fw$last), as.integer(fw$days), format(fw$interviews, big.mark = ","))
AI_CONNS <- tryCatch(ipm_ai_connections(), error = function(e) { message("AI settings: ", conditionMessage(e)); list() })   # server connections (keys stay on the server)
AI_CUSTOM <- ipm_ai_allow_custom()
AI_CHOICES <- c(if (length(AI_CONNS)) stats::setNames(names(AI_CONNS), vapply(AI_CONNS, function(x) x$label, "")),   # none configured: empty
                if (AI_CUSTOM) c("My own connection (this session only)" = "custom"))
if (!length(AI_CHOICES)) AI_CHOICES <- c("No AI model configured on this server" = "")
fmt_i <- function(x) format(x, scientific = FALSE, trim = TRUE)
prio_col <- c(High = "#E2231A", Medium = "#F26A21", Low = "#8A8A8A")   # Low = grey: nothing major found, not "fine"
md_html <- function(x) {
  x <- gsub("<", "&lt;", x %||% "", fixed = TRUE)                     # model output is never trusted as HTML
  if (requireNamespace("commonmark", quietly = TRUE)) HTML(commonmark::markdown_html(x, extensions = TRUE)) else tags$pre(style = "white-space:pre-wrap", x)
}

# ---------------- background jobs ----------------
# Long tasks (run, report, AI calls) run in a separate R process (callr), so that one user's task never blocks
# the other users of the same server; the session polls the process every second. Without callr, or with
# options(ipmR.background = FALSE), they run in the app process as before.
BG_SRC <- if ("package:ipmR" %in% search()) NULL else if (dir.exists("R")) normalizePath("R") else NA_character_   # NULL: the installed package; else the bundle's R/ folder
BG_OK <- !isFALSE(getOption("ipmR.background", TRUE)) && requireNamespace("callr", quietly = TRUE) && !identical(BG_SRC, NA_character_)
bg_child <- function(what, args, pf, src, libs) {
  .libPaths(libs)
  if (is.null(src)) suppressPackageStartupMessages(library(ipmR))
  else {                                                      # stand-alone bundle: the functions are in R/
    for (pk in c("data.table", "haven", "ggplot2", "officer", "flextable", "openxlsx", "yaml", "tidyselect", "scales", "jsonlite"))
      suppressPackageStartupMessages(library(pk, character.only = TRUE))
    for (f in list.files(src, pattern = "[.][Rr]$", full.names = TRUE)) sys.source(f, envir = globalenv())
  }
  suppressPackageStartupMessages(library(data.table))
  prog <- function(m) cat(paste0(m, "\n"), file = pf, append = TRUE)
  switch(what,
    run = {
      res <- run_ipm(args$cfg, write = FALSE, progress = prog, keep_data = FALSE)
      prog("Team scorecard"); sc <- tryCatch(ipm_scorecard(res), error = function(e) conditionMessage(e))
      prog("Writing summary"); res$summary <- tryCatch(if (is.list(sc)) ipm_summary(res, sc) else ipm_summary(res), error = function(e) NULL)
      list(res = res, sc = sc) },
    report = {
      unlink(args$dir, recursive = TRUE)
      rep <- ipm_report(args$res, args$dir, progress = prog)
      prog("Zipping all results"); unlink(args$zip)
      old <- setwd(args$dir); on.exit(setwd(old), add = TRUE)
      utils::zip(args$zip, list.files(".", recursive = TRUE), flags = "-r9Xq")
      list(report = rep, zip = args$zip) },
    plan = ipm_ai_plan(args$res, args$sc, args$st, language = args$language, compact = args$compact),
    ask = ipm_ai_ask(args$q, args$res, args$sc, args$st, history = args$history, language = args$language, compact = args$compact),
    test = ipm_ai_test(args$st))
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
  (function(){ var t; function reset(){ clearTimeout(t); t = setTimeout(function(){
      if (document.documentElement.classList.contains('shiny-busy')) { reset(); return; }   // a run, report build or AI call in progress is not inactivity
      Shiny.setInputValue('idle_timeout', Date.now()); }, %d); }
    $(document).on('shiny:idle', reset);
    ['mousemove','keydown','click','scroll','touchstart'].forEach(function(e){ document.addEventListener(e, reset, true); });
    $(document).on('shiny:connected', reset); })();", as.integer(round(IDLE_MINUTES * 60 * 1000)))


ipm_ui <- page_navbar(
  title = tags$span(tags$b("IPM"), " Interviewer Performance Monitoring for MICS",
                    tags$span(style = "font-size:0.7em; opacity:0.85; margin-left:10px", paste0("ipmR ", .int$version %||% ""))),
  theme = theme, id = "nav", fillable = FALSE, bg = cyan, inverse = TRUE, selected = "data",
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
    [id^=fi_] + .selectize-control .selectize-input {max-height:120px; overflow-y:auto}
    pre.json {max-height:60vh; overflow:auto; font-size:0.75em; background:#f6f6f6}
    .bf {max-width:1280px; margin:0 auto; padding:6px 4px 30px 4px; font-family:'Segoe UI', Inter, Roboto, Helvetica, Arial, sans-serif}
    .bf-top {display:flex; justify-content:space-between; align-items:flex-end; gap:12px; flex-wrap:wrap; margin:6px 0 16px 0}
    .bf-meta {font-size:0.85em; color:#5f6368}
    .bf-title {font-size:1.6em; font-weight:600; color:#1f2a44; line-height:1.2}
    .bf-kpis {display:grid; grid-template-columns:repeat(auto-fit, minmax(180px, 1fr)); gap:12px; margin-bottom:18px}
    .bf-kpi {background:white; border:1px solid #e6e8ec; border-radius:10px; padding:12px 16px}
    .bf-kpi .l {font-size:0.82em; color:#5f6368}
    .bf-kpi .v {font-size:1.9em; font-weight:600; color:#1f2a44; line-height:1.2}
    .bf-kpi .d {font-size:0.55em; font-weight:600; margin-left:6px; vertical-align:middle}
    .bf-up {color:#C4161C} .bf-down {color:#00833D} .bf-same {color:#8A8A8A}
    .bf-cols {display:grid; grid-template-columns:minmax(0, 7fr) minmax(0, 5fr); gap:18px}
    @media (max-width: 992px) {.bf-cols {grid-template-columns:minmax(0, 1fr)}}
    .bf-card {background:white; border:1px solid #e6e8ec; border-radius:10px; padding:14px 16px; margin-bottom:18px}
    .bf-card h4 {font-size:1.05em; font-weight:600; color:#1f2a44; margin:0 0 4px 0}
    .bf-card .hint {font-size:0.8em; color:#5f6368; margin-bottom:6px}
    .bf-item {display:grid; grid-template-columns:118px minmax(0, 1fr) auto; gap:12px; align-items:start; padding:11px 0; border-top:1px solid #eef0f3}
    @media (max-width: 600px) {.bf-item {grid-template-columns:minmax(0, 1fr)}}
    .bf-pill {display:inline-block; font-size:0.78em; font-weight:600; padding:3px 9px; border-radius:20px; white-space:nowrap}
    .bf-pill.high {background:#FDE8E7; color:#A3120F} .bf-pill.medium {background:#FEF0E6; color:#9A4A0F} .bf-pill.low {background:#F1F2F4; color:#555}
    .bf-pill.iv {background:#EAEFFA; color:#2B3F87}
    .bf-head {font-weight:600; color:#1f2a44; font-size:0.95em}
    .bf-sub {font-size:0.8em; color:#5f6368; margin-top:2px}
    .bf-do {font-size:0.85em; color:#2B3F87; margin-top:4px}
    .bf-acts {display:flex; flex-direction:column; align-items:flex-end; gap:6px}
    .bf-chip {font-size:0.72em; padding:2px 8px; border-radius:20px; white-space:nowrap}
    .bf-chip.new {background:#E6F4FB; color:#0B5A75} .bf-chip.old {background:#FFF3D6; color:#7A5300} .bf-chip.chg {background:#F1F2F4; color:#444}
    .bf-acts .btn {font-size:0.78em; padding:2px 10px; border-radius:6px}
    .bf-grid {border-collapse:separate; border-spacing:3px; width:100%; font-size:0.78em}
    .bf-grid th {font-weight:500; color:#5f6368; text-align:center; padding:0 2px; line-height:1.1}
    .bf-grid td.t {white-space:nowrap; padding-right:6px; cursor:pointer; color:#1f2a44}
    .bf-grid td.c {height:20px; border-radius:4px; background:#F3F4F6; cursor:pointer; text-align:center; color:white; font-size:0.85em}
    .bf-grid td.c.s1 {background:#E2231A} .bf-grid td.c.s2 {background:#F6B26B} .bf-grid td.c.p {outline:2px solid #1f2a44; outline-offset:-2px}
    .bf-legend {font-size:0.75em; color:#5f6368; margin-top:6px}
    .bf-legend span {display:inline-block; width:10px; height:10px; border-radius:2px; margin:0 4px 0 10px; vertical-align:-1px}
    .bf-line {font-size:0.86em; padding:6px 0; border-top:1px solid #eef0f3}
    .bf-kpi .s {font-size:0.75em; color:#5f6368; margin-top:2px}
    .evbars {margin:6px 0 2px 0; max-width:420px}
    .evb {display:grid; grid-template-columns:92px minmax(0, 1fr) 44px; gap:8px; align-items:center; font-size:0.75em; color:#5f6368; margin:2px 0}
    .evb .t {background:#F3F4F6; border-radius:3px; height:9px; overflow:hidden} .evb .f {height:9px; border-radius:3px}
    .evb .n {text-align:right; color:#1f2a44; font-weight:600}
    .tp-head {display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; max-width:1280px; margin:18px auto 0 auto; padding:0 12px}
    .tp-head .form-group {margin-bottom:0}
    .tp-tbl {width:100%; border-collapse:collapse; font-size:0.84em}
    .tp-tbl th {font-weight:500; color:#5f6368; text-align:left; padding:4px 6px; border-bottom:1px solid #e6e8ec}
    .tp-tbl td {padding:6px; border-top:1px solid #eef0f3; vertical-align:top} .tp-tbl td.m {color:#5f6368}
    .tp-tbl tr[onclick] {cursor:pointer} .tp-tbl tr[onclick]:hover td {background:#F5F9FF}
    .tp-ct {font-size:0.8em; font-weight:600; color:#1f2a44; margin-top:6px}
    .bf-pill.go {cursor:pointer}
    .bf-med {display:grid; grid-template-columns:118px minmax(0, 1fr) auto; gap:12px; align-items:center; padding:7px 0; border-top:1px solid #eef0f3; font-size:0.88em}
    @media (max-width: 600px) {.bf-med {grid-template-columns:minmax(0, 1fr)}}
    .bf-more summary {cursor:pointer; color:#2B3F87; font-size:0.86em; font-weight:600; padding:9px 0 4px 0; border-top:1px solid #eef0f3}
    .bf-more .btn {font-size:0.78em; padding:2px 10px; border-radius:6px}
    .name-box {background:#FFF8E1; border:1px solid #F3D27A; border-radius:8px; padding:10px 12px 2px 12px; margin:10px 0}
    .bf-empty {max-width:640px; margin:60px auto; text-align:center}
    .bf-ev {border-top:1px solid #eef0f3; padding:10px 0}
    .bf-ev .e {font-size:0.85em; color:#444; margin-top:3px}
    .bf-ev .a {font-size:0.85em; color:#2B3F87; margin-top:3px}"))),

  # ---------------- BRIEFING (home after a run) ----------------
  nav_panel(tagList(icon("bolt"), "Briefing"), value = "briefing", uiOutput("brief")),
  nav_panel(tagList(icon("users"), "Teams"), value = "team",
    div(class = "tp-head", selectInput("tp_team", NULL, choices = NULL, width = "340px"),
        downloadButton("tp_brief", "Supervisor brief (Word)", class = "btn-sm btn-outline-primary")),
    uiOutput("tp_body")),
  nav_panel(tagList(icon("user"), "Interviewers"), value = "interv",
    div(class = "tp-head", selectizeInput("ip_iv", NULL, choices = NULL, width = "340px")),
    uiOutput("ip_body")),

  nav_menu(tagList(icon("sliders"), "Setup"), value = "setup",
  # ---------------- 1. DATA ----------------
  nav_panel("1. Data", value = "data",
    explain("Loads the survey data files exported from CSPro/SPSS (hh, hl, wm, bh, ch, fs, mn ... in .sav or .dta format) and automatically recognises the MICS structure: which file is which questionnaire, the interviewer, result, date and time variables, the links between files, the teams and the modules to test. It works for any MICS survey (MICS5-MICS7) whatever the language of the labels.",
            "Drop the folder of your latest data export (or its .sav files, or a .zip of it) on the box, or click the box to choose. Wait for the green confirmation, check the table, then go to the next step."),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Load the survey datasets"),
        data_drop("up_data"),
        if (LOCAL_MODE) tags$details(style = "margin-top:6px; font-size:0.9em",
          tags$summary("Very large files on this computer or a network drive? Read them where they are"),
          div(style = "margin-top:8px", textInput("data_dir", NULL, value = getOption("ipmR.data_dir", ""), width = "100%", placeholder = "e.g. Z:/MICS/Somalia/21 Oct"),
              actionButton("scan", "Read this folder", class = "btn-outline-primary btn-sm"))),
        div(class = "howto", tags$b("Your data. "), sprintf("Files are processed only within your own session and are never shared with other users. Name and other free-text variables are removed when the files are read. All uploaded files, results and reports are deleted from the server when you close the browser tab, after %s minutes of inactivity, or immediately with the button below. Do not upload data you are not authorised to process on this server.", format(IDLE_MINUTES))),
        actionButton("wipe", "Delete my data now", class = "btn-outline-danger")),
      card(card_header("What was found"), uiOutput("found_ui"), uiOutput("name_box"), tableOutput("found_tbl"))),
    div(style = "text-align:right", actionButton("go2", HTML("Next: survey settings &rarr;"), class = "btn-primary"))),

  # ---------------- 2. SETTINGS & OUTPUTS ----------------
  nav_panel("2. Settings & outputs", value = "settings",
    explain("Sets the survey name, the period of analysis and the outputs to produce, and identifies the variables needed for the household members, birth history and anthropometry outputs.",
            "Check the survey name. The period of analysis (21 days recommended, normally the most recent 3 weeks) is used for the flag analysis and the anthropometry case listing; all other outputs use the whole fieldwork. Untick outputs you do not need, and check the variables in the boxes on the right (they were filled in automatically)."),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Survey and period"),
        textInput("survey_name", "Name of the survey", width = "100%", placeholder = "e.g. Madagascar MICS7 2025"),
        fluidRow(column(6, dateInput("period_start", "Period of analysis: first day", value = Sys.Date() - 21)),
                 column(6, numericInput("period_days", "Extent (days)", value = 21, min = 1))),
        div(style = "margin:-6px 0 8px 0", tags$span(class = "small-note", "Set to: "),
            actionButton("pp_7", "Last 7 days", class = "btn-sm"), actionButton("pp_14", "Last 14 days", class = "btn-sm"),
            actionButton("pp_21", "Last 21 days", class = "btn-sm"), actionButton("pp_all", "Whole fieldwork", class = "btn-sm")),
        uiOutput("period_info"),
        checkboxInput("o_period_weeks_only", "Descriptive charts (outputs 2-8 and FL): show only the weeks of the period of analysis (otherwise the whole fieldwork, with those weeks shaded)", FALSE),
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
    explain("Assigns each interviewer to a team. All comparisons are made within teams (an interviewer is compared with team-mates working in the same clusters), so correct teams are essential. Teams were derived automatically from the supervisor / team leader number; in the results each team is labelled T1, T2 ... followed by that code, e.g. T3 (103), so the field teams recognise it.",
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
            "Review the modules and the variables to ignore, then click 'Check flag settings' and go through the list: ideally every module is tested, and every variable that is not a direct question to the respondent or a direct observation is ignored. This takes time the first time (save the settings to reuse them). The advanced parameters reproduce the original IPM defaults; change them only if you know why."),
    uiOutput("flag_ui"),
    div(style = "border:1px solid #e3e3e3; padding:10px 16px; margin:12px 0", tags$h5(class = "sch", style = "margin-top:4px", "Check the flag settings"),
      p(class = "small-note", "Lists every variable of the modules to test with its label and status (tested or ignored), as the check in the Stata application. Variables that are not questions to the respondent or direct observations (identifiers, CAPI-filled, derived, counts) should be ignored, because they change the % of tests flagged: CAPI-filled and derived variables are almost never flagged, so they lower the % of a module and hide its real problems; identifiers and staff codes give meaningless flags. Review it the first time you set up a survey; the settings can then be saved and reused."),
      div(actionButton("fc_run", "Check flag settings", class = "btn-primary"), downloadButton("fc_dl", "Download the full list (CSV)", class = "btn-sm")),
      uiOutput("fc_problems"), DT::DTOutput("fc_modules"), DT::DTOutput("fc_vars")),
    card(card_header("Advanced parameters"),
      layout_columns(col_widths = c(2, 2, 2, 2, 2, 2, 12),
        numericInput("o_levelsig", "Significance level", 0.001, 0, 1, 0.0005),
        numericInput("o_test_min", "Min. obs per interviewer", 5, 1),
        numericInput("o_max_uniq", "Max. distinct values (categorical)", 34, 2),
        numericInput("o_highly_flag", "Highly flagged (%)", 10, 1),
        selectInput("o_woi_anchor", "Week 1 anchor", c("Per dataset (IPM)" = "dataset", "Survey-wide (hh)" = "survey")),
        checkboxInput("o_test_min_nonmissing", "Min. obs counted on answered cases", TRUE),
        checkboxInput("o_stata_compat", "Stata-compatible numbers (period of start + days, i.e. one day more; records up to 6 days before week 1 counted in week 1; min. obs on records)", FALSE))),
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
        fileInput("cfg_upload", "Load settings (YAML)", accept = c(".yml", ".yaml"), width = "100%"))))),

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
        explain("One line per team with a priority level computed from all warning signs: excess of flags compared with the survey rate, modules where the team is far above the other teams, interviewers with many flags, very short interviews, low completion, omitted household members, births or deaths, height heaping, out-of-range anthropometry and the foundational learning checks (last 3 weeks for the descriptive signs). Every sign is listed below with its evidence and the standard action.",
                "Start with the High-priority teams. Click a team in the first table to filter the list of warning signs. The same evidence is used by the action plan (next tab)."),
        uiOutput("sc_kpis"),
        tags$h5(class = "sch", "Teams (highest priority first)"), DT::DTOutput("sc_teams"),
        div(class = "howto", tags$b("How to read: "), "Each issue is a questionnaire problem (question or module flagged in most teams - not counted against teams), a whole-team issue (the team differs from the other teams) or an interviewer issue (one interviewer differs from team-mates). High priority = a team-wide excess of flags (at least 1.5 times the survey rate, binomial p < 0.01, questionnaire problems left out) or 2 or more high-severity issues; Medium = 1 high-severity issue or 3 or more issues. Points (3 per high, 1 per medium) only order the teams. 'x survey rate' compares the team's % of flagged tests with the survey as a whole (1 = average)."),
        tags$h5(class = "sch", "Warning signs with evidence and standard action"), DT::DTOutput("sc_signals"),
        tags$h5(class = "sch", "Interviewers"), DT::DTOutput("sc_interv")),
      nav_panel("Action plan (AI)", value = "ai",
        explain(tagList("Writes a team-by-team action plan (issues with evidence, actions with owner and timing, interviewers to observe, modules to retrain) and answers questions about the results. ",
                        tags$b("Only aggregated statistics are sent to the AI model"), " - the team scorecard, flag rates per module and question, one-line readings of flagged questions, interviewer codes. No record-level data, household identifiers or names leave the server; click 'Preview what is sent to the AI' to see the full request (instructions, evidence and question) exactly as it is sent. Without AI, the rule-based plan gives the same structure from the standard action library."),
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
            checkboxInput("ai_compact", "Compact evidence (High/Medium teams only, about a third of the size: faster and better with local models)", FALSE),
            actionButton("ai_go", "Generate AI action plan", class = "btn-primary", width = "100%"),
            actionButton("rules_go", "Rule-based plan (no AI)", width = "100%"),
            actionButton("ai_preview", "Preview what is sent to the AI", width = "100%"),
            downloadButton("plan_dl", "Download action plan (Word)", style = "width:100%")),
          card(card_header("Ask a question about the results"),
            uiOutput("chat"),
            textAreaInput("ai_q", NULL, width = "100%", rows = 2,
                          placeholder = "e.g. Why is team T4 flagged so much? Which interviewers should the supervisor of T1 observe first? Is the IC module a translation problem?"),
            actionButton("ai_ask", "Ask", class = "btn-primary"))),
        uiOutput("plan_ui")),
      nav_panel("Figures", value = "figures",
        explain("All graphs of the IPM report, one at a time or all the graphs of an output on one scrolling page. On the graphs of flags by module, point at (or tap) a circle to see the flag table of that interviewer and question.",
                "Choose a graph in the list; read the 'How to read' note below it. Use the download buttons for a sharp version to enlarge, print or put in a presentation (SVG can be zoomed without loss)."),
        fluidRow(column(8, uiOutput("fig_select")),
                 column(4, radioButtons("fig_mode", NULL, c("One graph" = "one", "All graphs of this output on one page" = "all"), inline = TRUE))),
        uiOutput("fig_howto"),
        conditionalPanel("input.fig_mode == 'one'",
          div(style = "margin-bottom:6px", downloadButton("fig_svg", "Sharp version (SVG)", class = "btn-sm"), downloadButton("fig_png", "High-resolution image (PNG)", class = "btn-sm")),
          plotOutput("fig", height = "650px", hover = hoverOpts("fig_hover", delay = 150, delayType = "debounce"), click = "fig_click"),
          uiOutput("fig_point")),
        conditionalPanel("input.fig_mode == 'all'", uiOutput("fig_all"))),
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
    p(class = "small-note", sprintf("ipmR %s - R port of the Stata IPM application v7.0.0 developed by Pierre Martel for UNICEF MICS.", .int$version %||% "")))
)

ipm_server <- function(input, output, session) {
  sweep_stale()
  message("session ", substr(session$token, 1, 6), " started; tempdir ", tempdir())
  sdir <- file.path(tempdir(), paste0("ipm_", session$token)); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  upload_dirs <- character()                       # Shiny's own temporary copies of uploaded files
  rv <- reactiveValues(cfg = NULL, light = NULL, res = NULL, log = character(), report = NULL, zip = NULL, n_files = 0,
                       sc = NULL, cmp = NULL, plan = NULL, chat = list(), ai_user = NULL, ai_test = NULL, prev = NULL, stale_model = NULL, listed = character(), jobs = 0L)
  cleanup <- function() {
    if (exists("kill_jobs")) kill_jobs()
    wipe(c(sdir, upload_dirs))
    isolate({ rv$cfg <- NULL; rv$light <- NULL; rv$res <- NULL; rv$report <- NULL; rv$zip <- NULL
              rv$sc <- NULL; rv$cmp <- NULL; rv$plan <- NULL; rv$chat <- list(); rv$ai_user <- NULL; rv$ai_test <- NULL })
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

  # ---------- background jobs (see bg_child) ----------
  bg <- new.env()
  job_running <- function(key) !is.null(bg[[key]])
  # step: progress added per message; hold: messages matching it update the text only
  start_job <- function(key, what, args, title, detail = NULL, done, step = 1 / 25, hold = NULL, log = FALSE) {
    if (job_running(key)) { showNotification("This task is already running: wait for it to finish.", type = "warning"); return(invisible(FALSE)) }
    if (!BG_OK) {                                                # in-process fallback (blocks this R process)
      r <- withProgress(message = title, detail = detail, value = 0.05, {
        pf <- tempfile(tmpdir = sdir, fileext = ".log"); on.exit(unlink(pf), add = TRUE)
        tryCatch(bg_child_local(what, args, function(m) { if (log) logm(m); if (!is.null(hold) && grepl(hold, m)) setProgress(detail = m) else incProgress(step, detail = m) }),
                 error = function(e) e) })
      done(r); return(invisible(TRUE))
    }
    dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
    pf <- tempfile(tmpdir = sdir, fileext = ".log"); file.create(pf)
    p <- tryCatch(callr::r_bg(bg_child, args = list(what = what, args = args, pf = pf, src = BG_SRC, libs = .libPaths()), supervise = TRUE),
                  error = function(e) { showNotification(paste("Could not start the task:", conditionMessage(e)), type = "error", duration = NULL); NULL })
    if (is.null(p)) return(invisible(FALSE))
    pr <- shiny::Progress$new(session, min = 0, max = 1); pr$set(message = title, detail = detail, value = 0.02)
    bg[[key]] <- list(p = p, pf = pf, pr = pr, done = done, n = 0L, v = 0.02, step = step, hold = hold, log = log)
    rv$jobs <- rv$jobs + 1L
    invisible(TRUE)
  }
  bg_child_local <- function(what, args, prog) {               # same jobs, in this process
    switch(what,
      run = { res <- run_ipm(args$cfg, write = FALSE, progress = prog, keep_data = FALSE)
              prog("Team scorecard"); sc <- tryCatch(ipm_scorecard(res), error = function(e) conditionMessage(e))
              prog("Writing summary"); res$summary <- tryCatch(if (is.list(sc)) ipm_summary(res, sc) else ipm_summary(res), error = function(e) NULL)
              list(res = res, sc = sc) },
      report = { unlink(args$dir, recursive = TRUE); rep <- ipm_report(args$res, args$dir, progress = prog)
                 prog("Zipping all results"); unlink(args$zip); old <- setwd(args$dir); on.exit(setwd(old), add = TRUE)
                 utils::zip(args$zip, list.files(".", recursive = TRUE), flags = "-r9Xq"); list(report = rep, zip = args$zip) },
      plan = ipm_ai_plan(args$res, args$sc, args$st, language = args$language, compact = args$compact),
      ask = ipm_ai_ask(args$q, args$res, args$sc, args$st, history = args$history, language = args$language, compact = args$compact),
      test = ipm_ai_test(args$st))
  }
  job_error <- function(r) { e <- if (!is.null(r$parent)) r$parent else r; msg <- conditionMessage(e)
    if (!nzchar(msg)) msg <- "the task stopped without a message (out of memory?)"; msg }
  observe({
    rv$jobs; alive <- FALSE
    for (key in ls(bg)) {
      j <- bg[[key]]
      msgs <- tryCatch(readLines(j$pf, warn = FALSE), error = function(e) character())
      if (length(msgs) > j$n) {
        for (m in msgs[(j$n + 1L):length(msgs)]) {
          if (j$log) isolate(logm(m))
          if (is.null(j$hold) || !grepl(j$hold, m)) j$v <- min(0.95, j$v + j$step)
        }
        j$pr$set(value = j$v, detail = utils::tail(msgs, 1)); j$n <- length(msgs); bg[[key]] <- j
      }
      if (j$p$is_alive()) { alive <- TRUE; next }
      j$pr$close(); rm(list = key, envir = bg); unlink(j$pf)
      r <- tryCatch(j$p$get_result(), error = function(e) e)
      isolate(j$done(r))
    }
    if (alive) invalidateLater(1000)
  })
  kill_jobs <- function() for (key in ls(bg)) { j <- bg[[key]]; try(j$p$kill(), silent = TRUE); try(j$pr$close(), silent = TRUE); unlink(j$pf); rm(list = key, envir = bg) }
  if (!is.null(pre <- getOption("ipmR.preload"))) isolate({   # testing only: preload a saved result
    rv$res <- readRDS(pre); rv$sc <- tryCatch(ipm_scorecard(rv$res), error = function(e) NULL)
    rv$cmp <- tryCatch(ipm_history_update(rv$res, rv$sc), error = function(e) NULL) })

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
      cfg$survey_name <- ""
      apply_cfg(cfg, keep_paths = TRUE)
    })
    rv$n_files <- n
    showNotification(sprintf("%d datasets read: %d questionnaires recognised, %d teams.", n,
                             sum(vapply(rv$cfg$datasets, function(d) !is.null(d$result), logical(1))), length(rv$cfg$teams$ranges)), type = "message", duration = 8)
  }
  observeEvent(input$up_data, ingest(input$up_data$datapath, input$up_data$name))
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
    updateSelectInput(session, "o_woi_anchor", selected = o$woi_anchor); updateCheckboxInput(session, "o_test_min_nonmissing", value = isTRUE(o$test_min_nonmissing))
    updateCheckboxInput(session, "o_stata_compat", value = isTRUE(o$stata_compat))
    updateCheckboxInput(session, "o_period_weeks_only", value = isTRUE(o$period_weeks_only))
  }
  observeEvent(input$cfg_upload, {
    req(rv$cfg); cfg <- read_config(input$cfg_upload$datapath)
    keep <- intersect(names(cfg$datasets), names(rv$cfg$datasets))
    if (!length(keep)) { showNotification("The settings file does not match the uploaded datasets.", type = "error"); return() }
    cfg$datasets <- cfg$datasets[keep]; cfg$fieldwork <- rv$cfg$fieldwork; apply_cfg(cfg)
    showNotification("Settings loaded.", type = "message")
  })

  # survey name asked on the data page too (same value as on the settings page)
  output$name_box <- renderUI({
    req(rv$cfg)
    div(class = "name-box",
        textInput("survey_name_d", tagList(tags$b("Name of the survey"), " - shown on the briefing and the reports, and used to keep the run history of this survey apart from others"),
                  value = isolate(input$survey_name) %||% "", width = "100%", placeholder = "e.g. Madagascar MICS7 2025"))
  })
  observeEvent(input$survey_name_d, if (!identical(input$survey_name_d, input$survey_name)) updateTextInput(session, "survey_name", value = input$survey_name_d))
  observeEvent(input$survey_name, if (!is.null(input$survey_name_d) && !identical(input$survey_name_d, input$survey_name)) updateTextInput(session, "survey_name_d", value = input$survey_name))
  output$found_ui <- renderUI({
    cfg <- cur_cfg()
    if (is.null(cfg)) return(p(class = "small-note", "No data loaded yet."))
    q <- names(cfg$datasets)[vapply(cfg$datasets, function(d) !is.null(d$result), logical(1))]
    div(div(class = "bignum", sprintf("%d files, %d questionnaires, %d teams", length(cfg$datasets), length(q), length(cfg$teams$ranges %||% character()))),
        if (!is.null(cfg$fieldwork)) p(tags$b(fw_text(cfg$fieldwork))),
        if (length(cfg$fieldwork$by_questionnaire) > 1) tags$table(class = "table table-sm", style = "width:auto; font-size:0.85em",
          tags$tr(tags$th("Questionnaire"), tags$th("First interview"), tags$th("Last interview"), tags$th("Records")),
          lapply(names(cfg$fieldwork$by_questionnaire), function(q) { z <- cfg$fieldwork$by_questionnaire[[q]]
            tags$tr(tags$td(q), tags$td(fmt_d(z$first)), tags$td(fmt_d(z$last)), tags$td(format(z$interviews, big.mark = ","))) })),
        p(sprintf("Period of analysis proposed: from %s, %s days (%s; change it on the next page). Outputs available: %d of 9.",
                  if (is.null(cfg$period$start)) "-" else fmt_d(cfg$period$start), cfg$period$days,
                  if (!is.null(cfg$fieldwork) && cfg$fieldwork$days <= 21) "the whole fieldwork so far" else "the last 3 weeks of fieldwork",
                  sum(unlist(cfg$outputs)))))
  })
  set_period <- function(n = NULL) {
    fw <- rv$cfg$fieldwork; if (is.null(fw)) { showNotification("No interview dates found in the household questionnaire.", type = "warning"); return() }
    last <- as.Date(fw$last); first <- as.Date(fw$first)
    if (is.null(n)) { st <- first; n <- as.integer(fw$days) } else st <- max(first, last - (n - 1))
    updateDateInput(session, "period_start", value = st); updateNumericInput(session, "period_days", value = n)
  }
  observeEvent(input$pp_7, set_period(7)); observeEvent(input$pp_14, set_period(14))
  observeEvent(input$pp_21, set_period(21)); observeEvent(input$pp_all, set_period(NULL))
  output$period_info <- renderUI({
    fw <- rv$cfg$fieldwork; st <- input$period_start; n <- input$period_days
    if (is.null(fw)) return(p(class = "small-note", "Interview dates are shown here once the data are loaded."))
    first <- as.Date(fw$first); last <- as.Date(fw$last)
    msg <- if (length(st) && !is.na(st) && length(n) && !is.na(n)) {
      en <- st + n                                                  # as in the Stata IPM: start to start + extent, both included
      if (st > last) tags$span(style = "color:#E2231A", "The period starts after the last interview: there are no data to analyse.")
      else if (en < first) tags$span(style = "color:#E2231A", "The period ends before the first interview: there are no data to analyse.")
      else sprintf("Period analysed: %s to %s%s.", fmt_d(max(st, first)), fmt_d(min(en, last)),
                   if (en < last) sprintf(" - the last %d day(s) of fieldwork are not included", as.integer(last - en)) else " (up to the last interview)")
    }
    div(class = "small-note", style = "margin-bottom:10px", div(fw_text(fw)), div(msg))
  })

  cur_cfg <- reactive({ req(rv$cfg); tryCatch(collect_cfg(), error = function(e) rv$cfg) })   # settings as currently edited
  output$found_tbl <- renderTable({
    cfg <- cur_cfg(); req(cfg)
    rbindlist(lapply(names(cfg$datasets), function(n) { d <- cfg$datasets[[n]]
      data.table(file = d$file, type = if (!is.null(d$result)) "questionnaire" else if (!is.null(d$parent)) paste("linked to", d$parent) else "not used",
                 interviewer = d$interviewer %||% (if (!is.null(d$parent)) sprintf("from %s: %s", d$parent, cfg$datasets[[d$parent]]$interviewer %||% "?") else ""),
                 result = d$result %||% "", date = d$day %||% (if (!is.null(d$parent)) sprintf("from %s: %s", d$parent, cfg$datasets[[d$parent]]$day %||% "?") else ""),
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
        column(1, sel(paste0("ds_", nm, "_keys"), "Link keys", ch[-1], d$keys, multiple = TRUE))),
        uiOutput(paste0("inh_", nm)))
    }))
  })
  observeEvent(rv$cfg, { for (nm in names(rv$cfg$datasets)) local({ n <- nm
    output[[paste0("inh_", n)]] <- renderUI({
      par <- input[[paste0("ds_", n, "_parent")]] %||% ""; if (!nzchar(par)) return(NULL)
      keys <- input[[paste0("ds_", n, "_keys")]]
      pint <- input[[paste0("ds_", par, "_interviewer")]] %||% rv$cfg$datasets[[par]]$interviewer %||% "?"
      pday <- input[[paste0("ds_", par, "_day")]] %||% rv$cfg$datasets[[par]]$day %||% "?"
      own_i <- input[[paste0("ds_", n, "_interviewer")]] %||% ""; own_d <- input[[paste0("ds_", n, "_day")]] %||% ""
      mdy <- if (!identical(pday, "?") && !is.null(rv$light$meta[[par]])) { v <- names(rv$light$meta[[par]]$varlab); i <- match(pday, v); if (!is.na(i)) paste(v[i:min(i + 2, length(v))], collapse = "/") else pday } else pday
      div(class = "small-note", style = "margin:-4px 0 6px 9%; color:#374EA2",
        sprintf("Linked to %s through %s. Interviewer: %s. Date of interview: %s.", par,
                if (length(keys)) paste(keys, collapse = " + ") else "(no keys chosen - choose the link keys)",
                if (nzchar(own_i)) sprintf("%s (own variable)", own_i) else sprintf("taken from %s (%s)", par, pint),
                if (nzchar(own_d)) sprintf("%s (own variable)", own_d) else sprintf("taken from %s (%s)", par, mdy)),
        " Leave Interviewer and Day empty to take them from the parent.")
    }) }) })
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
        lapply(names(fields), function(f) sel(paste0("ex_", key, "_", f), fields[[f]], var_choices(meta, ds), e[[key]][[f]], multiple = f %in% c("flags", "practice"))))
    }
    tagList(
      mk("hhm", "Household members list (sex 1=M 2=F; age in years)", list(sex = "Sex", age = "Age")),
      mk("bh", "Birth history (death coded 2; age at death in months)", list(death = "Living status", age_death = "Age at death (months, or number if a unit is given)", age_death_unit = "Age at death unit (optional)")),
      mk("ant", "Anthropometry (height in cm; flags coded 1)", list(height = "Height / length", flags = "Out-of-range / flag variables", measurer = "Measurer number (heaping by measurer)")),
      mk("fl", "Foundational learning (5-17 questionnaire; result 1 = completed, wants to read 1 = yes)", list(result = "Result of the FL assessment (FL28)",
         start_hour = "Start with the child - hour (FL2H)", end_hour = "End - hour (end of interview if empty)", read_agree = "Child wants to read (FL10)", read_try = "Tries a story in another language (FL10C; 95 = does not want to try)",
         practice = "Reading practice items, in order (FL14 FL15 FL17)", story = "Reading story started (FL19INTRO)",
         numeracy_first = "First numeracy task (FL23A)", numeracy_next = "Next numeracy task (FL24A)")))
  })
  outputOptions(output, "extras_ui", suspendWhenHidden = FALSE)
  for (k in c("hhm", "bh", "ant", "fl")) local({
    key <- k
    observeEvent(input[[paste0("ex_", key, "_dataset")]], {
      ds <- input[[paste0("ex_", key, "_dataset")]]; req(rv$light)
      flds <- extra_fields[[key]]
      for (f in flds) updateSelectizeInput(session, paste0("ex_", key, "_", f), choices = var_choices(rv$light$meta, ds),
                                           selected = if (identical(ds, rv$cfg$extras[[key]]$dataset)) rv$cfg$extras[[key]][[f]] else "")
    }, ignoreInit = TRUE)
  })

  fc <- eventReactive(input$fc_run, { req(rv$light); ipm_flag_check(collect_cfg(), rv$light$meta) })
  output$fc_problems <- renderUI({ x <- fc(); if (!length(x$problems)) return(p(class = "small-note", style = "color:#00833D", "No module without variables and no unknown variable to ignore."))
    div(class = "aiwarn", tags$ul(lapply(x$problems, tags$li))) })
  output$fc_modules <- DT::renderDT({ x <- fc()
    DT::datatable(x$modules[, .(Dataset = dataset, Module = module, Variables = variables, Tested = tested, Ignored = ignored, `Ignored variables` = ignored_list)],
                  rownames = FALSE, selection = "single", filter = "top", fillContainer = FALSE, options = list(pageLength = 10, scrollX = TRUE),
                  caption = "Click a module to see its variables") })
  output$fc_vars <- DT::renderDT({ x <- fc(); i <- input$fc_modules_rows_selected; req(length(i) > 0, i[1] <= nrow(x$modules)); m <- x$modules[i[1]]
    DT::datatable(x$variables[dataset == m$dataset & module == m$module, .(Variable = variable, Label = label, Status = status)], rownames = FALSE,
                  fillContainer = FALSE, options = list(pageLength = 50, dom = "tp"), caption = sprintf("%s, module %s", m$dataset, m$module)) |>
      DT::formatStyle("Status", color = DT::styleEqual("ignored", "#999999")) })
  output$fc_dl <- downloadHandler(filename = function() "ipm_flag_settings_check.csv", content = function(f) {
    req(rv$light); data.table::fwrite(ipm_flag_check(collect_cfg(), rv$light$meta)$variables, f) })

  output$flag_ui <- renderUI({
    cfg <- rv$cfg; req(cfg, rv$light); meta <- rv$light$meta
    tagList(lapply(names(cfg$datasets), function(nm) {
      d <- cfg$datasets[[nm]]
      div(class = "dsrow", fluidRow(
        column(1, div(class = "dsname", nm)),
        column(5, textInput(paste0("fm_", nm), "Modules to test", paste(d$flag_modules, collapse = " "), width = "100%")),
        column(4, selectizeInput(paste0("fi_", nm), "Variables to ignore", choices = unique(c(var_choices(meta, nm)[-1], stats::setNames(d$flag_ignore, d$flag_ignore))),
                                 selected = d$flag_ignore, multiple = TRUE, width = "100%",
                                 options = list(create = TRUE, plugins = list("remove_button"), placeholder = "pick variables, or type a name / range (HL1-HL10)"))),
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
    cfg$survey_name <- trimws(g("survey_name", cfg$survey_name) %||% "")
    if (!nzchar(cfg$survey_name)) cfg$survey_name <- "MICS survey"
    cfg$period <- list(start = if (length(input$period_start) && !is.na(input$period_start)) format(input$period_start) else cfg$period$start, days = g("period_days", 21))
    ds <- list()
    for (nm in names(cfg$datasets)) {
      if (isFALSE(input[[paste0("inc_", nm)]])) next
      d <- cfg$datasets[[nm]]
      for (f in c(names(ds_fields), "parent")) d[[f]] <- nz(g(paste0("ds_", nm, "_", f), d[[f]]))
      d$keys <- g(paste0("ds_", nm, "_keys"), d$keys) %||% character()
      d$flag_modules <- split_ws(g(paste0("fm_", nm), paste(d$flag_modules, collapse = " ")))
      d$flag_ignore <- split_ws(paste(g(paste0("fi_", nm), d$flag_ignore), collapse = " "))
      d$flag_units <- g(paste0("fu_", nm), d$flag_units) %||% character()
      ds[[nm]] <- d
    }
    cfg$datasets <- ds
    if (identical(input$team_method, "variable")) cfg$teams <- list(variable = g("team_var", "HH4"))
    else { r <- trimws(unlist(strsplit(g("team_ranges", paste(cfg$teams$ranges, collapse = "\n")), "\n"))); cfg$teams <- list(ranges = r[nzchar(r)]) }
    sel_out <- g("outputs", names(cfg$outputs)[unlist(cfg$outputs)])
    cfg$outputs <- as.list(stats::setNames(names(output_names) %in% sel_out, names(output_names)))
    cfg$hhm_groups <- g("hhm_groups", cfg$hhm_groups)
    for (k in c("hhm", "bh", "ant", "fl")) {
      ds_k <- nz(g(paste0("ex_", k, "_dataset"), cfg$extras[[k]]$dataset))
      flds <- extra_fields[[k]]
      if (is.null(ds_k)) { cfg$extras[[k]] <- NULL; next }
      e <- list(dataset = ds_k); for (f in flds) e[[f]] <- nz(g(paste0("ex_", k, "_", f), cfg$extras[[k]][[f]]))
      cfg$extras[[k]] <- e
    }
    for (k in c("levelsig", "test_min", "max_uniq", "highly_flag", "woi_anchor", "test_min_nonmissing", "stata_compat", "period_weeks_only")) cfg$options[[k]] <- g(paste0("o_", k), cfg$options[[k]])
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
      cols <- unique(c(d$interviewer, cfg$teams$variable, d$supervisor))
      x <- tryCatch(.int$read_cols(file.path(cfg$data_dir, d$file), cols), error = function(e) NULL); if (is.null(x) || !d$interviewer %in% names(x)) next
      iv <- as.numeric(x[[d$interviewer]]); sup <- if (!is.null(cfg$teams$variable) && cfg$teams$variable %in% names(x)) as.numeric(x[[cfg$teams$variable]]) else NULL
      code <- if (!is.null(d$supervisor) && d$supervisor %in% names(x)) suppressWarnings(as.numeric(x[[d$supervisor]])) else NA_real_
      rows[[n]] <- data.table(interv = iv, sup = if (is.null(sup)) NA_real_ else sup, code = code) }
    req(length(rows) > 0); a <- rbindlist(rows)[!is.na(interv)]
    lv <- if (!is.null(cfg$teams$variable)) sort(unique(a$sup)) else NULL
    a[, team := .int$assign_teams(interv, if (is.null(lv)) NULL else sup, cfg, lv)]
    if (!is.null(lv)) a[, code := sup]
    codes <- a[!is.na(team) & !is.na(code), .N, by = .(team, code)][order(team, -N)][, .(code = paste(fmt_i(code), collapse = ", ")), by = team]
    a <- unique(a[, .(interv, team)])
    out <- a[order(team, interv), .(interviewers = paste(sort(unique(interv)), collapse = ", "), n = uniqueN(interv)), by = team]
    out[, `supervisor / team leader code` := codes$code[match(team, codes$team)]]
    out[, team := ifelse(is.na(team), "NOT ASSIGNED (excluded)", paste0("T", team))]
    out[is.na(`supervisor / team leader code`), `supervisor / team leader code` := ""]
    out
  })

  # ---------- run ----------
  observeEvent(input$run, {
    req(rv$cfg); if (job_running("run")) { showNotification("The analysis is already running.", type = "warning"); return() }
    cfg <- collect_cfg(); rv$log <- character(); rv$report <- NULL; rv$zip <- NULL
    rv$res <- NULL; rv$sc <- NULL; rv$cmp <- NULL; rv$plan <- NULL; rv$chat <- list(); gc(verbose = FALSE)
    start_job("run", "run", list(cfg = cfg), "Running the IPM", if (BG_OK) "you can keep using the app; other users are not slowed down" else NULL, log = TRUE,
      done = function(r) {
        if (inherits(r, "error")) { m <- job_error(r); logm("ERROR:", m); showNotification(m, type = "error", duration = NULL); return() }
        if (is.character(r$sc)) { logm("Scorecard:", r$sc); r$sc <- NULL }
        rv$cmp <- if (!is.null(r$sc)) tryCatch(ipm_history_update(r$res, r$sc), error = function(e) { logm("Run history:", conditionMessage(e)); NULL })
        rv$sc <- r$sc; rv$res <- r$res; logm("Done."); nav_to(if (!is.null(r$sc)) "briefing" else "results")
      })
  })
  output$log <- renderText(paste(rev(rv$log), collapse = "\n"))

  # ---------- briefing (home page after a run) ----------
  brief <- reactive({ req(rv$sc); ipm_briefing(rv$sc, rv$cmp) })
  bf_nth <- function(n) paste0(n, if (n %% 100 %in% 11:13) "th" else c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1])
  bf_date <- function(d) if (is.null(d) || is.na(d)) "-" else format(as.Date(d), "%d %b %Y")
  bf_chip <- function(status, prev_priority = NA, priority = NA) tagList(
    if (!is.na(status) && status != "first") span(class = paste("bf-chip", if (status == "new") "new" else "old"), if (status == "new") "New since last run" else status),
    if (!is.na(prev_priority) && !is.na(priority) && prev_priority != priority) span(class = "bf-chip chg", paste("was", prev_priority)))
  bf_btn <- function(id, value, label = "Evidence")
    tags$button(class = "btn btn-outline-secondary", onclick = sprintf("Shiny.setInputValue('%s', %s, {priority: 'event'})", id, value), label)
  output$brief <- renderUI({
    if (is.null(rv$sc)) return(div(class = "bf bf-empty",
      tags$h3("Your briefing appears here after a run"),
      p(style = "color:#5f6368", "Load the latest data export, check the settings and click Run. You then see what to act on this week, and - from the second run of the same survey - what is new, what persists and what was resolved."),
      actionButton("bf_start", "Load the data", class = "btn-primary")))
    b <- brief(); res <- rv$res; ds <- res$data_summary
    last <- suppressWarnings(max(as.Date(ds$last_day), na.rm = TRUE))
    cmp_txt <- if (!is.null(b$previous)) sprintf("Compared with the export of %s (%d earlier run%s kept)", bf_date(b$previous$data_last), b$runs, if (b$runs > 1) "s" else "")
               else if (!is.null(rv$cmp$saved)) "First run kept for this survey: the next run will show what is new, persisting and resolved"
               else "Run history is off on this server"
    kp <- b$kpi
    kpis <- lapply(seq_len(nrow(kp)), function(i) { k <- kp[i]
      d <- if (is.na(k$before)) NULL else { x <- k$now - k$before
        cls <- if (x == 0) "bf-same" else if ((x > 0) == (k$what != "resolved")) "bf-up" else "bf-down"
        span(class = paste("d", cls), if (x == 0) "same" else sprintf("%+d", x)) }
      div(class = "bf-kpi", div(class = "l", k$label), div(class = "v", k$now, d),
          if (k$what == "teams" && k$now > 0) div(class = "s", sprintf("%d high · %d medium", sum(b$teams$priority == "High"), sum(b$teams$priority == "Medium")))) })
    if (is.null(b$previous)) kpis[[4]] <- div(class = "bf-kpi", div(class = "l", "Teams in the survey"), div(class = "v", nrow(rv$sc$teams)))

    hi <- b$teams[priority == "High"]; md <- b$teams[priority != "High"]
    team_rows <- lapply(seq_len(nrow(hi)), function(i) { h <- hi[i]
      div(class = "bf-item",
        div(span(class = paste("bf-pill go", tolower(h$priority)), title = "Open the team page", onclick = sprintf("Shiny.setInputValue('go_team', %d, {priority: 'event'})", h$team),
                 sprintf("%s · %s", sub(" \\([^()]*\\)$", "", h$team_label), h$priority))),
        div(div(class = "bf-head", h$headline),
            div(class = "bf-sub", paste(c(if (h$more > 0) sprintf("+%d more team issue%s", h$more, if (h$more > 1) "s" else ""),
                                          if (nzchar(h$interviewers)) paste("interviewers", h$interviewers)), collapse = " · ")),
            div(class = "bf-do", icon("arrow-right"), sprintf(" %s (%s, %s)", h$do, h$owner, tolower(h$timing)))),
        div(class = "bf-acts", bf_chip(h$status, h$prev_priority, h$priority), bf_btn("bf_team", h$team)))
    })
    med_rows <- lapply(seq_len(nrow(md)), function(i) { h <- md[i]
      div(class = "bf-med",
        div(span(class = paste("bf-pill", tolower(h$priority)), sprintf("%s · %s", sub(" \\([^()]*\\)$", "", h$team_label), h$priority))),
        div(h$headline, if (h$more > 0) span(style = "color:#5f6368", sprintf(" (+%d more)", h$more))),
        div(class = "bf-acts", bf_chip(h$status, h$prev_priority, h$priority), bf_btn("bf_team", h$team)))
    })
    iv_rows <- lapply(seq_len(nrow(b$interviewers)), function(i) { h <- b$interviewers[i]
      div(class = "bf-item",
        div(span(class = "bf-pill iv go", title = "Open the interviewer page", onclick = sprintf("Shiny.setInputValue('go_iv', %s, {priority: 'event'})", fmt_i(h$interv)),
                 sprintf("Int. %s · %s", fmt_i(h$interv), sub(" \\([^()]*\\)$", "", h$team_label)))),
        div(div(class = "bf-head", h$headline), div(class = "bf-do", icon("arrow-right"), sprintf(" %s (%s, %s)", h$do, h$owner, tolower(h$timing)))),
        div(class = "bf-acts", bf_chip(h$status), bf_btn("bf_iv", h$interv)))
    })

    # team health grid
    g <- b$grid; ha <- .int$health_areas
    short <- c(flags = "Flags", duration = "Length", completion = "Compl.", hhm = "Listing", dk = "DK", births = "Births", anthropometry = "Anthro", fl = "FL")
    used <- names(ha)[names(ha) %in% g[!is.na(sev), harea] | names(ha) %in% c("flags", "duration", "completion")]
    tms <- unique(g[, .(team, team_label, priority)])
    click <- function(t) sprintf("Shiny.setInputValue('bf_team', %d, {priority: 'event'})", t)
    grid <- tags$table(class = "bf-grid",
      tags$tr(tags$th(""), lapply(used, function(a) tags$th(title = ha[[a]], short[[a]]))),
      lapply(seq_len(nrow(tms)), function(i) { t <- tms[i]
        tags$tr(tags$td(class = "t", onclick = click(t$team),
                        span(style = sprintf("display:inline-block;width:7px;height:7px;border-radius:50%%;margin-right:5px;background:%s", prio_col[[t$priority]])),
                        sub(" \\([^()]*\\)$", "", t$team_label)),
                lapply(used, function(a) { z <- g[team == t$team & harea == a]
                  if (!nrow(z) || is.na(z$sev)) return(tags$td(class = "c", title = sprintf("%s - %s: nothing found", t$team_label, ha[[a]])))
                  tags$td(class = paste("c", paste0("s", z$sev), if (isTRUE(z$persisting)) "p"), title = sprintf("%s - %s:\n%s", t$team_label, ha[[a]], z$what),
                          onclick = click(t$team), if (z$n > 1) z$n) }))
      }))

    q <- b$questionnaire
    qmods <- if (nrow(q)) q[, .(qs = paste(head(stats::na.omit(question), 4), collapse = ", "), n = .N), by = .(dataset, module)][order(-n)] else NULL
    res_rows <- if (nrow(b$resolved)) lapply(seq_len(min(nrow(b$resolved), 8)), function(i) { h <- b$resolved[i]
      div(class = "bf-line", span(class = "bf-pill", style = "background:#E7F5EC;color:#0B6B33",
                                  if (is.na(h$team)) "Survey" else if (h$level == "interviewer") sprintf("Int. %s", fmt_i(h$interv)) else sub(" \\([^()]*\\)$", "", h$team_label)),
          " ", span(style = "color:#5f6368", "no longer found: "), h$headline) })

    div(class = "bf",
      div(class = "bf-top",
        div(div(class = "bf-meta", sprintf("%s · data up to %s · period of analysis %s", res$cfg$survey_name %||% "Survey", bf_date(last), b$survey$period %||% "-")),
            div(class = "bf-title", "Briefing"), div(class = "bf-meta", cmp_txt)),
        div(downloadButton("bf_plan", "Supervisor briefs (Word)", class = "btn-sm btn-outline-primary"),
            actionButton("bf_more", "All results", class = "btn-sm btn-outline-secondary", icon = icon("table")))),
      div(class = "bf-kpis", kpis),
      div(class = "bf-cols",
        div(
          div(class = "bf-card", tags$h4("Teams to act on this week"),
              div(class = "hint", "High priority teams first, most urgent first. The arrow is the first thing to do; 'Evidence' shows every issue with its figures and action."),
              if (length(team_rows)) team_rows else if (!length(med_rows)) div(class = "bf-line", "No team stands out this week.")
              else div(class = "bf-line", "No High priority team this week."),
              if (length(med_rows)) tags$details(class = "bf-more", open = if (!length(team_rows)) NA,
                tags$summary(sprintf("%d Medium priority team%s - follow up at the next supervisor call", length(med_rows), if (length(med_rows) > 1) "s" else "")),
                med_rows)),
          div(class = "bf-card", tags$h4("Interviewers to observe now"),
              div(class = "hint", "Interviewers with at least one high-severity issue, compared with their team-mates or the survey."),
              if (length(iv_rows)) iv_rows else div(class = "bf-line", "No interviewer with a high-severity issue."))),
        div(
          div(class = "bf-card", tags$h4("Team health"),
              div(class = "hint", "Worst issue of each team in each area (team or interviewer level). Point at a cell for the details, click a team for the evidence."),
              grid,
              div(class = "bf-legend", span(style = "background:#E2231A"), "high", span(style = "background:#F6B26B"), "medium",
                  span(style = "background:#F3F4F6"), "nothing found",
                  if (!is.null(b$previous)) tagList(span(style = "outline:2px solid #1f2a44;outline-offset:-2px;background:white"), "also at the last run"))),
          div(class = "bf-card", tags$h4(sprintf("Questionnaire problems (%d)", nrow(q))),
              div(class = "hint", "Questions flagged in most teams: check wording, translation and CAPI routing, and send one note to all teams. Not counted against teams."),
              if (!is.null(qmods)) lapply(seq_len(min(nrow(qmods), 6)), function(i) div(class = "bf-line", tags$b(sprintf("%s %s", qmods$dataset[i], qmods$module[i])), " ",
                                                                                     if (nzchar(qmods$qs[i])) qmods$qs[i] else "whole module"))
              else div(class = "bf-line", "None: no question is flagged in most teams.")),
          if (!is.null(b$previous)) div(class = "bf-card", tags$h4(sprintf("Resolved since the last run (%d)", nrow(b$resolved))),
              if (length(res_rows)) res_rows else div(class = "bf-line", "Nothing resolved yet."),
              if (nrow(b$resolved) > 8) div(class = "bf-line", sprintf("... and %d more.", nrow(b$resolved) - 8))),
          div(class = "bf-meta", style = "margin-top:-6px",
              if (!is.null(rv$cmp$saved)) tagList("The list of issues of each run (codes and figures, no records) is kept to compare runs. ",
                                                  actionLink("bf_forget", "Forget this survey's history"))))))
  })
  observeEvent(input$bf_start, nav_to("data"))
  observeEvent(input$bf_more, { nav_to("results"); bslib::nav_select("restabs", "scorecard") })
  bf_evidence <- function(rows, title, page = NULL) {
    showModal(modalDialog(title = title, size = "l", easyClose = TRUE, footer = tagList(page, modalButton("Close")),
      lapply(seq_len(nrow(rows)), function(i) ev_item(rows[i]))))
  }
  bf_issues <- reactive({ req(rv$sc); c <- rv$cmp %||% ipm_compare(rv$sc, list(), as.Date(NA)); s <- copy(c$issues)
    s[, sev := match(severity, c("high", "medium"))]; s[, lev := match(level, c("team", "interviewer"))]; setorder(s, lev, sev); s })
  observeEvent(input$bf_team, { s <- bf_issues()[level != "survey" & team == input$bf_team]
    t <- rv$sc$teams[team == input$bf_team]
    bf_evidence(s, sprintf("%s - %s priority: %d issue%s", t$team_label, t$priority, nrow(s), if (nrow(s) == 1) "" else "s"),
                tags$button(class = "btn btn-primary", onclick = sprintf("Shiny.setInputValue('go_team', %d, {priority: 'event'})", t$team), "Open the team page")) })
  observeEvent(input$bf_iv, { s <- bf_issues()[level == "interviewer" & interv == input$bf_iv]
    bf_evidence(s, sprintf("Interviewer %s (%s)", fmt_i(input$bf_iv), s$team_label[1]),
                tags$button(class = "btn btn-primary", onclick = sprintf("Shiny.setInputValue('go_iv', %s, {priority: 'event'})", fmt_i(input$bf_iv)), "Open the interviewer page")) })
  observeEvent(input$bf_forget, showModal(modalDialog(title = "Forget this survey's history?",
    "The issues kept from earlier runs of this survey will be deleted; the next run starts a new history. Your current results stay.",
    footer = tagList(modalButton("Cancel"), actionButton("bf_forget_ok", "Forget the history", class = "btn-danger")))))
  observeEvent(input$bf_forget_ok, { removeModal(); req(rv$res); ipm_history_forget(rv$res); rv$cmp <- ipm_compare(rv$sc, list(), as.Date(NA))
    showNotification("History deleted.", type = "message") })
  output$bf_plan <- downloadHandler(filename = function() sprintf("supervisor_briefs_%s.docx", format(Sys.Date(), "%Y%m%d")),
    content = function(f) { req(rv$sc); ipm_supervisor_brief(rv$res, rv$sc, f, cmp = rv$cmp) })


  # ---------- team and interviewer pages ----------
  short_lbl <- function(x) sub(" \\([^()]*\\)$", "", x)
  fmt_n <- function(x) ifelse(is.na(x), "-", ifelse(abs(x) >= 10, sprintf("%.0f", x), sprintf("%.1f", x)))
  # value vs benchmark as two small bars
  ev_bars <- function(v, bm, lv = "This", lb = "Comparison") {
    if (is.na(v) || is.na(bm) || max(abs(c(v, bm))) == 0) return(NULL)
    m <- max(abs(c(v, bm)))
    row <- function(l, x, col) div(class = "evb", span(class = "l", l), div(class = "t", div(class = "f", style = sprintf("width:%.0f%%;background:%s", 100 * abs(x) / m, col))), span(class = "n", fmt_n(x)))
    div(class = "evbars", row(lv, v, "#1CABE2"), row(lb, bm, "#B8BEC6"))
  }
  ev_item <- function(h) {
    st <- if (is.na(h$status)) NA else if (h$status == "persisting") sprintf("%s run in a row (since %s)", bf_nth(h$runs), format(h$since, "%d %b")) else h$status
    div(class = "bf-ev",
      div(span(class = paste("bf-pill", h$severity), h$severity), " ",
          if (h$level == "interviewer") span(class = "bf-pill iv", style = "cursor:pointer", onclick = sprintf("Shiny.setInputValue('go_iv', %s, {priority: 'event'})", fmt_i(h$interv)),
                                             paste("Int.", fmt_i(h$interv))), " ",
          tags$b(h$headline), " ", bf_chip(st)),
      ev_bars(h$value, h$benchmark, if (h$level == "interviewer") "Interviewer" else "Team", if (h$type %in% c("heaping")) "Expected" else if (h$level == "interviewer") "Team / survey" else "Others / survey"),
      div(class = "e", h$evidence),
      if (!is.na(h$was) && identical(h$status, "persisting") && h$was != h$headline) div(class = "e", style = "color:#7A5300", "Last run: ", h$was),
      div(class = "a", icon("arrow-right"), sprintf(" %s (%s, %s)", h$action, h$owner, tolower(h$timing))))
  }
  go_team <- function(t) { updateSelectInput(session, "tp_team", selected = t); nav_to("team"); removeModal() }
  go_iv <- function(i) { updateSelectizeInput(session, "ip_iv", selected = i); nav_to("interv"); removeModal() }
  observeEvent(input$go_team, go_team(input$go_team))
  observeEvent(input$go_iv, go_iv(input$go_iv))

  all_ivs <- reactive({ req(rv$sc, rv$res); sc <- rv$sc; o <- rv$res$outputs
    x <- unique(rbindlist(list(if (!is.null(sc$interviewers)) sc$interviewers[, .(team, interv)], if (!is.null(o$quesmtimes$data)) o$quesmtimes$data[, .(team, interv)],
                               if (!is.null(o$quescrates$data)) o$quescrates$data[, .(team, interv)])))[!is.na(team) & !is.na(interv)]
    x <- x[, .(team = team[1]), by = interv]
    s <- sc$signals[level == "interviewer", .(n = .N, high = sum(severity == "high")), by = interv]
    x[, `:=`(n = s$n[match(interv, s$interv)], high = s$high[match(interv, s$interv)], team_label = sc$teams$team_label[match(team, sc$teams$team)])]
    x[is.na(n), `:=`(n = 0L, high = 0L)]
    x[, r := fifelse(high > 0, 1L, fifelse(n > 0, 2L, 3L))]
    setorder(x, r, team, interv)[] })
  observe({
    sc <- rv$sc
    if (is.null(sc)) { updateSelectInput(session, "tp_team", choices = character()); updateSelectizeInput(session, "ip_iv", choices = character()); return() }
    tc <- stats::setNames(as.character(sc$teams$team), sprintf("%s · %s priority", sc$teams$team_label, sc$teams$priority))
    updateSelectInput(session, "tp_team", choices = tc, selected = if (isTRUE(isolate(input$tp_team) %in% tc)) isolate(input$tp_team) else tc[1])
    a <- all_ivs()
    ic <- stats::setNames(fmt_i(a$interv), sprintf("%s · %s%s", fmt_i(a$interv), short_lbl(a$team_label),
                                                   ifelse(a$high > 0, sprintf(" · %d high issue%s", a$high, ifelse(a$high > 1, "s", "")), ifelse(a$n > 0, sprintf(" · %d issue%s", a$n, ifelse(a$n > 1, "s", "")), ""))))
    updateSelectizeInput(session, "ip_iv", choices = ic, selected = if (isTRUE(isolate(input$ip_iv) %in% ic)) isolate(input$ip_iv) else ic[1], server = FALSE)
  })

  tp_prof <- reactive({ req(rv$sc, input$tp_team); tryCatch(ipm_team_profile(rv$res, rv$sc, input$tp_team, rv$cmp), error = function(e) NULL) })
  ip_prof <- reactive({ req(rv$sc, input$ip_iv); tryCatch(ipm_interviewer_profile(rv$res, rv$sc, input$ip_iv, rv$cmp), error = function(e) NULL) })
  no_run <- function() div(class = "bf bf-empty", tags$h3("This page fills in after a run"), actionButton("bf_start2", "Load the data", class = "btn-primary"))
  observeEvent(input$bf_start2, nav_to("data"))
  trend_card <- function(prefix, p) {
    if (!length(p$trends)) return(NULL)
    div(class = "bf-card", tags$h4("Week by week"),
        div(class = "hint", sprintf("Whole fieldwork; questionnaire %s for interview length and completion.", p$questionnaire %||% "-")),
        lapply(names(p$trends), function(k) div(div(class = "tp-ct", .int$trend_titles[[k]] %||% k), plotOutput(paste0(prefix, k), height = "170px"))))
  }
  for (k in c("minutes", "completion", "under5")) local({ k <- k
    output[[paste0("tp_plot_", k)]] <- renderPlot({ p <- tp_prof(); req(p, p$trends[[k]]); ipm_trend_plot(p$trends[[k]]) }, res = 96)
    output[[paste0("ip_plot_", k)]] <- renderPlot({ p <- ip_prof(); req(p, p$trends[[k]]); ipm_trend_plot(p$trends[[k]]) }, res = 96)
  })
  mod_bars <- function(m, lv, lb) {
    if (is.null(m) || !nrow(m)) return(div(class = "bf-line", "No module stands out."))
    lapply(seq_len(nrow(m)), function(i) { h <- m[i]
      div(class = "bf-line", tags$b(sprintf("%s %s", h$dataset, h$module)), span(style = "color:#5f6368", sprintf("  %d of %d tests flagged%s", h$flags, h$tests,
            if (!is.null(h$questions) && nzchar(h$questions %||% "")) paste0(" - ", h$questions) else if (!is.null(h$interviewers_flagged)) paste0(" - interviewers ", h$interviewers_flagged) else "")),
          ev_bars(h$rate, h$module_rate, lv, lb)) })
  }

  output$tp_body <- renderUI({
    if (is.null(rv$sc)) return(no_run())
    p <- tp_prof(); req(p); t <- p$team; s <- p$issues; iv <- p$interviewers; sv <- p$survey
    kpi <- function(l, v, sub = NULL) div(class = "bf-kpi", div(class = "l", l), div(class = "v", v), if (!is.null(sub)) div(class = "s", sub))
    ivt <- tags$table(class = "tp-tbl",
      tags$tr(tags$th("Interviewer"), tags$th("Issues"), tags$th("Flags"), tags$th(sprintf("Length (%s)", p$questionnaire)), tags$th("Completion"), tags$th("Main issue")),
      lapply(seq_len(nrow(iv)), function(i) { h <- iv[i]
        tags$tr(onclick = sprintf("Shiny.setInputValue('go_iv', %s, {priority: 'event'})", fmt_i(h$interv)),
          tags$td(tags$b(fmt_i(h$interv))),
          tags$td(if (h$high > 0) span(class = "bf-pill high", h$issues) else if (h$issues > 0) span(class = "bf-pill medium", h$issues) else span(style = "color:#9aa0a6", "-")),
          tags$td(if (is.na(h$flag_rate)) "-" else sprintf("%.1f%%", h$flag_rate)), tags$td(if (is.na(h$minutes)) "-" else sprintf("%.0f min", h$minutes)),
          tags$td(if (is.na(h$completion)) "-" else sprintf("%.0f%%", h$completion)), tags$td(class = "m", h$top %||% "")) }))
    div(class = "bf",
      div(class = "bf-kpis",
          kpi("Priority", span(class = paste("bf-pill", tolower(t$priority)), style = "font-size:0.5em;vertical-align:middle", t$priority), sprintf("%s warning points", fmt_n(t$points))),
          kpi("Issues", nrow(s), sprintf("%d high · %d team · %d interviewer", sum(s$severity == "high"), sum(s$level == "team"), sum(s$level == "interviewer"))),
          kpi("Questions flagged", if (is.na(t$rate_excl)) "-" else sprintf("%.1f%%", t$rate_excl), if (!is.null(sv$flag_rate_excl)) sprintf("survey %.1f%% (questionnaire problems left out)", sv$flag_rate_excl)),
          kpi("Interviewers", nrow(iv), sprintf("%d with an issue", sum(iv$issues > 0)))),
      div(class = "bf-cols",
        div(class = "bf-card", tags$h4(sprintf("Issues (%d)", nrow(s))),
                div(class = "hint", "Team issues first, then interviewers; the bars compare the team or interviewer with its benchmark. Click an interviewer for their page."),
                if (nrow(s)) lapply(seq_len(nrow(s)), function(i) ev_item(s[i])) else div(class = "bf-line", "No issue found for this team.")),
        div(div(class = "bf-card", tags$h4("Interviewers"), div(class = "hint", sprintf("Flags: share of questions flagged (period of analysis). Length and completion: last %d weeks. Click a row for the interviewer's page.", p$recent_weeks)), ivt),
            trend_card("tp_plot_", p),
            div(class = "bf-card", tags$h4("Modules where the team stands out"),
                div(class = "hint", "Share of the team's tests flagged (blue) vs the other teams (grey); questions flagged in most teams left out."),
                mod_bars(p$modules, "Team", "Others")))))
  })
  output$tp_brief <- downloadHandler(filename = function() sprintf("supervisor_brief_T%s_%s.docx", input$tp_team, format(Sys.Date(), "%Y%m%d")),
    content = function(f) { req(rv$sc, input$tp_team); ipm_supervisor_brief(rv$res, rv$sc, f, teams = as.integer(input$tp_team), cmp = rv$cmp) })

  output$ip_body <- renderUI({
    if (is.null(rv$sc)) return(no_run())
    p <- ip_prof(); req(p); h <- p$interviewer; s <- p$issues; fg <- p$figures
    kpi <- function(l, v, sub = NULL) div(class = "bf-kpi", div(class = "l", l), div(class = "v", v), if (!is.null(sub)) div(class = "s", sub))
    figt <- if (!is.null(fg) && nrow(fg)) tags$table(class = "tp-tbl",
      tags$tr(tags$th(""), tags$th("Questionnaire"), tags$th("Interviewer"), tags$th("Team"), tags$th("Survey"), tags$th("Interviews")),
      lapply(seq_len(nrow(fg)), function(i) { x <- fg[i]; u <- if (x$what == "minutes") " min" else "%"
        low <- !is.na(x$interviewer) && !is.na(x$survey) && x$interviewer < (if (x$what == "minutes") 0.6 else 0.9) * x$survey
        tags$tr(tags$td(if (x$what == "minutes") "Length" else "Completion"), tags$td(x$dataset),
                tags$td(style = if (low) "color:#C4161C;font-weight:600", paste0(fmt_n(x$interviewer), u)), tags$td(paste0(fmt_n(x$team), u)), tags$td(paste0(fmt_n(x$survey), u)), tags$td(x$n)) }))
    div(class = "bf",
      div(class = "bf-kpis",
          kpi("Team", actionLink("ip_team", short_lbl(h$team_label)), "open the team page"),
          kpi("Issues", nrow(s), sprintf("%d high", sum(s$severity == "high"))),
          kpi("Questions flagged", if (is.na(h$flag_rate)) "-" else sprintf("%.1f%%", h$flag_rate),
              sprintf("team %s · survey %s", if (is.na(h$team_rate)) "-" else sprintf("%.1f%%", h$team_rate), if (is.na(h$survey_rate)) "-" else sprintf("%.1f%%", h$survey_rate))),
          kpi("Tests", if (is.na(h$tests)) "-" else format(h$tests, big.mark = ","), "flag tests in the period of analysis")),
      div(class = "bf-cols",
        div(div(class = "bf-card", tags$h4(sprintf("Issues (%d)", nrow(s))),
                if (nrow(s)) lapply(seq_len(nrow(s)), function(i) ev_item(s[i])) else div(class = "bf-line", "No issue found for this interviewer.")),
            div(class = "bf-card", tags$h4("Length and completion"), div(class = "hint", sprintf("Last %d weeks; red when well below the survey.", p$recent_weeks)),
                figt %||% div(class = "bf-line", "No interview times or results found."))),
        div(trend_card("ip_plot_", p),
            div(class = "bf-card", tags$h4("Modules with the most flags"),
                div(class = "hint", "Share of the interviewer's tests flagged (blue) vs all interviewers (grey); questions flagged in most teams left out."),
                mod_bars(p$modules, "Interviewer", "All")))))
  })
  observeEvent(input$ip_team, { p <- ip_prof(); req(p); go_team(p$interviewer$team) })

  # ---------- results: summary ----------
  output$kpis <- renderUI({
    res <- rv$res; req(res); fa <- res$outputs$flaganal
    k <- function(v, l) div(style = "display:inline-block; margin-right:40px", div(class = "bignum", v), div(class = "small-note", l))
    div(k(format(sum(res$data_summary$records), big.mark = ","), "records analysed"),
        k(sum(!is.na(unique(res$team_info$team))), "teams defined"),
        if (!is.null(fa$tests)) k(format(nrow(fa$tests), big.mark = ","), "flag tests"),
        if (!is.null(fa$tests)) k(sprintf("%.1f%%", mean(fa$tests$flag) * 100), "of tests flagged (nominal level 0.1%)"),
        k(sprintf("%s - %s", fmt_d(min(as.Date(res$data_summary$first_day), na.rm = TRUE)), fmt_d(max(as.Date(res$data_summary$last_day), na.rm = TRUE))), "fieldwork: first - last interview"),
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
        k(nrow(sc$signals[level == "survey"]), "questionnaire problems"), k(nrow(sc$signals[level == "team"]), "whole-team issues"), k(nrow(sc$signals[level == "interviewer"]), "interviewer issues"))
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
    DT::datatable(s[, .(Level = c(survey = "Questionnaire", team = "Team", interviewer = "Interviewer")[level], Team = team_label, Interviewer = ifelse(is.na(interv), "", fmt_i(interv)), Sign = gsub("_", " ", type), Severity = severity,
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
    if (nzchar(m) && identical(m, rv$stale_model) && !m %in% c(c0$model, rv$listed)) m <- ""   # a model name left from the previous connection is never used
    ipm_ai_settings(c0$provider, if (nzchar(m)) m else c0$model, c0$api_key, c0$base_url, c0$label)
  })
  list_models <- function(quiet = FALSE) {
    c0 <- conn(); if (is.null(c0)) return(invisible())
    ms <- tryCatch(ipm_ai_models(c0), error = function(e) { if (!quiet) showNotification(conditionMessage(e), type = "error", duration = 10); NULL })
    if (length(ms)) {
      rv$listed <- ms
      cur <- isolate(input$ai_model) %||% c0$model
      if (identical(cur, rv$stale_model) && !cur %in% c(ms, c0$model)) cur <- ""
      sel <- if (nzchar(cur %||% "") && cur %in% ms) cur else if (nzchar(c0$model) && c0$model %in% ms) c0$model else ms[1]
      updateSelectizeInput(session, "ai_model", choices = unique(c(ms, cur[nzchar(cur %||% "")])), selected = sel)
      if (!quiet) showNotification(sprintf("%d model(s) available.", length(ms)), type = "message")
    } else if (!quiet && !is.null(ms)) showNotification("The endpoint returned no model. On Ollama: ollama pull <model>.", type = "warning")
  }
  observeEvent(conn(), {
    c0 <- conn(); rv$ai_test <- NULL; rv$stale_model <- isolate(input$ai_model); rv$listed <- character()
    updateCheckboxInput(session, "ai_compact", value = c0$provider %in% c("ollama", "openai"))
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
    start_job("ai_test", "test", list(st = st), "Testing the model", if (st$provider == "ollama") "the first call loads the model: up to a minute" else NULL,
              done = function(r) rv$ai_test <- if (inherits(r, "error")) list(ok = FALSE, message = job_error(r)) else r)
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
    st <- ai_st(); q <- trimws(input$ai_q %||% "")
    cmp <- isTRUE(input$ai_compact)
    rv$prev <- list(plan = ipm_ai_preview(rv$res, rv$sc, st, "plan", language = input$ai_lang, compact = cmp),
                    question = ipm_ai_preview(rv$res, rv$sc, st, "question", question = if (nzchar(q)) q else "Which team should the coordinator visit first, and why?",
                                              history = rv$chat, language = input$ai_lang, compact = cmp))
    one <- function(pv, id, what) {
      ctx <- pv$body$options$num_ctx
      tagList(
        tags$table(class = "table table-sm", style = "width:auto; font-size:0.86em",
          tags$tr(tags$td(tags$b("Sent to")), tags$td(sprintf("POST %s", pv$url))),
          tags$tr(tags$td(tags$b("Model")), tags$td(pv$model)),
          if (length(pv$headers)) tags$tr(tags$td(tags$b("Headers")), tags$td(paste(names(pv$headers), pv$headers, sep = ": ", collapse = " | "))),
          tags$tr(tags$td(tags$b("Size")), tags$td(sprintf("%s characters, about %s tokens: instructions %s, evidence + request %s, answer format %s characters",
                                                              format(pv$chars, big.mark = ","), format(pv$tokens_estimate, big.mark = ","), format(pv$parts[["instructions"]], big.mark = ","),
                                                              format(pv$parts[["evidence_and_request"]], big.mark = ","), format(pv$parts[["answer_format"]], big.mark = ",")))),
          tags$tr(tags$td(tags$b("Answer limit")), tags$td(sprintf("%s tokens%s", format(pv$max_output_tokens, big.mark = ","),
                                                                  if (!is.null(ctx)) sprintf("; Ollama context window requested: %s tokens (input + answer)", format(ctx, big.mark = ",")) else "")))),
        p(class = "small-note", what),
        downloadButton(id, "Download this request (JSON)", class = "btn-sm"),
        tags$pre(class = "json", style = "margin-top:8px", pv$json))
    }
    showModal(modalDialog(title = "Exactly what is sent to the AI model", size = "xl", easyClose = TRUE,
      p(class = "small-note", "This is the full request, built by the same code that sends it: the instructions (system prompt), the evidence (aggregated statistics only - no record-level data, household identifiers or names), the request and, for the plan, the required answer format. The API key is sent in a header and is never shown."),
      navset_tab(
        nav_panel("Action plan request", one(rv$prev$plan, "prev_dl_plan", "Sent when you click 'Generate AI action plan'.")),
        nav_panel("Question request", one(rv$prev$question, "prev_dl_q",
          "Sent when you click 'Ask', here with the question in the box (or an example). Naming a team adds its flag tables; the last 4 questions and answers of the session are included."))),
      footer = modalButton("Close")))
  })
  output$prev_dl_plan <- downloadHandler(filename = function() "ipm_ai_request_plan.json", content = function(f) writeLines(rv$prev$plan$json, f))
  output$prev_dl_q <- downloadHandler(filename = function() "ipm_ai_request_question.json", content = function(f) writeLines(rv$prev$question$json, f))
  observeEvent(input$ai_go, {
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (tab 6. Run): the AI works from its results.", type = "warning", duration = 8); return() }
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model ready: choose a connection and a model, or use the rule-based plan.", type = "warning"); return() }
    start_job("ai_plan", "plan", list(res = rv$res, sc = rv$sc, st = st, language = input$ai_lang, compact = isTRUE(input$ai_compact)),
      sprintf("%s is writing the action plan", st$model), if (st$provider == "ollama") "local models can take several minutes" else "30 seconds to 2 minutes",
      done = function(p) {
        if (inherits(p, "error")) { showNotification(job_error(p), type = "error", duration = NULL); return() }
        rv$plan <- p; r <- rv$res; r$action_plan <- p; rv$res <- r })
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
    start_job("ai_ask", "ask", list(q = q, res = rv$res, sc = rv$sc, st = st, history = rv$chat, language = input$ai_lang, compact = isTRUE(input$ai_compact)),
      sprintf("%s is answering", st$model), if (st$provider == "ollama") "local models re-read the whole evidence: 1-5 minutes" else NULL,
      done = function(a) {
        if (inherits(a, "error")) { showNotification(job_error(a), type = "error", duration = NULL); return() }
        rv$chat <- c(rv$chat, list(list(q = q, a = a$answer, team = a$team))); updateTextAreaInput(session, "ai_q", value = "") })
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
          out[[nm]] <- list(plot = z$plot, how = howto[[paste0("flag_", z$type)]], type = z$type,
                            group = if (z$type == "summary") "Flags | summary" else paste("Flags |", z$dataset), dataset = z$dataset) } }
      else for (s in r$sections) for (j in seq_along(s$plots))
        out[[sprintf("%s | %s%s%s", output_names[[id]], s$dataset %||% "", if (!is.null(s$group)) paste0(" - ", hhm_defs[[s$group]]$label) else if (!is.null(s$label)) paste0(" - ", s$label) else "", if (length(s$plots) > 1) paste0(" (page ", j, ")") else "")]] <-
          list(plot = s$plots[[j]], how = howto[[id]], type = id, group = output_names[[id]])
    }
    out
  })
  output$fig_select <- renderUI(selectInput("fig_pick", NULL, names(figs()), width = "100%"))
  output$fig_howto <- renderUI({ req(input$fig_pick); f <- figs()[[input$fig_pick]]; req(f); div(class = "howto", tags$b("How to read: "), f$how) })
  output$fig <- renderPlot({ req(input$fig_pick); f <- figs()[[input$fig_pick]]; req(f); f$plot }, res = 96)   # returned, not printed: Shiny then knows the ggplot axes (hover)
  fig_file <- function(f, file, type) {
    g <- figs()[[input$fig_pick]]$plot; req(g)
    if (type == "svg") {
      if (requireNamespace("svglite", quietly = TRUE)) svglite::svglite(file, width = 13, height = 8.3)
      else if (isTRUE(capabilities("cairo"))) grDevices::svg(file, width = 13, height = 8.3)
      else { showNotification("SVG is not available on this server (needs the svglite package or cairo): use the PNG download.", type = "warning", duration = 10); req(FALSE) } }
    else if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(file, width = 13, height = 8.3, units = "in", res = 220) else grDevices::png(file, width = 13, height = 8.3, units = "in", res = 220)
    on.exit(grDevices::dev.off()); print(g)
  }
  fig_fname <- function(ext) function() sprintf("ipm_%s.%s", gsub("[^A-Za-z0-9]+", "_", substr(input$fig_pick %||% "figure", 1, 60)), ext)
  output$fig_svg <- downloadHandler(filename = fig_fname("svg"), content = function(file) fig_file(NULL, file, "svg"))
  output$fig_png <- downloadHandler(filename = fig_fname("png"), content = function(file) fig_file(NULL, file, "png"))
  # all graphs of the same output: high-resolution images on one scrolling page
  output$fig_all <- renderUI({
    req(input$fig_pick, identical(input$fig_mode, "all")); fs <- figs(); f <- fs[[input$fig_pick]]; req(f)
    nm <- names(fs)[vapply(fs, function(z) identical(z$group, f$group), logical(1))]
    shown <- head(nm, 40)
    imgs <- withProgress(message = "Drawing the graphs", value = 0, lapply(shown, function(n) {
      incProgress(1 / length(shown), detail = n)
      tf <- tempfile(tmpdir = sdir, fileext = ".png"); on.exit(unlink(tf))
      if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(tf, width = 12, height = 7.7, units = "in", res = 160) else grDevices::png(tf, width = 12, height = 7.7, units = "in", res = 160)
      print(fs[[n]]$plot); grDevices::dev.off()
      div(style = "margin:10px 0 26px 0", tags$h6(style = "color:#1CABE2; font-weight:bold", n),
          tags$img(src = paste0("data:image/png;base64,", jsonlite::base64_enc(readBin(tf, "raw", file.info(tf)$size))), style = "width:100%; height:auto"))
    }))
    tagList(p(class = "small-note", sprintf("%d graph(s) of '%s'%s.", length(nm), f$group, if (length(nm) > 40) " - first 40 shown" else "")), imgs)
  })
  # flags by module: the flag table behind a circle
  fig_pt <- reactiveVal(NULL)
  observeEvent(input$fig_pick, fig_pt(NULL))
  pick_point <- function(ev) {
    f <- figs()[[input$fig_pick]]; if (is.null(f) || !identical(f$type, "module") || is.null(ev)) return()
    d <- f$plot$data; d <- d[d$flag == 1, ]
    np <- tryCatch(nearPoints(d, ev, xvar = "x", yvar = "yf", threshold = 10, maxpoints = 1), error = function(e) NULL)
    if (!is.null(np) && nrow(np)) fig_pt(np)
  }
  observeEvent(input$fig_hover, pick_point(input$fig_hover))
  observeEvent(input$fig_click, pick_point(input$fig_click))
  output$fig_point <- renderUI({
    f <- figs()[[input$fig_pick %||% ""]]
    if (is.null(f) || !identical(f$type, "module")) return(NULL)
    np <- fig_pt(); if (is.null(np)) return(p(class = "small-note", "Point at (or tap) a circle to see the flag table of that interviewer and question."))
    tb <- Filter(function(z) z$dataset == np$dataset[1] && z$variable == np$varname[1] && z$interv == np$interv[1] && z$team == np$team[1], rv$res$outputs$flaganal$tables)
    if (!length(tb)) return(p(class = "small-note", sprintf("Interviewer %s, question %s: flag table not available.", fmt_i(np$interv[1]), np$varname[1])))
    z <- tb[[1]]
    card(card_header(z$title), p(class = "small-note", z$desc), div(class = "pattern", z$pattern), renderTable(z$table, digits = 0))
  })

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
    req(length(tb) > 0); z <- tb[[1]]
    card(card_header(z$title), p(class = "small-note", z$desc), div(class = "pattern", z$pattern), renderTable(z$table, digits = 0))
  })

  # ---------- results: flag tables by team ----------
  df_html <- function(df) {
    tags$table(class = "table table-sm table-striped", style = "width:auto !important; min-width:420px; font-size:0.88em",
      tags$thead(tags$tr(lapply(seq_along(df), function(j) { n <- names(df)[j]; tags$th(n, style = paste0(if (endsWith(n, "*")) "background:#374EA2;" else "background:#1CABE2;", " color:white; padding:2px 14px;", if (j > 1) " text-align:right" else "")) }))),
      tags$tbody(lapply(seq_len(nrow(df)), function(i) tags$tr(lapply(seq_along(df), function(j) tags$td(as.character(df[i, j]), style = if (j > 1) "text-align:right; padding:2px 14px" else "padding:2px 14px")))))) }
  fgroups <- reactive({ t <- rv$res$outputs$flaganal$tables; req(length(t) > 0); group_flag_tables(t) })
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
    g <- tt_sel(); req(length(g) > 0); shown <- head(g, 80)
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
    before <- if (!BG_OK) list.files(tempdir(), full.names = TRUE)
    rv$report <- NULL; rv$zip <- NULL
    start_job("report", "report", list(res = res, dir = res$cfg$results_dir, zip = file.path(sdir, "IPM_results.zip")), "Building the report",
      if (BG_OK) "you can keep using the app while it is built" else NULL, step = 1 / 16, hold = "^Writing (flag tables|report: flag graphs)",
      done = function(r) {
        if (!is.null(before)) { new <- setdiff(list.files(tempdir(), full.names = TRUE), before); wipe(new[!grepl("^ipm_", basename(new))]) }
        if (inherits(r, "error")) { showNotification(job_error(r), type = "error", duration = NULL); return() }
        rv$report <- r$report; rv$zip <- r$zip
        showNotification("Report ready - use the download buttons.", type = "message", duration = 8) })
  })
  output$dl_report <- downloadHandler(filename = function() sprintf("IPM_report_%s.docx", format(Sys.Date(), "%Y%m%d")),
                                      content = function(f) { req(rv$report); file.copy(rv$report, f) })
  output$dl_zip <- downloadHandler(filename = function() sprintf("IPM_results_%s.zip", format(Sys.Date(), "%Y%m%d")),
                                   content = function(f) { req(rv$zip); file.copy(rv$zip, f) })
}
