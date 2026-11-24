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

explain <- function(what, todo) tags$details(class = "ipm-explain",
  tags$summary(icon("circle-info"), " About this page"),
  div(style = "margin-top:6px", tags$b("What it does. "), what), div(style = "margin-top:4px", tags$b("What you need to do. "), todo))

# advanced analysis settings: one input per option of ipm_param_spec(), id "o_<option>", with its help as a tooltip
PSPEC <- ipm_param_spec()
adv_inputs <- function(group) {
  d <- ipm_defaults()
  tagList(lapply(names(PSPEC)[vapply(PSPEC, `[[`, "", "group") == group], function(n) { s <- PSPEC[[n]]; id <- paste0("o_", n)
    lab <- tagList(s$label, " ", tooltip(icon("circle-info", class = "su-i"), s$help))
    div(class = "su-adv-row", switch(s$type,
      lgl = checkboxInput(id, lab, isTRUE(d[[n]])),
      select = selectInput(id, lab, s$choices, d[[n]], width = "100%"),
      numericInput(id, lab, d[[n]], min = s$min, max = s$max, step = s$step, width = "100%"))) }))
}
adv_read <- function(input) {                       # current values of the advanced inputs (NULL when not shown yet)
  o <- lapply(names(PSPEC), function(n) { v <- input[[paste0("o_", n)]]; s <- PSPEC[[n]]
    if (is.null(v) || (is.numeric(v) && is.na(v))) return(NULL)
    if (s$type == "int") as.integer(v) else v })
  names(o) <- names(PSPEC); o[!vapply(o, is.null, logical(1))]
}

# link to the Data explorer with a variable chosen
ex_link <- function(ds, var, text = NULL)
  tags$a(href = "#", class = "ex-link", title = "Open in Data explorer: distribution of this question by team and interviewer",
         style = "margin-left:6px; font-size:0.85em", onclick = sprintf("event.stopPropagation(); Shiny.setInputValue('ex_open', {ds: '%s', var: '%s'}, {priority: 'event'}); return false;", ds, var),
         icon("magnifying-glass-chart"), text)
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
prio_col <- c(High = "#E2231A", Medium = "#F28C28", Low = "#8A8A8A")   # Low = grey: nothing major found, not "fine"
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
      rep <- ipm_report(args$res, args$dir, progress = prog, sections = args$sections, files = args$files)
      prog("Zipping all results"); unlink(args$zip)
      old <- setwd(args$dir); on.exit(setwd(old), add = TRUE)
      utils::zip(args$zip, list.files(".", recursive = TRUE), flags = "-r9Xq")
      list(report = rep, zip = args$zip) },
    plan = ipm_ai_plan(args$res, args$sc, args$st, language = args$language, compact = args$compact, priorities = args$priorities),
    ask = ipm_ai_ask(args$q, args$res, args$sc, args$st, history = args$history, language = args$language, compact = args$compact),
    read = ipm_ai_read(args$title, args$x, args$how, args$st, language = args$language),
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
  title = tags$span(tags$b("IPM"), tags$span(class = "ttl-long", " Interviewer Performance Monitoring for MICS"),
                    tags$span(style = "font-size:0.7em; opacity:0.85; margin-left:10px", paste0("ipmR ", .int$version %||% ""))),
  theme = theme, id = "nav", fillable = FALSE, bg = cyan, inverse = TRUE, selected = "setup",
  header = tags$head(tags$script(HTML(idle_js)), tags$script(HTML("
    $(document).on('shiny:connected', function(){ Shiny.addCustomMessageHandler('su_scroll', function(v){
      setTimeout(function(){ var el = document.querySelector('#rev [data-value=\"' + v + '\"]'); if (el) el.scrollIntoView({behavior: 'smooth', block: 'start'}); }, 400); }); });")), tags$style(HTML("
    .navbar .navbar-brand, .navbar .nav-link {color:white !important}
    .navbar .nav-link.active {border-bottom:3px solid white}
    .ipm-explain {border-left:4px solid #1CABE2; background:#F2FAFD; padding:6px 14px; margin:6px 0 14px 0; font-size:0.9em; border-radius:0 8px 8px 0}
    .ipm-explain summary {cursor:pointer; color:#0b5a75; font-weight:600}
    .su {max-width:1280px; margin:0 auto; padding:4px 4px 90px 4px}
    .su-top {display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap; margin:10px 0 14px 0}
    .su-steps {display:flex; gap:8px; flex-wrap:wrap; align-items:center}
    .su-step {display:flex; align-items:center; gap:8px; padding:6px 14px 6px 6px; border-radius:22px; background:#F3F4F6; color:#5f6368; cursor:pointer; font-size:0.92em; border:1px solid transparent}
    .su-step .n {display:inline-flex; width:26px; height:26px; border-radius:50%; align-items:center; justify-content:center; background:white; font-weight:600; color:#5f6368}
    .su-step.on {background:#E6F4FB; color:#0b5a75; border-color:#1CABE2} .su-step.on .n {background:#1CABE2; color:white}
    .su-step.ok .n {background:#00833D; color:white} .su-step.off {opacity:0.55; cursor:default}
    .su-sep {width:22px; height:2px; background:#d9dce1}
    .su-adv .badge {margin-left:6px}
    .su-t {font-weight:600}
    .su-chip {display:inline-block; font-size:0.74em; font-weight:600; padding:2px 9px; border-radius:20px; margin-left:10px; vertical-align:1px}
    .su-chip.ok {background:#E7F5EC; color:#0B6B33} .su-chip.warn {background:#FFF3D6; color:#7A5300} .su-chip.err {background:#FDE8E7; color:#A3120F}
    .su-sum {font-size:0.82em; color:#5f6368; margin-left:10px; font-weight:400}
    .su-bar {position:fixed; left:0; right:0; bottom:0; z-index:1030; background:rgba(255,255,255,0.97); border-top:1px solid #e6e8ec; box-shadow:0 -2px 10px rgba(0,0,0,0.05)}
    .su-bar .in {max-width:1280px; margin:0 auto; padding:10px 16px; display:flex; justify-content:space-between; align-items:center; gap:12px; flex-wrap:wrap}
    .su-bar .msg {font-size:0.9em; color:#1f2a44} .su-bar .btns {display:flex; gap:8px}
    .su-list {list-style:none; padding:0; margin:6px 0 10px 0} .su-list li {padding:5px 0; border-top:1px solid #eef0f3; font-size:0.92em}
    .su-list li:first-child {border-top:0} .su-list .fa, .su-list svg {margin-right:6px}
    .su-ok {color:#00833D} .su-wn {color:#B26A00} .su-er {color:#C4161C}
    .su-check {border:1px solid #e6e8ec; border-radius:8px; padding:10px 14px; margin-top:12px}
    .su-privacy, .su-files {margin-top:12px; font-size:0.9em} .su-privacy summary, .su-files summary {cursor:pointer; color:#2B3F87}
    .su-adv-row .form-group {margin-bottom:10px} .su-i {color:#8a8f98; font-size:0.85em}
    .su-chg {background:#FFF8E1; border:1px solid #F3D27A; border-radius:8px; padding:8px 10px; font-size:0.85em; margin:8px 0}
    .su-chg .r {font-size:0.85em; margin-left:6px}
    .accordion-button .su-chip {font-size:0.7em}
    @media (max-width: 600px) {.su-sep {display:none} .su-step {padding:3px 10px 3px 3px; font-size:0.85em} .su-step .n {width:22px; height:22px}
      .accordion-button .su-sum {display:none} .su-bar .in {padding:8px 10px}}
    [data-bs-theme=dark] .su-step {background:#232833; color:#a8adb4} [data-bs-theme=dark] .su-step .n {background:#14171c; color:#a8adb4}
    [data-bs-theme=dark] .su-step.on {background:#16242c; color:#e8eaed} [data-bs-theme=dark] .su-bar {background:rgba(20,23,28,0.97); border-color:#2e333b}
    [data-bs-theme=dark] .su-bar .msg {color:#e8eaed} [data-bs-theme=dark] .su-chg {background:#2b2614; border-color:#6b5a1e}
    [data-bs-theme=dark] .su-check, [data-bs-theme=dark] .su-list li {border-color:#2e333b}
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
    .bf-grid td.c.s1 {background:#E2231A} .bf-grid td.c.s2 {background:#F28C28} .bf-grid td.c.p {outline:2px solid #1f2a44; outline-offset:-2px}
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
    :root {--heat0:#F7F8FA}
    .act {font-size:0.74em; padding:2px 8px; border-radius:20px; white-space:nowrap}
    .act-open {background:#F1F2F4; color:#5f6368} .act-asg {background:#E6F4FB; color:#0B5A75} .act-done {background:#E7F5EC; color:#0B6B33}
    .act-esc {background:#EFE7FB; color:#5B2A9A} .act-warn {background:#FDE8E7; color:#A3120F; font-weight:600}
    .actbar {display:flex; align-items:center; gap:8px; flex-wrap:wrap; margin-top:6px}
    .actbar .btn {font-size:0.74em; padding:1px 9px; border-radius:6px}
    .actnote {font-size:0.78em; color:#5f6368; font-style:italic}
    .bf-acts-sum {font-size:0.76em; color:#5f6368; margin-top:4px}
    .qh-wrap {overflow-x:auto; margin-top:8px}
    .qh {border-collapse:separate; border-spacing:2px; font-size:0.78em}
    .qh th {font-weight:500; color:#5f6368; padding:2px 3px; white-space:nowrap}
    .qh th.tm {writing-mode:vertical-rl; transform:rotate(180deg); text-align:left; cursor:pointer; height:64px; vertical-align:bottom}
    .qh th.tm .dot {display:inline-block; width:6px; height:6px; border-radius:50%; margin-bottom:3px}
    .qh td.lab {white-space:nowrap; padding-right:8px; min-width:150px} .qh td.lab .d, .qh td.tot .d {color:#5f6368; font-size:0.9em; white-space:nowrap}
    .qh td.tot {text-align:right; padding-right:8px; white-space:nowrap}
    .qh td.c {width:20px; min-width:18px; height:20px; border-radius:3px; cursor:pointer}
    .qh td.c.na, .qh-legend .sw.na {background:repeating-linear-gradient(45deg, transparent, transparent 3px, #e3e5e8 3px, #e3e5e8 5px)}
    .qh-legend {font-size:0.75em; color:#5f6368; display:flex; align-items:center; gap:4px; flex-wrap:wrap}
    .qh-legend .sw {display:inline-block; width:16px; height:12px; border-radius:2px}
    .aibox .sugg {display:flex; gap:6px; flex-wrap:wrap; margin:6px 0}
    .aibox .sugg .btn {font-size:0.76em; border-radius:16px; padding:2px 10px}
    .aibox .aiq {display:flex; gap:8px; align-items:flex-start} .aibox .aiq .form-group {flex:1; margin-bottom:0}
    @media (max-width: 768px) {.ttl-long {display:none} .navbar-brand {white-space:normal}}
    @media (max-width: 600px) {.bf-kpis {grid-template-columns:repeat(2, minmax(0, 1fr))} .bf-kpi .v {font-size:1.5em} .bf {padding:4px 2px 24px 2px}
      .tp-head .form-group, .tp-head .selectize-control {width:100% !important} .tp-tbl {display:block; overflow-x:auto} .evb {grid-template-columns:70px minmax(0, 1fr) 40px}}
    [data-bs-theme=dark] {--heat0:#2a2f36}
    [data-bs-theme=dark] body {background:#14171c}
    [data-bs-theme=dark] .bf-card, [data-bs-theme=dark] .bf-kpi {background:#1c2027; border-color:#2e333b}
    [data-bs-theme=dark] .bf-title, [data-bs-theme=dark] .bf-kpi .v, [data-bs-theme=dark] .bf-head, [data-bs-theme=dark] .bf-card h4, [data-bs-theme=dark] .tp-ct,
      [data-bs-theme=dark] .bf-grid td.t, [data-bs-theme=dark] .evb .n {color:#e8eaed}
    [data-bs-theme=dark] .bf-meta, [data-bs-theme=dark] .bf-kpi .l, [data-bs-theme=dark] .bf-kpi .s, [data-bs-theme=dark] .hint, [data-bs-theme=dark] .bf-sub,
      [data-bs-theme=dark] .bf-ev .e, [data-bs-theme=dark] .tp-tbl td.m, [data-bs-theme=dark] .small-note {color:#a8adb4}
    [data-bs-theme=dark] .bf-do, [data-bs-theme=dark] .bf-ev .a {color:#8fb4ff}
    [data-bs-theme=dark] .bf-item, [data-bs-theme=dark] .bf-line, [data-bs-theme=dark] .bf-med, [data-bs-theme=dark] .bf-ev, [data-bs-theme=dark] .tp-tbl td,
      [data-bs-theme=dark] .bf-more summary {border-color:#2e333b}
    [data-bs-theme=dark] .bf-grid td.c, [data-bs-theme=dark] .evb .t {background:#2a2f36}
    [data-bs-theme=dark] .bf-grid td.c.s1 {background:#E2231A} [data-bs-theme=dark] .bf-grid td.c.s2 {background:#F28C28}
    [data-bs-theme=dark] .bf-grid td.c.p {outline-color:#e8eaed}
    [data-bs-theme=dark] .tp-tbl tr[onclick]:hover td {background:#232833}
    [data-bs-theme=dark] .ipm-explain {background:#16242c} [data-bs-theme=dark] .howto {background:#1b1e2b}
    [data-bs-theme=dark] .name-box {background:#2b2614; border-color:#6b5a1e}
    [data-bs-theme=dark] .shiny-plot-output img {border-radius:6px}
    .bf-med {display:grid; grid-template-columns:118px minmax(0, 1fr) auto; gap:12px; align-items:center; padding:7px 0; border-top:1px solid #eef0f3; font-size:0.88em}
    @media (max-width: 600px) {.bf-med {grid-template-columns:minmax(0, 1fr)}}
    .bf-more summary {cursor:pointer; color:#2B3F87; font-size:0.86em; font-weight:600; padding:9px 0 4px 0; border-top:1px solid #eef0f3}
    .bf-more .btn {font-size:0.78em; padding:2px 10px; border-radius:6px}
    .name-box {background:#FFF8E1; border:1px solid #F3D27A; border-radius:8px; padding:10px 12px 2px 12px; margin:10px 0}
    .bf-empty {max-width:640px; margin:60px auto; text-align:center}
    .bf-ev {border-top:1px solid #eef0f3; padding:10px 0}
    .bf-ev .e {font-size:0.85em; color:#444; margin-top:3px}
    .bf-ev .a {font-size:0.85em; color:#2B3F87; margin-top:3px}
    .fct-grid {border-collapse:separate; border-spacing:2px; font-size:0.78em}
    .fct-grid th {font-weight:600; text-align:center; padding:2px 4px; white-space:nowrap}
    .fct-grid th.t {cursor:pointer; color:#2B3F87; writing-mode:vertical-rl; transform:rotate(180deg); height:64px; vertical-align:bottom}
    .fct-grid th.t:hover {text-decoration:underline}
    .fct-grid td.r {text-align:left; padding-right:8px; white-space:nowrap; font-weight:600}
    .fct-grid td.c {width:24px; height:20px; text-align:center; border-radius:3px; cursor:pointer}
    .fct-grid td.ok {background:#CDE9C1} .fct-grid td.bad {background:#E2231A; color:white} .fct-grid td.na {background:#F1F2F4}
    .fct-grid td.n {font-weight:600; padding-left:8px}
    .fct-grid tr.tot td {border-bottom:2px solid #B6B7B8}
    .fct-kpi {display:flex; gap:12px; flex-wrap:wrap; margin-bottom:12px}
    .fct-kpi div {background:#F4F8FB; border-left:4px solid #1CABE2; padding:8px 14px; min-width:150px}
    .fct-kpi div b {display:block; font-size:1.5em; color:#374EA2}
    .fct-kpi div.r {border-left-color:#E2231A} .fct-kpi div.r b {color:#E2231A}
    #fct_dt td:first-child, #fct_ov_dt td:first-child {white-space:nowrap}
    span.fct-bad {background:#F8CBAD; color:#9C0006; font-weight:700; padding:1px 4px; border-radius:3px}
    table.dataTable tr.fct-total td {font-weight:700; background:#F2F2F2 !important}
    [data-bs-theme=dark] .fct-grid td.ok {background:#2f5a2a} [data-bs-theme=dark] .fct-grid td.na {background:#2a2f36}
    [data-bs-theme=dark] .fct-kpi div {background:#1b2530} [data-bs-theme=dark] table.dataTable tr.fct-total td {background:#2a2f36 !important}"))),

  # ---------------- BRIEFING (home after a run) ----------------
  nav_panel(tagList(icon("bolt"), "Briefing"), value = "briefing", uiOutput("brief")),
  nav_panel(tagList(icon("users"), "Teams"), value = "team",
    div(class = "tp-head", selectInput("tp_team", NULL, choices = NULL, width = "340px"),
        downloadButton("tp_brief", "Supervisor brief (Word)", class = "btn-sm btn-outline-primary")),
    uiOutput("tp_body")),
  nav_panel(tagList(icon("user"), "Interviewers"), value = "interv",
    div(class = "tp-head", selectizeInput("ip_iv", NULL, choices = NULL, width = "340px")),
    uiOutput("ip_body")),
  nav_panel(tagList(icon("table-cells"), "Questionnaire"), value = "quest",
    div(class = "tp-head", radioButtons("qh_by", NULL, c("Questions" = "question", "Modules" = "module"), inline = TRUE),
        selectInput("qh_top", NULL, c("20 with the most flags" = 20, "40 with the most flags" = 40, "80 with the most flags" = 80), selected = 40, width = "220px"),
        checkboxInput("qh_hide", "Hide those flagged in most teams (questionnaire problems)", FALSE)),
    uiOutput("qh_body")),
  nav_panel(tagList(icon("clipboard-check"), "Field check tables"), value = "fct",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(id = "fct_side", width = 300, open = "always",
        uiOutput("fct_read_ui"),
        radioButtons("fct_rows", "One row per", c("Team" = "team", "Interviewer" = "interv"), inline = TRUE),
        radioButtons("fct_period", "Days of interview", c("Whole fieldwork to date" = "all", "Period of analysis" = "period", "Choose dates" = "custom")),
        conditionalPanel("input.fct_period == 'custom'", dateRangeInput("fct_dates", NULL, width = "100%")),
        selectizeInput("fct_teams", "Teams", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "All teams")),
        tags$hr(style = "margin:6px 0"),
        downloadButton("fct_dl", "All tables (Excel)", class = "btn-sm btn-outline-primary w-100"),
        div(class = "small-note", style = "margin-top:6px", "One sheet per table as in the MICS7 FCT workbook, targets not met in red, with the summary, the overview and the parameters.")),
      explain("Produces the standard MICS7 Field Check Tables (the SPSS syntaxes 'MICS7 - Table F1...F20' of the central office) from the data loaded under Setup: response rates, eligible members per household, age displacement, completeness of dates of birth, key answers, observations, anthropometry, birth history and modules, by team or interviewer, with the targets of 'targets.sps'. Tables whose questions are not in the data are listed as not produced.",
              "Click 'Read the data' (only the variables the tables need are read; names are never read). Set the expected numbers of eligible members and the other targets under Parameters, then 'Apply'. 'Summary' shows the teams and tables where a target is not met: click a table to open it. The Excel workbook has all the tables."),
      navset_underline(id = "fct_tabs",
        nav_panel(tagList(icon("table-cells-large"), "Summary"), value = "sum", uiOutput("fct_sum")),
        nav_panel(tagList(icon("table"), "Tables"), value = "tab",
          div(class = "tp-head", selectInput("fct_pick", NULL, choices = NULL, width = "460px"),
              actionButton("fct_prev", icon("chevron-left"), class = "btn-sm btn-outline-secondary", title = "Previous table"),
              actionButton("fct_next", icon("chevron-right"), class = "btn-sm btn-outline-secondary", title = "Next table")),
          uiOutput("fct_tab_ui")),
        nav_panel(tagList(icon("chart-column"), "Performance overview"), value = "ov", uiOutput("fct_ov")),
        nav_panel(tagList(icon("sliders"), "Parameters"), value = "par", uiOutput("fct_par_ui"))))),
  nav_panel(tagList(icon("magnifying-glass-chart"), "Data explorer"), value = "explore",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(id = "ex_side", width = 330, open = "always",
        selectInput("ex_ds", "Dataset", choices = NULL, width = "100%"),
        selectizeInput("ex_var", "Variable", choices = NULL, width = "100%", options = list(placeholder = "Type a name or a word of the label")),
        radioButtons("ex_type", NULL, c("Auto" = "auto", "Categories" = "categorical", "Numbers" = "numeric"), inline = TRUE),
        uiOutput("ex_weight_ui"),
        selectInput("ex_by", "Broken down by", c("Nothing" = "none", "Team" = "team", "Interviewer" = "interv", "Week of fieldwork" = "week", "Another variable" = "var"), width = "100%"),
        conditionalPanel("input.ex_by == 'var'", selectizeInput("ex_byvar", NULL, choices = NULL, width = "100%", options = list(placeholder = "Variable to break down by")),
          selectInput("ex_bins", "If it is a number with many values (a score, an age), group it in", c("Fifths (quintiles)" = 5, "Quarters (quartiles)" = 4, "Thirds (terciles)" = 3, "Tenths (deciles)" = 10), width = "100%"),
          actionLink("ex_swap", tagList(icon("right-left"), " Swap the two variables"))),
        tags$hr(style = "margin:6px 0"),
        checkboxInput("ex_completed", "Completed interviews only", TRUE),
        checkboxInput("ex_blank", "Leave out blanks (question not asked)", TRUE),
        radioButtons("ex_period", "Days of interview", c("Whole fieldwork" = "all", "Period of analysis" = "period", "Choose dates" = "custom")),
        conditionalPanel("input.ex_period == 'custom'", dateRangeInput("ex_dates", NULL, width = "100%")),
        selectizeInput("ex_teams", "Teams", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "All teams")),
        selectizeInput("ex_intervs", "Interviewers", choices = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "All interviewers"))),
      explain("Shows the distribution of any variable of the data: frequencies of the answers (with 'don't know', missing and 'other' marked) or, for numbers, the summary, histogram and last digit (heaping). Broken down by team, interviewer, week or another variable, with a test of the differences. Only the variables you choose are read from the files, only counts and percentages are shown, and names and other text variables are never read.",
              "Choose a dataset and a variable. To compare, choose what to break it down by; filter by days, teams or interviewers on the left. From the Flag explorer and the Questionnaire page, 'Open in Data explorer' brings you here with the variable chosen."),
      uiOutput("ex_body"))),

  # ---------------- SETUP: 3 steps (data, review, run) with the advanced analysis settings on the side ----------------
  nav_panel(tagList(icon("sliders"), "Setup"), value = "setup",
    layout_sidebar(fillable = FALSE, border = FALSE,
      sidebar = sidebar(id = "adv", position = "right", open = "closed", width = 420, title = "Advanced analysis",
        tags$p(class = "small-note", "Change these only if you know why: they change which differences are flagged and which teams come first. The report lists every setting that differs from the defaults."),
        radioButtons("preset", "Preset", choices = c(stats::setNames(names(ipm_presets()), vapply(ipm_presets(), `[[`, "", "label")), "Custom (changed by hand)" = "custom"),
                     selected = "standard"),
        uiOutput("preset_help"), uiOutput("adv_changed"),
        accordion(id = "adv_groups", open = FALSE,
          accordion_panel("Flag tests", value = "flags", adv_inputs("flags")),
          accordion_panel("Issue rules and priorities", value = "rules", adv_inputs("rules")),
          accordion_panel("Output details", value = "outputs", adv_inputs("outputs")))),
      div(class = "su",
        div(class = "su-top", uiOutput("stepper"),
            actionButton("adv_open", tagList(icon("sliders"), " Advanced analysis", uiOutput("adv_badge", inline = TRUE)), class = "btn-outline-secondary btn-sm su-adv")),
        navset_hidden(id = "step",
          # ---- step 1: data ----
          nav_panel_hidden("s_data",
            explain("Loads the survey data files exported from CSPro/SPSS (hh, hl, wm, bh, ch, fs, mn ... in .sav or .dta format) and recognises the MICS structure: which file is which questionnaire, the interviewer, result, date and time variables, the links between files, the teams and the modules to test. It works for any MICS survey (MICS5-MICS7) whatever the language of the labels.",
                    "Drop the folder of your latest data export (or its .sav files, or a .zip of it) on the box, type the survey name, then review the settings or run at once."),
            layout_columns(col_widths = c(5, 7),
              card(card_header("Load the survey data"),
                data_drop("up_data"),
                if (LOCAL_MODE) tags$details(style = "margin-top:6px; font-size:0.9em",
                  tags$summary("Very large files on this computer or a network drive? Read them where they are"),
                  div(style = "margin-top:8px", textInput("data_dir", NULL, value = getOption("ipmR.data_dir", ""), width = "100%", placeholder = "e.g. Z:/MICS/Somalia/21 Oct"),
                      actionButton("scan", "Read this folder", class = "btn-outline-primary btn-sm"))),
                tags$details(class = "su-privacy", tags$summary(icon("lock"), " Your data stay in your session"),
                  div(class = "small-note", sprintf("Files are processed only within your own session and are never shared with other users. Name and other free-text variables are removed when the files are read. All uploaded files, results and reports are deleted from the server when you close the browser tab, after %s minutes of inactivity, or immediately with the button below. Do not upload data you are not authorised to process on this server.", format(IDLE_MINUTES)),
                      div(style = "margin-top:8px", actionButton("wipe", "Delete my data now", class = "btn-outline-danger btn-sm"))))),
              card(card_header("What was found"), uiOutput("found_ui"), uiOutput("name_box"),
                   tags$details(class = "su-files", tags$summary("Files found"), tableOutput("found_tbl"))))),
          # ---- step 2: review ----
          nav_panel_hidden("s_review",
            explain("Everything below was filled in from the data. Each part shows what was set and whether it needs a look: only the parts marked 'check' are open.",
                    "Open a part to change it. The status updates as you edit; when nothing needs a look, run the analysis."),
            accordion(id = "rev", multiple = TRUE, open = FALSE,
              accordion_panel(value = "period", title = tagList(tags$span(class = "su-t", "Survey and period"), uiOutput("st_period", inline = TRUE)), icon = icon("calendar"),
                uiOutput("iss_period"),
                layout_columns(col_widths = c(6, 6),
                  div(textInput("survey_name", "Name of the survey", width = "100%", placeholder = "e.g. Madagascar MICS7 2025"),
                      fluidRow(column(6, dateInput("period_start", "Period of analysis: first day", value = Sys.Date() - 21)),
                               column(6, numericInput("period_days", "Extent (days)", value = 21, min = 1))),
                      div(style = "margin:-6px 0 8px 0", tags$span(class = "small-note", "Set to: "),
                          actionButton("pp_7", "Last 7 days", class = "btn-sm"), actionButton("pp_14", "Last 14 days", class = "btn-sm"),
                          actionButton("pp_21", "Last 21 days", class = "btn-sm"), actionButton("pp_all", "Whole fieldwork", class = "btn-sm"))),
                  div(uiOutput("period_info"),
                      tags$p(class = "small-note", "The period of analysis (21 days recommended, normally the most recent 3 weeks) is used for the flag analysis and the anthropometry case listing; the other outputs cover the whole fieldwork.")))),
              accordion_panel(value = "outputs", title = tagList(tags$span(class = "su-t", "Outputs"), uiOutput("st_outputs", inline = TRUE)), icon = icon("chart-column"),
                uiOutput("iss_outputs"),
                layout_columns(col_widths = c(5, 7),
                  div(checkboxGroupInput("outputs", "Outputs", choices = stats::setNames(names(output_names), output_names), width = "100%"),
                      checkboxGroupInput("hhm_groups", "Household member groups (output 4)", inline = TRUE,
                                         choices = stats::setNames(names(hhm_defs), vapply(hhm_defs, `[[`, "", "label")))),
                  div(tags$b("Variables for specific outputs"), uiOutput("extras_ui")))),
              accordion_panel(value = "datasets", title = tagList(tags$span(class = "su-t", "Datasets"), uiOutput("st_datasets", inline = TRUE)), icon = icon("database"),
                tags$p(class = "small-note", "For each data file: the interviewer, the result of the interview (1 = completed), the date and the start/end time. Files that are only part of a questionnaire (household members, birth history, bednets...) are linked to their parent questionnaire by key variables (cluster, household, line number) and take its interviewer and date. Month and year are taken from the two variables after the day, and minutes from the variable after each hour (MICS convention)."),
                uiOutput("iss_datasets"), uiOutput("ds_ui")),
              accordion_panel(value = "teams", title = tagList(tags$span(class = "su-t", "Teams"), uiOutput("st_teams", inline = TRUE)), icon = icon("users"),
                uiOutput("iss_teams"),
                layout_columns(col_widths = c(5, 7),
                  div(tags$p(class = "small-note", "All comparisons are made within teams (an interviewer is compared with team-mates working in the same clusters), so correct teams are essential. Teams are labelled T1, T2 ... followed by the supervisor / team leader code, e.g. T3 (103)."),
                      radioButtons("team_method", NULL, c("Interviewer number ranges (as in IPM)" = "ranges", "Team = value of a variable (e.g. supervisor number)" = "variable")),
                      conditionalPanel("input.team_method == 'ranges'",
                        textAreaInput("team_ranges", "One range per team (T1, T2, ...)", rows = 10, width = "100%"),
                        fluidRow(column(4, textInput("pat_first", "Team 1 range", "10-19")), column(4, numericInput("pat_n", "No. of teams", 20, 1, 90)), column(4, numericInput("pat_step", "Step", 10, 1))),
                        actionButton("pat_apply", "Apply pattern of team 1 to all teams", class = "btn-sm")),
                      conditionalPanel("input.team_method == 'variable'", uiOutput("team_var_ui"))),
                  div(tags$b("Interviewers by team (from the data)"), tableOutput("team_tbl")))),
              accordion_panel(value = "flags", title = tagList(tags$span(class = "su-t", "Questions tested"), uiOutput("st_flags", inline = TRUE)), icon = icon("list-check"),
                uiOutput("iss_flags"),
                tags$p(class = "small-note", "Modules follow the MICS naming (HC* = all variables starting with HC). Identification variables are ignored, and 'unit' variables (e.g. PN3U days/weeks) are combined with the number that follows before testing. Variables that are not questions to the respondent or direct observations (identifiers, CAPI-filled, derived, counts) should be ignored: CAPI-filled and derived variables are almost never flagged and lower the % of a module; identifiers and staff codes give meaningless flags."),
                uiOutput("flag_ui"),
                div(class = "su-check", div(actionButton("fc_run", "List every variable tested", class = "btn-outline-primary btn-sm"), downloadButton("fc_dl", "Download the list (CSV)", class = "btn-sm")),
                  uiOutput("fc_problems"), DT::DTOutput("fc_modules"), DT::DTOutput("fc_vars"))))),
          # ---- step 3: run ----
          nav_panel_hidden("s_run",
            explain("Runs all selected outputs: from a few seconds (early fieldwork) to 1-2 minutes (complete national survey). You can keep using the app while it runs.",
                    "Check the summary, then click Run. You are taken to the briefing when it is done."),
            layout_columns(col_widths = c(7, 5),
              card(card_header("What will run"), uiOutput("run_summary"),
                actionButton("run", tagList(icon("play"), " Run the analysis"), class = "btn-primary btn-lg", width = "100%"),
                verbatimTextOutput("log", placeholder = TRUE) |> tagAppendAttributes(class = "log")),
              card(card_header("Settings"),
                uiOutput("saved_note"),
                tags$p(class = "small-note", "To use the settings on another computer or server: save them to a file here, and load the file after uploading the data there."),
                downloadButton("cfg_download", "Save settings (YAML)", class = "btn-sm"),
                fileInput("cfg_upload", "Load settings (YAML)", accept = c(".yml", ".yaml"), width = "100%"))))),
        uiOutput("su_bar")))),

  # ---------------- RESULTS ----------------
  nav_panel(tagList(icon("chart-line"), "Results"), value = "results",
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
        div(class = "howto", tags$b("How to read: "), "Each issue is a questionnaire problem (question or module flagged in most teams - not counted against teams), a whole-team issue (the team differs from the other teams) or an interviewer issue (one interviewer differs from team-mates). Priority, with the standard settings (Setup > Advanced analysis): High = a team-wide excess of flags (at least 1.5 times the survey rate, binomial p < 0.01, questionnaire problems left out) or 2 or more high-severity issues; Medium = 1 high-severity issue or 3 or more issues. Points (3 per high, 1 per medium) only order the teams. '% flagged' and 'x survey rate' leave out the questions flagged in most teams, like the issues: 'x survey rate' compares the team's % of flagged tests with the survey on the same basis (1 = average)."),
        tags$h5(class = "sch", "Warning signs with evidence and standard action"), DT::DTOutput("sc_signals"),
        tags$h5(class = "sch", "Interviewers"), DT::DTOutput("sc_interv")),
      nav_panel("Action plan (AI)", value = "ai",
        explain(tagList("Writes a team-by-team action plan (issues with evidence, actions with owner and timing, interviewers to observe, modules to retrain) and answers questions about the results. ",
                        tags$b("Only aggregated statistics are sent to the AI model"), " - the team scorecard, flag rates per module and question, one-line readings of flagged questions, interviewer codes. No record-level data, household identifiers or names leave the server; click 'Preview what is sent to the AI' to see the full request (instructions, evidence and question) exactly as it is sent. Without AI, the rule-based plan gives the same structure from the standard action library."),
                "Click 'Generate AI action plan' (a window shows the size, cost and time and lets you choose the teams first; 1 to 5 minutes) or 'Rule-based plan'. Rule-based: the routine weekly check and the complete list of everything flagged; AI: a readable briefing for supervisors and managers; both: the AI plan to brief people, the rule-based plan as the full checklist behind it. Check the plan against the flag tables before sharing it, especially any line marked 'check'. Download it in Word for the supervisors' debrief."),
        layout_columns(col_widths = c(4, 8),
          card(card_header("AI model and action plan"),
            selectInput("ai_conn", "Connection", AI_CHOICES, width = "100%", selectize = FALSE),
            conditionalPanel("input.ai_conn == 'custom'",
              radioButtons("u_prov", NULL, c("Claude (Anthropic API)" = "anthropic", "Local model (Ollama)" = "ollama",
                                             "OpenAI-compatible (vLLM, LM Studio, OpenAI...)" = "openai")),
              textInput("u_base", "Endpoint URL (filled in for the provider; change it if yours is different)", value = "https://api.anthropic.com", width = "100%",
                        placeholder = "Ollama: http://192.168.1.10:11434  |  vLLM: http://host:8000/v1"),
              passwordInput("u_key", "API key (not needed for Ollama)", width = "100%"),
              actionButton("u_set", "Connect", width = "100%")),
            selectizeInput("ai_model", "Model", choices = NULL, width = "100%",
                           options = list(create = TRUE, placeholder = "List the models or type a name")),
            uiOutput("ai_model_hint"),
            layout_columns(col_widths = c(6, 6), actionButton("ai_models", "List models", width = "100%"), actionButton("ai_test", "Test", width = "100%")),
            uiOutput("ai_status"),
            tags$hr(style = "margin:4px 0"),
            selectInput("ai_lang", "Language of the plan", c("English", "French", "Spanish", "Portuguese", "Arabic"), width = "100%"),
            checkboxInput("ai_compact", "Compact evidence (High/Medium teams only, about a third of the size: faster and better with local models)", FALSE),
            actionButton("ai_go", "Generate AI action plan", class = "btn-primary", width = "100%"),
            actionButton("rules_go", "Rule-based plan (no AI)", width = "100%"),
            div(class = "small-note", style = "margin:2px 0 10px 0", tags$b("Which plan?"),
              tags$ul(style = "padding-left:18px; margin:4px 0 0 0",
                tags$li(tags$b("Rule-based: "), "the routine weekly check, and a complete list of everything flagged."),
                tags$li(tags$b("AI: "), "the version you hand to supervisors or managers, a readable briefing with priorities and explanations."),
                tags$li(tags$b("Both: "), "the AI plan to brief people, with the rule-based plan as the full checklist behind it."))),
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
          div(style = "margin-bottom:6px", downloadButton("fig_svg", "Sharp version (SVG)", class = "btn-sm"), downloadButton("fig_png", "High-resolution image (PNG)", class = "btn-sm"),
              actionButton("fig_read", tagList(icon("wand-magic-sparkles"), "Read this graph with AI"), class = "btn-sm btn-outline-primary")),
          uiOutput("fig_read_out"),
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
        div(style = "margin-bottom:8px", actionButton("dsum_read", tagList(icon("wand-magic-sparkles"), "Read these tables with AI"), class = "btn-sm btn-outline-primary")),
        uiOutput("dsum_read_out"),
        tableOutput("dsum"), tableOutput("qpd")),
      nav_panel("Downloads", value = "downloads",
        explain("Builds the IPM report (Word) with the sections you choose, and the companion files you choose: flag tables per dataset and per team (Word), team action plan (Word), anthropometry case listing (Excel) and tidy CSV files for dashboards.",
                "Tick the sections and files you need (fewer parts build faster), click 'Build report', and download when the 'Report complete' message appears. You can keep using the app while it is built."),
        uiOutput("rep_parts"),
        actionButton("build", "Build report and files", class = "btn-primary"),
        br(), br(),
        uiOutput("dl_status")))),

  nav_spacer(),
  nav_item(input_dark_mode(id = "dark_mode", mode = "light")),
  nav_panel("Guide", value = "guide",
    card(card_header("About the Interviewer Performance Monitoring (IPM)"),
      tags$p("The IPM complements the MICS field check tables. While fieldwork is going on, it compares the work of each interviewer with that of his/her team-mates, who work in the same clusters at the same time and should therefore record similar answers. Large differences point to interviewers who may need support (or to questions that are not well understood) while there is still time to fix them."),
      tags$ol(
        tags$li(tags$b("Data: "), "upload the latest export of the fieldwork data (.sav)."),
        tags$li(tags$b("Settings, datasets, teams, flags: "), "checked automatically - review them the first time, then save the settings."),
        tags$li(tags$b("Run: "), "computes everything."),
        tags$li(tags$b("Results: "), "start with 'Summary & priorities' and 'Team scorecard' (priority of each team with the evidence), then 'Action plan (AI)' for a team-by-team plan and questions (rule-based plan for the routine weekly check and the full list of what is flagged; AI plan for a readable briefing to supervisors and managers; or both); check with the figures and the flag explorer; download the Word report and action plan to share."),
        tags$li(tags$b("Field check tables: "), "the standard MICS7 Field Check Tables (F1 to F20) by team or interviewer, without SPSS: read the data, set the targets under Parameters, look at the summary grid of the targets not met, open each table and its graph, and download the Excel workbook. They do not need a run of the analysis.")),
      tags$p("Recommended rhythm: every 1-2 weeks from the 2nd-3rd week of fieldwork, with the last 2-3 weeks as period of analysis; share a limited number of priorities with the field teams each time."),
      tags$h5("How to read each output"),
      tags$dl(lapply(names(output_names), function(id) tagList(tags$dt(output_names[[id]]), tags$dd(howto[[id]] %||% ""))))),
    tags$p(class = "small-note", sprintf("ipmR %s - R port of the Stata IPM application v7.0.0 developed by Pierre Martel for UNICEF MICS.", .int$version %||% "")))
)

ipm_server <- function(input, output, session) {
  sweep_stale()
  message("session ", substr(session$token, 1, 6), " started; tempdir ", tempdir())
  sdir <- file.path(tempdir(), paste0("ipm_", session$token)); dir.create(sdir, recursive = TRUE, showWarnings = FALSE)
  upload_dirs <- character()                       # Shiny's own temporary copies of uploaded files
  rv <- reactiveValues(cfg = NULL, light = NULL, res = NULL, log = character(), report = NULL, zip = NULL, n_files = 0,
                       sc = NULL, cmp = NULL, acts_tick = 0, act_key = NULL, plan = NULL, chat = list(), reads = list(), ai_user = NULL, ai_test = NULL, prev = NULL, stale_model = NULL, listed = character(), jobs = 0L)
  cleanup <- function() {
    if (exists("kill_jobs")) kill_jobs()
    wipe(c(sdir, upload_dirs))
    isolate({ rv$cfg <- NULL; rv$light <- NULL; rv$res <- NULL; rv$report <- NULL; rv$zip <- NULL
              rv$sc <- NULL; rv$cmp <- NULL; rv$plan <- NULL; rv$chat <- list(); rv$reads <- list(); rv$ai_user <- NULL; rv$ai_test <- NULL })
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
      report = { unlink(args$dir, recursive = TRUE); rep <- ipm_report(args$res, args$dir, progress = prog, sections = args$sections, files = args$files)
                 prog("Zipping all results"); unlink(args$zip); old <- setwd(args$dir); on.exit(setwd(old), add = TRUE)
                 utils::zip(args$zip, list.files(".", recursive = TRUE), flags = "-r9Xq"); list(report = rep, zip = args$zip) },
      plan = ipm_ai_plan(args$res, args$sc, args$st, language = args$language, compact = args$compact, priorities = args$priorities),
      ask = ipm_ai_ask(args$q, args$res, args$sc, args$st, history = args$history, language = args$language, compact = args$compact),
      read = ipm_ai_read(args$title, args$x, args$how, args$st, language = args$language),
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
    rv$res <- readRDS(pre); rv$cfg <- rv$res$cfg; rv$light <- tryCatch(ipm_load(rv$cfg, quiet = TRUE, n_max = 400), error = function(e) NULL); rv$sc <- tryCatch(ipm_scorecard(rv$res), error = function(e) NULL)
    rv$cmp <- tryCatch(ipm_history_update(rv$res, rv$sc), error = function(e) NULL) })

  # Setup is one page in 3 steps; the old page names lead to the step (and the part of the review) they were
  step_of <- c(data = "s_data", settings = "s_review", datasets = "s_review", teams = "s_review", flags = "s_review", run = "s_run")
  part_of <- c(settings = "period", datasets = "datasets", teams = "teams", flags = "flags")
  cur_step <- reactiveVal("s_data")
  go_step <- function(s) { cur_step(s); bslib::nav_select("step", s)
    if (s == "s_review") {                       # open only the parts that need a look (none: all closed)
      st <- tryCatch(isolate(setup_status()), error = function(e) NULL); if (is.null(st)) return(invisible())
      w <- names(Filter(function(z) z$status != "ok", st))
      if (length(w)) accordion_panel_set("rev", w) else accordion_panel_close("rev", TRUE) } }
  nav_to <- function(v) {
    if (v %in% names(step_of)) { bslib::nav_select("nav", "setup"); go_step(step_of[[v]])
      if (!is.na(part_of[v])) accordion_panel_open("rev", part_of[[v]]) }
    else bslib::nav_select("nav", v)
  }
  observeEvent(input$go_step, go_step(input$go_step))
  observeEvent(input$su_next, go_step(c(s_data = "s_review", s_review = "s_run")[[cur_step()]]))
  observeEvent(input$su_back, go_step(c(s_review = "s_data", s_run = "s_review")[[cur_step()]]))
  observeEvent(input$su_run, { go_step("s_run"); do_run() })
  observeEvent(input$adv_open, toggle_sidebar("adv"))

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
    set_adv(cfg$options)
  }
  set_adv <- function(o) {                       # advanced inputs from a set of options (and the preset they match)
    o <- utils::modifyList(ipm_defaults(), o %||% list())
    for (n in names(PSPEC)) { id <- paste0("o_", n); s <- PSPEC[[n]]
      switch(s$type, lgl = updateCheckboxInput(session, id, value = isTRUE(o[[n]])), select = updateSelectInput(session, id, selected = o[[n]]),
             updateNumericInput(session, id, value = o[[n]])) }
    m <- .int$match_preset(o); adv_pending_set(m %|na|% "custom")
    updateRadioButtons(session, "preset", selected = m %|na|% "custom")
  }
  `%|na|%` <- function(a, b) if (length(a) && !is.na(a)) a else b
  adv_pending <- reactiveVal(NULL)
  adv_pending_set <- function(p) adv_pending(list(p = p, t = Sys.time()))
  use_settings <- function(cfg, what) {          # settings from a file or remembered: datasets of this export only, its dates
    keep <- intersect(names(cfg$datasets), names(rv$cfg$datasets))
    if (!length(keep)) { showNotification(sprintf("The %s do not match the uploaded datasets.", what), type = "error"); return(FALSE) }
    cfg$datasets <- cfg$datasets[keep]; cfg$fieldwork <- rv$cfg$fieldwork
    if (is.null(cfg$period$start)) cfg$period$start <- rv$cfg$period$start
    cfg$saved <- NULL; cfg$ipmR_version <- NULL
    apply_cfg(cfg); TRUE
  }
  observeEvent(input$cfg_upload, {
    req(rv$cfg); if (use_settings(read_config(input$cfg_upload$datapath), "settings in the file")) showNotification("Settings loaded.", type = "message")
  })

  # survey name asked on the data page too (same value as in the review); settings remembered for that name can be used at once
  svy_names <- reactive({ input$su_run; input$run; tryCatch(ipm_settings_list(), error = function(e) data.frame(name = character())) })
  saved_cfg <- reactive({ nm <- trimws(input$survey_name %||% ""); req(rv$cfg, nzchar(nm)); ipm_settings_load(nm) })
  saved_d <- debounce(saved_cfg, 600)
  output$name_box <- renderUI({
    req(rv$cfg)
    ti <- textInput("survey_name_d", tagList(tags$b("Name of the survey"), " - shown on the briefing and the reports, and used to keep the run history and the settings of this survey"),
                    value = isolate(input$survey_name) %||% "", width = "100%", placeholder = "e.g. Madagascar MICS7 2025")
    nms <- isolate(svy_names())$name
    if (length(nms)) ti <- htmltools::tagQuery(ti)$find("input")$addAttrs(list = "svy_list", autocomplete = "off")$allTags()
    div(class = "name-box", ti, if (length(nms)) tags$datalist(id = "svy_list", lapply(nms, function(n) tags$option(value = n))),
        uiOutput("saved_offer"),
        div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap; margin:4px 0 10px 0",
            actionButton("run_quick", tagList(icon("play"), " Run now"), class = "btn-primary"),
            actionButton("go_review", tagList("Review the settings ", icon("arrow-right")), class = "btn-outline-primary"),
            span(class = "small-note", "Everything was filled in from the data; the review shows what needs a look.")))
  })
  output$saved_offer <- renderUI({
    s <- tryCatch(saved_d(), error = function(e) NULL); if (is.null(s)) return(NULL)
    if (identical(used_saved(), s$saved)) return(div(class = "small-note", style = "margin:-6px 0 8px 0", icon("circle-check", class = "su-ok"),
                                                     sprintf(" Using the settings of the last run (saved %s).", s$saved %||% "earlier")))
    div(class = "su-chg", style = "background:#E6F4FB; border-color:#9fd6ee", icon("clock-rotate-left"),
        sprintf(" Settings of the last run of %s found (saved %s).", s$survey_name, s$saved %||% "earlier"),
        actionButton("use_saved", "Use them", class = "btn-sm btn-primary", style = "margin-left:8px"))
  })
  used_saved <- reactiveVal(NULL)
  observeEvent(input$use_saved, { s <- saved_d(); req(s)
    if (use_settings(.int$normalise(s), "remembered settings")) { used_saved(s$saved %||% "")
      showNotification("The settings of the last run are used: check them in the review, or run now.", type = "message", duration = 6) } })
  observeEvent(input$go_review, go_step("s_review"))
  observeEvent(input$survey_name_d, if (!identical(input$survey_name_d, input$survey_name)) updateTextInput(session, "survey_name", value = input$survey_name_d))
  observeEvent(input$survey_name, if (!is.null(input$survey_name_d) && !identical(input$survey_name_d, input$survey_name)) updateTextInput(session, "survey_name_d", value = input$survey_name))
  output$found_ui <- renderUI({
    cfg <- cur_cfg()
    if (is.null(cfg)) return(tags$p(class = "small-note", "No data loaded yet: drop the data export on the box."))
    q <- names(cfg$datasets)[vapply(cfg$datasets, function(d) !is.null(d$result), logical(1))]
    st <- tryCatch(setup_status(), error = function(e) NULL)
    if (is.null(st)) return(tags$p(class = "small-note", icon("spinner", class = "fa-spin"), " Checking the settings..."))
    li <- function(ok, txt) tags$li(if (isTRUE(ok)) icon("circle-check", class = "su-ok") else icon("triangle-exclamation", class = "su-wn"), txt)
    div(tags$ul(class = "su-list",
          li(length(q) > 0, sprintf("%d files read: %d questionnaires, %d linked files", length(cfg$datasets), length(q), sum(vapply(cfg$datasets, function(d) !is.null(d$parent), logical(1))))),
          if (!is.null(cfg$fieldwork)) li(TRUE, fw_text(cfg$fieldwork)) else li(FALSE, "No interview dates found"),
          li(st$teams$status == "ok", st$teams$text),
          li(isTRUE(st$period$period_ok), sprintf("Period of analysis proposed: %s, %s days (%s)", if (is.null(cfg$period$start)) "-" else fmt_d(cfg$period$start), cfg$period$days,
                  if (!is.null(cfg$fieldwork) && cfg$fieldwork$days <= 21) "the whole fieldwork so far" else "the last 3 weeks")),
          li(st$outputs$status == "ok", st$outputs$text),
          if (!isTRUE(st$period$named)) li(FALSE, "Type the name of the survey below")),
        if (length(cfg$fieldwork$by_questionnaire) > 1) tags$details(class = "su-files", tags$summary("Interview dates per questionnaire"),
          tags$table(class = "table table-sm", style = "width:auto; font-size:0.85em",
            tags$tr(tags$th("Questionnaire"), tags$th("First interview"), tags$th("Last interview"), tags$th("Records")),
            lapply(names(cfg$fieldwork$by_questionnaire), function(q) { z <- cfg$fieldwork$by_questionnaire[[q]]
              tags$tr(tags$td(q), tags$td(fmt_d(z$first)), tags$td(fmt_d(z$last)), tags$td(format(z$interviews, big.mark = ","))) }))))
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
    if (is.null(fw)) return(tags$p(class = "small-note", "Interview dates are shown here once the data are loaded."))
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
  output$fc_problems <- renderUI({ x <- fc(); if (!length(x$problems)) return(tags$p(class = "small-note", style = "color:#00833D", "No module without variables and no unknown variable to ignore."))
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
    a <- adv_read(input); for (k in names(a)) cfg$options[[k]] <- a[[k]]
    if (!is.null(fct_keep())) cfg$fct <- fct_keep()                # field check table parameters remembered with the settings
    cfg
  }
  output$cfg_download <- downloadHandler(filename = function() "ipm_settings.yml", content = function(f) {
    cfg <- collect_cfg(); cfg$data_dir <- NULL; cfg$results_dir <- NULL; write_config(cfg, f) })

  # ---------- teams from the data (for the table and the status) ----------
  team_key <- reactive({ cfg <- cur_cfg(); req(cfg)
    list(teams = cfg$teams, dir = cfg$data_dir, iv = lapply(cfg$datasets, function(d) c(d$file, d$interviewer, d$supervisor))) })
  team_key_d <- debounce(team_key, 800)
  team_assign <- reactive({
    team_key_d(); cfg <- isolate(cur_cfg()); req(cfg)
    rows <- list()
    for (n in names(cfg$datasets)) { d <- cfg$datasets[[n]]; if (is.null(d$interviewer)) next
      cols <- unique(c(d$interviewer, cfg$teams$variable, d$supervisor))
      x <- tryCatch(.int$read_cols(file.path(cfg$data_dir, d$file), cols), error = function(e) NULL); if (is.null(x) || !d$interviewer %in% names(x)) next
      iv <- as.numeric(x[[d$interviewer]]); sup <- if (!is.null(cfg$teams$variable) && cfg$teams$variable %in% names(x)) as.numeric(x[[cfg$teams$variable]]) else NULL
      code <- if (!is.null(d$supervisor) && d$supervisor %in% names(x)) suppressWarnings(as.numeric(x[[d$supervisor]])) else NA_real_
      rows[[n]] <- data.table(interv = iv, sup = if (is.null(sup)) NA_real_ else sup, code = code) }
    if (!length(rows)) return(NULL)
    a <- rbindlist(rows)[!is.na(interv)]
    lv <- if (!is.null(cfg$teams$variable)) sort(unique(a$sup)) else NULL
    a[, team := tryCatch(.int$assign_teams(interv, if (is.null(lv)) NULL else sup, cfg, lv), error = function(e) NA_integer_)]
    if (!is.null(lv)) a[, code := sup]
    a
  })
  output$team_tbl <- renderTable({
    a <- team_assign(); req(a)
    codes <- a[!is.na(team) & !is.na(code), .N, by = .(team, code)][order(team, -N)][, .(code = paste(fmt_i(code), collapse = ", ")), by = team]
    a <- unique(a[, .(interv, team)])
    out <- a[order(team, interv), .(interviewers = paste(sort(unique(interv)), collapse = ", "), n = uniqueN(interv)), by = team]
    out[, `supervisor / team leader code` := codes$code[match(team, codes$team)]]
    out[, team := ifelse(is.na(team), "NOT ASSIGNED (excluded)", paste0("T", team))]
    out[is.na(`supervisor / team leader code`), `supervisor / team leader code` := ""]
    out
  })

  # ---------- status of each part of the setup (review cards, step bar, run summary) ----------
  status_cfg <- debounce(cur_cfg, 700)
  DS_FIELDS <- "^(interviewer|result|day|start_hour|end_hour|supervisor|file|parent|keys|linked|no interviewer|no day)"
  setup_status <- reactive({
    cfg <- status_cfg(); req(cfg, rv$light)
    iss <- tryCatch(validate_config(cfg, rv$light), error = function(e) data.frame(level = "error", where = "load", message = conditionMessage(e)))
    part <- ifelse(iss$where == "teams", "teams", ifelse(iss$where %in% c("main", "load"), "period", ifelse(iss$where == "extras", "outputs",
            ifelse(grepl("^(module matches|unit variable)", iss$message), "flags", ifelse(grepl(DS_FIELDS, iss$message), "datasets", "outputs")))))
    iss$message <- ifelse(iss$where %in% c("main", "teams", "extras", "load", "datasets"), iss$message, paste0(iss$where, ": ", iss$message))
    mk <- function(p, text, extra = character(), extra_lvl = "warning") {
      m <- iss[part == p, , drop = FALSE]
      lv <- c(m$level, rep(extra_lvl, length(extra)))
      list(status = if (any(lv == "error")) "err" else if (length(lv)) "warn" else "ok", text = text, issues = c(m$message, extra))
    }
    fw <- cfg$fieldwork; st <- suppressWarnings(as.Date(cfg$period$start)); n <- cfg$period$days %||% NA
    per_x <- character(); per_txt <- "no period set"
    if (length(st) && !is.na(st) && !is.na(n)) { per_txt <- sprintf("%s days from %s", n, fmt_d(st))
      if (!is.null(fw) && st > as.Date(fw$last)) per_x <- "The period starts after the last interview: no data to analyse."
      else if (!is.null(fw) && st + n < as.Date(fw$first)) per_x <- "The period ends before the first interview: no data to analyse." }
    nm <- trimws(input$survey_name %||% "")
    name_x <- if (!nzchar(nm)) "No survey name: the briefing, reports and run history will say 'MICS survey'." else character()
    nout <- sum(unlist(cfg$outputs))
    a <- tryCatch(team_assign(), error = function(e) NULL)
    nt <- if (!is.null(a)) uniqueN(a$team[!is.na(a$team)]) else length(cfg$teams$ranges %||% character())
    na_iv <- if (!is.null(a)) uniqueN(a$interv[is.na(a$team)]) else 0L
    tm_txt <- sprintf("%d teams%s", nt, if (!is.null(a)) sprintf(", %d interviewers", uniqueN(a$interv[!is.na(a$team)])) else "")
    fc <- tryCatch(ipm_flag_check(cfg, rv$light$meta), error = function(e) NULL)
    nmod <- sum(lengths(lapply(cfg$datasets, `[[`, "flag_modules"))); nign <- sum(lengths(lapply(cfg$datasets, `[[`, "flag_ignore")))
    nq <- sum(vapply(cfg$datasets, function(d) !is.null(d$result), logical(1))); nl <- sum(vapply(cfg$datasets, function(d) !is.null(d$parent), logical(1)))
    list(period = c(mk("period", per_txt, c(per_x, name_x), if (length(per_x)) "error" else "warning"), list(period_ok = !length(per_x) && !any(part == "period"), named = nzchar(nm))),
         outputs = mk("outputs", sprintf("%d of %d outputs", nout, length(cfg$outputs))),
         datasets = mk("datasets", sprintf("%d questionnaires, %d linked files", nq, nl)),
         teams = mk("teams", tm_txt, if (na_iv) sprintf("%d interviewer(s) not in any team: they are left out of the analysis.", na_iv)),
         flags = mk("flags", sprintf("%d modules, %d variables ignored", nmod, nign), if (!is.null(fc)) fc$problems else character()))
  })
  chip <- function(s) span(span(class = paste("su-chip", s$status), c(ok = "OK", warn = "Check", err = "Fix")[[s$status]]), span(class = "su-sum", s$text))
  for (k in c("period", "outputs", "datasets", "teams", "flags")) local({ key <- k
    output[[paste0("st_", key)]] <- renderUI({ s <- setup_status()[[key]]; chip(s) }) })
  for (k in c("period", "outputs", "datasets", "teams", "flags")) local({ key <- k
    output[[paste0("iss_", key)]] <- renderUI({ s <- setup_status()[[key]]; if (!length(s$issues)) return(NULL)
      div(class = "aiwarn", tags$ul(style = "margin:0; padding-left:18px", lapply(s$issues, tags$li))) }) })
  warn_parts <- function(st) { lab <- c(period = "Survey and period", outputs = "Outputs", datasets = "Datasets", teams = "Teams", flags = "Questions tested")
    w <- names(Filter(function(z) z$status != "ok", st)); stats::setNames(unname(lab[w]), w) }

  output$stepper <- renderUI({
    s <- cur_step(); has <- !is.null(rv$cfg)
    st <- if (has) tryCatch(setup_status(), error = function(e) NULL) else NULL
    rev_ok <- !is.null(st) && !length(warn_parts(st))
    one <- function(id, n, lab, ok) { off <- !has && id != "s_data"
      div(class = paste("su-step", if (s == id) "on", if (ok && s != id) "ok", if (off) "off"),
          onclick = if (!off) sprintf("Shiny.setInputValue('go_step', '%s', {priority: 'event'})", id),
          span(class = "n", if (ok && s != id) icon("check") else n), lab) }
    div(class = "su-steps", one("s_data", 1, "Data", has), div(class = "su-sep"), one("s_review", 2, "Review", rev_ok), div(class = "su-sep"),
        one("s_run", 3, "Run", !is.null(rv$res)))
  })
  output$su_bar <- renderUI({
    s <- cur_step()
    msg <- if (is.null(rv$cfg)) span(icon("cloud-arrow-up"), " Load the data to start.") else {
      st <- tryCatch(setup_status(), error = function(e) NULL); w <- if (is.null(st)) character() else warn_parts(st)
      if (is.null(st)) span(icon("spinner", class = "fa-spin"), " Checking the settings...")
      else if (!length(w)) span(icon("circle-check", class = "su-ok"), " Ready to run: nothing needs a look.")
      else span(icon("triangle-exclamation", class = "su-wn"), sprintf(" %d part%s to check: ", length(w), if (length(w) > 1) "s" else ""),
                lapply(seq_along(w), function(i) tagList(if (i > 1) ", ", tags$a(href = "#", onclick = sprintf("Shiny.setInputValue('go_part', '%s', {priority: 'event'}); return false;", names(w)[i]), w[[i]])))) }
    div(class = "su-bar", div(class = "in", div(class = "msg", msg),
      if (!is.null(rv$cfg)) div(class = "btns",
        if (s != "s_data") actionButton("su_back", tagList(icon("arrow-left"), " Back"), class = "btn-outline-secondary"),
        if (s != "s_run") actionButton("su_next", tagList(if (s == "s_data") "Review the settings " else "Continue ", icon("arrow-right")), class = "btn-outline-primary"),
        actionButton("su_run", tagList(icon("play"), " Run"), class = "btn-primary"))))
  })
  observeEvent(input$go_part, { go_step("s_review"); accordion_panel_open("rev", input$go_part); session$sendCustomMessage("su_scroll", input$go_part) })

  output$run_summary <- renderUI({
    cfg <- cur_cfg(); req(cfg); st <- tryCatch(setup_status(), error = function(e) NULL); req(st)
    chg <- adv_chg(); pr <- input$preset %||% "standard"
    row <- function(lab, txt, s = NULL) tags$li(if (is.null(s) || s$status == "ok") icon("circle-check", class = "su-ok") else icon("triangle-exclamation", class = if (s$status == "err") "su-er" else "su-wn"),
                                          tags$b(lab, ": "), txt, if (!is.null(s) && length(s$issues)) div(class = "small-note", style = "margin-left:22px", paste(s$issues, collapse = " ")))
    tags$ul(class = "su-list",
      row("Survey", if (nzchar(trimws(input$survey_name %||% ""))) input$survey_name else "(no name: 'MICS survey')"),
      row("Period of analysis", st$period$text, st$period), row("Outputs", st$outputs$text, st$outputs), row("Datasets", st$datasets$text, st$datasets),
      row("Teams", st$teams$text, st$teams), row("Questions tested", st$flags$text, st$flags),
      row("Analysis settings", if (!nrow(chg)) "standard" else sprintf("%s - %d changed from the defaults (%s)", if (pr == "custom") "custom" else ipm_presets()[[pr]]$label, nrow(chg), paste(chg$setting, collapse = "; "))))
  })
  output$saved_note <- renderUI({
    nm <- trimws(input$survey_name %||% "")
    tags$p(class = "small-note", if (!.int$history_on()) "Settings are not remembered on this server (run history turned off)."
           else if (!nzchar(nm)) "Give the survey a name: its settings are then remembered when you run, for the next data export."
           else sprintf("The settings are remembered for '%s' when you run: with the next data export, type the same name and choose 'Use them'.", nm))
  })

  # ---------- advanced analysis: presets, changed settings ----------
  cur_opts <- reactive(adv_read(input))
  observeEvent(input$preset, { p <- input$preset; if (identical(p, "custom")) return()
    if (identical(.int$match_preset(cur_opts()), p)) return()
    set_adv(.int$preset_values(p)) }, ignoreInit = TRUE)
  observe({
    o <- cur_opts(); req(length(o) == length(PSPEC))
    m <- .int$match_preset(o) %|na|% "custom"; pend <- isolate(adv_pending())
    if (!is.null(pend) && difftime(Sys.time(), pend$t, units = "secs") < 3) { if (identical(m, pend$p)) adv_pending(NULL); return() }
    adv_pending(NULL)
    if (!identical(isolate(input$preset), m)) updateRadioButtons(session, "preset", selected = m)
  })
  adv_chg <- reactive(ipm_settings_changed(list(options = cur_opts())))
  output$preset_help <- renderUI({ p <- input$preset %||% "standard"
    tags$p(class = "small-note", if (p == "custom") "Your own settings: see below what differs from the defaults." else ipm_presets()[[p]]$help) })
  output$adv_changed <- renderUI({ x <- adv_chg(); if (!nrow(x)) return(tags$p(class = "small-note", icon("circle-check", class = "su-ok"), " All settings are the defaults."))
    div(class = "su-chg", tags$b(sprintf("%d setting%s changed from the defaults", nrow(x), if (nrow(x) > 1) "s" else "")),
        tags$ul(style = "margin:4px 0; padding-left:18px", lapply(seq_len(nrow(x)), function(i) tags$li(sprintf("%s: %s (default %s)", x$setting[i], x$value[i], x$default[i]),
          tags$a(href = "#", class = "r", onclick = sprintf("Shiny.setInputValue('adv_reset', '%s', {priority: 'event'}); return false;", x$option[i]), "reset")))),
        actionLink("adv_reset_all", "Reset all to the defaults"))
  })
  output$adv_badge <- renderUI({ n <- nrow(adv_chg()); if (n) span(class = "badge rounded-pill bg-warning text-dark", n) })
  observeEvent(input$adv_reset, { o <- cur_opts(); o[[input$adv_reset]] <- ipm_defaults()[[input$adv_reset]]; set_adv(o) })
  observeEvent(input$adv_reset_all, set_adv(list()))

  # ---------- run ----------
  observeEvent(input$run, do_run())
  do_run <- function() {
    req(rv$cfg); if (job_running("run")) { showNotification("The analysis is already running.", type = "warning"); return() }
    cfg <- collect_cfg(); rv$log <- character(); rv$report <- NULL; rv$zip <- NULL
    rv$res <- NULL; rv$sc <- NULL; rv$cmp <- NULL; rv$plan <- NULL; rv$chat <- list(); rv$reads <- list(); gc(verbose = FALSE)
    start_job("run", "run", list(cfg = cfg), "Running the IPM", if (BG_OK) "you can keep using the app; other users are not slowed down" else NULL, log = TRUE,
      done = function(r) {
        if (inherits(r, "error")) { m <- job_error(r); logm("ERROR:", m); showNotification(m, type = "error", duration = NULL); return() }
        if (is.character(r$sc)) { logm("Scorecard:", r$sc); r$sc <- NULL }
        rv$cmp <- if (!is.null(r$sc)) tryCatch(ipm_history_update(r$res, r$sc), error = function(e) { logm("Run history:", conditionMessage(e)); NULL })
        tryCatch(ipm_settings_save(cfg), error = function(e) logm("Settings not remembered:", conditionMessage(e)))
        rv$sc <- r$sc; rv$res <- r$res; logm("Done."); nav_to(if (!is.null(r$sc)) "briefing" else "results")
      })
  }
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
      tags$p(style = "color:#5f6368", "Load the latest data export, check the settings and click Run. You then see what to act on this week, and - from the second run of the same survey - what is new, what persists and what was resolved."),
      actionButton("bf_start", "Load the data", class = "btn-primary")))
    b <- brief(); res <- rv$res; ds <- res$data_summary
    ai_all <- with_act(cur_issues())
    act_sum <- function(t) { if (is.null(ai_all$action)) return(NULL)
      x <- ai_all[level != "survey" & team == t]; if (!nrow(x)) return(NULL)
      n <- c(done = sum(x$action == "done" & !x$still_found), assigned = sum(x$action == "assigned"), escalated = sum(x$action == "escalated"), still = sum(x$still_found))
      if (!sum(n)) return(div(class = "bf-acts-sum", sprintf("Follow-up: none recorded for its %d issue%s", nrow(x), if (nrow(x) > 1) "s" else "")))
      div(class = "bf-acts-sum", "Follow-up: ", paste(c(if (n[["done"]]) paste(n[["done"]], "done"), if (n[["assigned"]]) paste(n[["assigned"]], "assigned"),
                                                       if (n[["escalated"]]) paste(n[["escalated"]], "escalated")), collapse = " · "),
          if (n[["still"]]) span(class = "act act-warn", style = "margin-left:6px", sprintf("%d done but still found", n[["still"]]))) }
    still <- if (!is.null(ai_all$still_found)) ai_all[still_found == TRUE] else ai_all[0]
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
            div(class = "bf-do", icon("arrow-right"), sprintf(" %s (%s, %s)", h$do, h$owner, tolower(h$timing))),
            act_sum(h$team)),
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
        div(downloadButton("bf_html", "Briefing to share (HTML)", class = "btn-sm btn-outline-primary"),
            downloadButton("bf_plan", "Supervisor briefs (Word)", class = "btn-sm btn-outline-primary"),
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
              div(class = "bf-legend", span(style = "background:#E2231A"), "high", span(style = "background:#F28C28"), "medium",
                  span(style = "background:#F3F4F6"), "nothing found",
                  if (!is.null(b$previous)) tagList(span(style = "outline:2px solid #1f2a44;outline-offset:-2px;background:white"), "also at the last run"))),
          if (nrow(still)) div(class = "bf-card", style = "border-color:#F4B4B0", tags$h4(sprintf("Done but still found (%d)", nrow(still))),
              div(class = "hint", "Set to Done on an earlier export, still found in this one: check that the action was really carried out, or escalate."),
              lapply(seq_len(nrow(still)), function(i) { h <- still[i]
                div(class = "bf-line", tags$b(if (is.na(h$team)) "Survey" else if (h$level == "interviewer") sprintf("Int. %s", fmt_i(h$interv)) else short_lbl(h$team_label)), " ", h$headline, act_bar(h)) })),
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
  bf_issues <- reactive({ req(rv$sc); s <- with_act(cur_issues())
    s[, sev := match(severity, c("high", "medium"))]; s[, lev := match(level, c("team", "interviewer"))]; setorder(s, lev, sev); s })
  observeEvent(input$bf_team, { s <- bf_issues()[level != "survey" & team == input$bf_team]
    t <- rv$sc$teams[team == input$bf_team]
    bf_evidence(s, sprintf("%s - %s priority: %d issue%s", t$team_label, t$priority, nrow(s), if (nrow(s) == 1) "" else "s"),
                tags$button(class = "btn btn-primary", onclick = sprintf("Shiny.setInputValue('go_team', %d, {priority: 'event'})", t$team), "Open the team page")) })
  observeEvent(input$bf_iv, { s <- bf_issues()[level == "interviewer" & interv == input$bf_iv]
    bf_evidence(s, sprintf("Interviewer %s (%s)", fmt_i(input$bf_iv), s$team_label[1]),
                tags$button(class = "btn btn-primary", onclick = sprintf("Shiny.setInputValue('go_iv', %s, {priority: 'event'})", fmt_i(input$bf_iv)), "Open the interviewer page")) })
  observeEvent(input$bf_forget, showModal(modalDialog(title = "Forget this survey's history?",
    "The issues kept from earlier runs of this survey and their follow-up (assigned, done, notes) will be deleted; the next run starts a new history. Your current results stay.",
    footer = tagList(modalButton("Cancel"), actionButton("bf_forget_ok", "Forget the history", class = "btn-danger")))))
  observeEvent(input$bf_forget_ok, { removeModal(); req(rv$res); ipm_history_forget(rv$res); rv$cmp <- ipm_compare(rv$sc, list(), as.Date(NA))
    showNotification("History deleted.", type = "message") })
  output$bf_html <- downloadHandler(filename = function() sprintf("IPM_briefing_%s.html", format(Sys.Date(), "%Y%m%d")),
    content = function(f) { req(rv$sc); ipm_briefing_html(rv$res, rv$sc, f, cmp = rv$cmp, actions = acts()) })
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
      div(class = "a", icon("arrow-right"), sprintf(" %s (%s, %s)", h$action, h$owner, tolower(h$timing))),
      act_bar(h))
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
    p <- tp_prof(); req(p); t <- p$team; s <- with_act(p$issues); iv <- p$interviewers; sv <- p$survey
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
                mod_bars(p$modules, "Team", "Others")),
            ai_box("tp", sprintf("Ask about %s", short_lbl(t$team_label)), c("Why is this team a priority, and what should the supervisor do first?", "Which interviewer needs help most, and on what?")))))
  })
  output$tp_brief <- downloadHandler(filename = function() sprintf("supervisor_brief_T%s_%s.docx", input$tp_team, format(Sys.Date(), "%Y%m%d")),
    content = function(f) { req(rv$sc, input$tp_team); ipm_supervisor_brief(rv$res, rv$sc, f, teams = as.integer(input$tp_team), cmp = rv$cmp) })

  output$ip_body <- renderUI({
    if (is.null(rv$sc)) return(no_run())
    p <- ip_prof(); req(p); h <- p$interviewer; s <- with_act(p$issues); fg <- p$figures
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
                mod_bars(p$modules, "Interviewer", "All")),
            ai_box("ip", sprintf("Ask about interviewer %s", fmt_i(h$interv)), c("What should the supervisor check when observing this interviewer?", "Is this interviewer getting better or worse?")))))
  })
  observeEvent(input$ip_team, { p <- ip_prof(); req(p); go_team(p$interviewer$team) })

  # ---------- action follow-up ----------
  track_on <- function() isTRUE(.int$history_on()) && !is.null(rv$res)
  acts <- reactive({ rv$acts_tick; req(rv$res)
    if (!track_on()) return(NULL)
    tryCatch(ipm_actions(rv$res), error = function(e) NULL) })
  with_act <- function(s) {
    a <- acts(); if (is.null(a) || is.null(s) || !nrow(s)) return(s)
    ipm_action_status(s, a, .int$history_dates(rv$res)$last)
  }
  act_chip <- function(h) {
    if (is.null(h$action) || is.na(h$action)) return(NULL)
    d <- if (!is.null(h$action_updated) && !is.na(h$action_updated)) format(as.Date(h$action_updated), "%d %b") else ""
    w <- if (!is.null(h$action_who) && !is.na(h$action_who) && nzchar(h$action_who)) paste0(" ", h$action_who) else ""
    if (isTRUE(h$still_found)) return(span(class = "act act-warn", sprintf("Done on %s but still found", d)))
    switch(h$action, open = span(class = "act act-open", "Not followed up yet"),
           assigned = span(class = "act act-asg", sprintf("Assigned to%s, %s", if (nzchar(w)) w else " someone", d)),
           done = span(class = "act act-done", sprintf("Done, %s%s", d, if (nzchar(w)) paste0(" (", trimws(w), ")") else "")),
           escalated = span(class = "act act-esc", sprintf("Escalated%s, %s", if (nzchar(w)) paste0(" to", w) else "", d)), NULL)
  }
  act_bar <- function(h) {
    if (is.null(h$action) || is.null(h$key)) return(NULL)
    div(class = "actbar", act_chip(h),
        if (!is.na(h$action_note %||% NA) && nzchar(h$action_note)) span(class = "actnote", h$action_note),
        tags$button(class = "btn btn-sm btn-outline-primary", onclick = sprintf("Shiny.setInputValue('act_open', %s, {priority: 'event'})", encodeString(h$key, quote = "'")), "Follow up"))
  }
  cur_issues <- reactive({ req(rv$sc); c <- rv$cmp %||% ipm_compare(rv$sc, list(), as.Date(NA)); s <- copy(c$issues)
    if (!"key" %in% names(s)) s[, key := .int$issue_key(s)]; s })
  observeEvent(input$act_open, {
    k <- input$act_open; s <- with_act(cur_issues()[key == k]); req(nrow(s) == 1); rv$act_key <- k
    showModal(modalDialog(title = "Follow up", size = "m", easyClose = TRUE,
      div(class = "bf-head", s$headline), div(class = "small-note", style = "margin-bottom:10px", sprintf("%s · suggested: %s (%s)", s$team_label %||% "Survey", s$do %||% s$action, s$owner)),
      radioButtons("act_status", NULL, inline = TRUE, choices = stats::setNames(names(.int$action_states), .int$action_states), selected = s$action %||% "open"),
      textInput("act_who", "Who (name or role)", value = if (is.na(s$action_who %||% NA)) "" else s$action_who, width = "100%", placeholder = "e.g. supervisor T5, field coordinator"),
      textAreaInput("act_note", "Note", value = if (is.na(s$action_note %||% NA)) "" else s$action_note, width = "100%", rows = 3,
                    placeholder = "What was done or agreed, e.g. 'observed 2 interviews on 12 Nov, retrained on HC module'"),
      div(class = "small-note", "Kept with the run history of this survey and shown at the next runs; an issue set to Done that is still found at the next export is shown as such."),
      footer = tagList(modalButton("Cancel"), actionButton("act_save", "Save", class = "btn-primary"))))
  })
  observeEvent(input$act_save, {
    k <- rv$act_key; req(k, rv$res); s <- cur_issues()[key == k]
    ok <- tryCatch({ ipm_action_set(rv$res, k, input$act_status, input$act_who, input$act_note, headline = s$headline[1]); TRUE },
                   error = function(e) { showNotification(conditionMessage(e), type = "error"); FALSE })
    if (ok) { removeModal(); rv$acts_tick <- (rv$acts_tick %||% 0) + 1; showNotification("Follow-up saved.", type = "message", duration = 3) }
  })

  # ---------- questionnaire page ----------
  heat <- reactive({ req(rv$sc, rv$res); ipm_question_heat(rv$res, rv$sc, by = input$qh_by %||% "question", top = as.integer(input$qh_top %||% 40), hide_survey_wide = isTRUE(input$qh_hide)) })
  heat_col <- function(r) { if (is.na(r)) return("transparent"); if (r == 0) return("var(--heat0)")
    x <- min(r / 40, 1); grDevices::rgb(grDevices::colorRamp(c("#FDE3C8", "#F28C28", "#B3140F"))(x), maxColorValue = 255) }
  output$qh_body <- renderUI({
    if (is.null(rv$sc)) return(no_run())
    h <- heat(); if (is.null(h)) return(div(class = "bf", div(class = "bf-card", "No flag analysis in this run.")))
    rw <- h$rows; tm <- h$teams; ce <- h$cells
    key <- function(r, t) paste(r, t)
    cmap <- stats::setNames(seq_len(nrow(ce)), key(ce$row, ce$team))
    tbl <- tags$table(class = "qh",
      tags$tr(tags$th(class = "lab", if (h$by == "question") "Question" else "Module"), tags$th(class = "tot", "All teams"),
              lapply(seq_len(nrow(tm)), function(j) tags$th(class = "tm", title = sprintf("%s - %s priority: open the team page", tm$team_label[j], tm$priority[j]),
                                                         onclick = sprintf("Shiny.setInputValue('go_team', %d, {priority: 'event'})", tm$team[j]),
                                                         span(class = "dot", style = sprintf("background:%s", prio_col[[tm$priority[j]]])), short_lbl(tm$team_label[j])))),
      lapply(seq_len(nrow(rw)), function(i) { r <- rw[i]
        tags$tr(
          tags$td(class = "lab", title = r$description %||% "",
                  tags$b(if (h$by == "question") paste(r$dataset, r$question) else paste(r$dataset, r$module)),
                  if (isTRUE(r$survey_wide)) span(class = "bf-chip old", style = "margin-left:6px", "most teams"),
                  if (h$by == "question") ex_link(r$dataset, r$question),
                  if (h$by == "question" && !is.na(r$description)) div(class = "d", substr(r$description, 1, 60))),
          tags$td(class = "tot", sprintf("%.0f%%", r$rate), div(class = "d", sprintf("%d of %d teams", r$teams_flagged, r$teams))),
          lapply(seq_len(nrow(tm)), function(j) { k <- cmap[key(r$row, tm$team[j])]
            if (is.na(k)) return(tags$td(class = "c na", title = sprintf("%s: not tested", tm$team_label[j])))
            x <- ce[k]
            tags$td(class = "c", style = sprintf("background:%s", heat_col(x$rate)),
                    title = sprintf("%s - %s: %d of %d tests flagged (%.0f%%)%s", tm$team_label[j], r$row, x$flags, x$tests, x$rate,
                                    if (nzchar(x$interviewers)) paste0("; interviewers ", x$interviewers) else ""),
                    onclick = sprintf("Shiny.setInputValue('go_team', %d, {priority: 'event'})", tm$team[j])) }))
      }))
    nsw <- sum(rw$survey_wide)
    div(class = "bf",
      div(class = "bf-kpis",
          div(class = "bf-kpi", div(class = "l", "Questionnaire problems"), div(class = "v", nrow(rv$sc$signals[level == "survey"])), div(class = "s", "questions or modules flagged in most teams")),
          div(class = "bf-kpi", div(class = "l", "Shown here"), div(class = "v", nrow(rw)), div(class = "s", sprintf("%s with the most flags · %d in most teams", if (h$by == "question") "questions" else "modules", nsw))),
          div(class = "bf-kpi", div(class = "l", "Survey flag rate"), div(class = "v", sprintf("%.1f%%", rv$sc$survey$flag_rate %||% NA)), div(class = "s", "of all flag tests"))),
      div(class = "bf-card",
          tags$h4(if (h$by == "question") "Which questions are flagged in which team" else "Which modules are flagged in which team"),
          div(class = "hint", "Each cell: share of the team's tests of that question flagged (darker = more). A row dark in most teams is a questionnaire problem (wording, translation, CAPI routing): one note to all teams. A row dark in one or two teams is a team problem. Point at a cell for the figures and the interviewers flagged; click it to open the team."),
          div(class = "qh-legend", "0%", span(class = "sw", style = "background:var(--heat0)"), span(class = "sw", style = sprintf("background:%s", heat_col(5))),
              span(class = "sw", style = sprintf("background:%s", heat_col(15))), span(class = "sw", style = sprintf("background:%s", heat_col(30))),
              span(class = "sw", style = sprintf("background:%s", heat_col(40))), "40%+", span(class = "sw na", style = "margin-left:14px"), "not tested"),
          div(class = "qh-wrap", tbl)),
      ai_box("qh", "Ask about the questionnaire", c("Which questions should be checked for wording or translation first?", "Is any module a problem in only a few teams?")))
  })

  # ---------- AI on each page ----------
  ai_box <- function(prefix, title, sugg) {
    st <- tryCatch(ai_st(), error = function(e) NULL)
    div(class = "bf-card aibox", tags$h4(icon("wand-magic-sparkles"), " ", title),
      if (!isTRUE(st$ready)) div(class = "hint", "No AI model is set up: choose a connection under Results > Action plan (AI). The answers use the same aggregated figures as this page, never the records.")
      else div(class = "hint", sprintf("Answered by %s from the aggregated figures of this run (no records are sent).", st$model)),
      div(class = "sugg", lapply(sugg, function(x) tags$button(class = "btn btn-sm btn-outline-secondary",
                                                               onclick = sprintf("Shiny.setInputValue('%s_sugg', %s, {priority: 'event'})", prefix, encodeString(x, quote = "'")), x))),
      div(class = "aiq", textAreaInput(paste0(prefix, "_q"), NULL, width = "100%", rows = 2, placeholder = "Your question"),
          actionButton(paste0(prefix, "_ask"), "Ask", class = "btn-primary")),
      uiOutput(paste0(prefix, "_chat")))
  }
  ai_ctx <- function(prefix) switch(prefix,
    tp = list(key = paste0("team:", input$tp_team), lead = { t <- rv$sc$teams[team == as.integer(input$tp_team)]; sprintf("About team %s (%s): ", short_lbl(t$team_label), t$team_label) }),
    ip = list(key = paste0("iv:", input$ip_iv), lead = { p <- ip_prof(); sprintf("About interviewer %s of team %s: ", input$ip_iv, short_lbl(p$interviewer$team_label)) }),
    qh = list(key = "questionnaire", lead = "About the questionnaire (questions and modules flagged across teams): "))
  ai_page_ask <- function(prefix, q) {
    q <- trimws(q %||% ""); if (!nzchar(q)) return()
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model is set up: choose a connection under Results > Action plan (AI).", type = "warning", duration = 8); return() }
    if (job_running("ai_ask")) { showNotification("The AI is already answering a question.", type = "warning"); return() }
    cx <- ai_ctx(prefix)
    start_job("ai_ask", "ask", list(q = paste0(cx$lead, q), res = rv$res, sc = rv$sc, st = st, history = Filter(function(x) identical(x$ctx, cx$key), rv$chat),
                                    language = input$ai_lang %||% "English", compact = isTRUE(input$ai_compact)),
      sprintf("%s is answering", st$model), if (st$provider == "ollama") "local models re-read the whole evidence: 1-5 minutes" else NULL,
      done = function(a) {
        if (inherits(a, "error")) { showNotification(job_error(a), type = "error", duration = NULL); return() }
        rv$chat <- c(rv$chat, list(list(q = q, a = a$answer, team = a$team, ctx = cx$key))); updateTextAreaInput(session, paste0(prefix, "_q"), value = "") })
  }
  for (px in c("tp", "ip", "qh")) local({ px <- px
    observeEvent(input[[paste0(px, "_ask")]], ai_page_ask(px, input[[paste0(px, "_q")]]))
    observeEvent(input[[paste0(px, "_sugg")]], ai_page_ask(px, input[[paste0(px, "_sugg")]]))
    output[[paste0(px, "_chat")]] <- renderUI({
      k <- tryCatch(ai_ctx(px)$key, error = function(e) "")
      ch <- Filter(function(x) identical(x$ctx, k), rv$chat); if (!length(ch)) return(NULL)
      tagList(lapply(rev(ch), function(x) tagList(div(class = "chatq", x$q), div(class = "chata", md_html(x$a)))))
    })
  })

  # ---------- AI reading of one table or graph (only its figures are sent) ----------
  ai_read <- function(key, title, x, how = NULL) {
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model is set up: choose a connection under Results > Action plan (AI).", type = "warning", duration = 8); return() }
    if (job_running("ai_read")) { showNotification("The AI is already reading a table or graph.", type = "warning"); return() }
    start_job("ai_read", "read", list(title = title, x = x, how = how, st = st, language = input$ai_lang %||% "English"),
      sprintf("%s is reading '%s'", st$model, substr(title, 1, 60)), if (st$provider == "ollama") "local models: up to a few minutes" else NULL,
      done = function(a) {
        if (inherits(a, "error")) { showNotification(job_error(a), type = "error", duration = NULL); return() }
        rv$reads[[key]] <- a })
  }
  read_box <- function(key) {
    a <- rv$reads[[key]]; if (is.null(a)) return(NULL)
    div(class = "bf-card aibox", tags$h4(icon("wand-magic-sparkles"), " AI reading"),
        div(class = "hint", sprintf("By %s from the figures of this %s only (no records). Signals to verify, not proof.", a$model, if (key == "dsum") "page" else "graph")),
        div(class = "chata", md_html(a$answer)))
  }
  observeEvent(input$fig_read, {
    req(input$fig_pick); f <- figs()[[input$fig_pick]]; req(f)
    ai_read(paste0("fig:", input$fig_pick), input$fig_pick, f$plot, f$how)
  })
  output$fig_read_out <- renderUI({ req(input$fig_pick); read_box(paste0("fig:", input$fig_pick)) })
  observeEvent(input$dsum_read, {
    res <- rv$res; req(res); q <- res$outputs$quesperday
    x <- list(records_dates_interviewers_by_dataset = res$data_summary)
    if (!is.null(q) && length(q$tables)) x$completed_questionnaires_by_day_and_team <- q$tables[[1]]
    ai_read("dsum", "Data and questionnaires: records, dates and interviewers per dataset; completed questionnaires per team and day", x,
            "Check that all datasets were analysed, that the dates and number of interviewers look right, and which teams worked on which days.")
  })
  output$dsum_read_out <- renderUI(read_box("dsum"))

  # ---------- run from the data page ----------
  observeEvent(input$run_quick, { nav_to("run"); do_run() })

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
  # endpoint filled in for the chosen provider, unless the user typed their own
  u_urls <- c(anthropic = "https://api.anthropic.com", ollama = "http://localhost:11434", openai = "https://api.openai.com/v1")
  observeEvent(input$u_prov, {
    cur <- trimws(input$u_base %||% "")
    if (!nzchar(cur) || cur %in% u_urls) updateTextInput(session, "u_base", value = u_urls[[input$u_prov]])
  })
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
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (Setup > Run): the AI works from its results.", type = "warning", duration = 8); return() }
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
        tags$p(class = "small-note", what),
        downloadButton(id, "Download this request (JSON)", class = "btn-sm"),
        tags$pre(class = "json", style = "margin-top:8px", pv$json))
    }
    showModal(modalDialog(title = "Exactly what is sent to the AI model", size = "xl", easyClose = TRUE,
      tags$p(class = "small-note", "This is the full request, built by the same code that sends it: the instructions (system prompt), the evidence (aggregated statistics only - no record-level data, household identifiers or names), the request and, for the plan, the required answer format. The API key is sent in a header and is never shown."),
      navset_tab(
        nav_panel("Action plan request", one(rv$prev$plan, "prev_dl_plan", "Sent when you click 'Generate AI action plan'.")),
        nav_panel("Question request", one(rv$prev$question, "prev_dl_q",
          "Sent when you click 'Ask', here with the question in the box (or an example). Naming a team adds its flag tables; the last 4 questions and answers of the session are included."))),
      footer = modalButton("Close")))
  })
  output$prev_dl_plan <- downloadHandler(filename = function() "ipm_ai_request_plan.json", content = function(f) writeLines(rv$prev$plan$json, f))
  output$prev_dl_q <- downloadHandler(filename = function() "ipm_ai_request_question.json", content = function(f) writeLines(rv$prev$question$json, f))
  # before sending: size, cost and time of the request, which teams to include, which model to use
  model_advice <- function(st) {
    m <- st$model %||% ""
    if (st$provider == "anthropic") {
      rec <- "Recommended: claude-sonnet-5-5 - good plans, fast and low cost. claude-opus-5-5 for the most careful plan (about twice the cost, slower). claude-haiku-5-5 is the cheapest but its plans are thinner. Older models (opus-4, sonnet-4, opus-5) cost more for less."
      note <- if (startsWith(m, "claude-sonnet-5-5")) "You are using the recommended model." else if (startsWith(m, "claude-opus-5-5")) "You are using the most careful model." else sprintf("You are using %s.", m)
      paste(note, rec)
    } else if (st$provider == "ollama") "Local models: tick 'Compact evidence' and use a model of 14B or more (e.g. qwen2.5:14b); smaller models often miss teams or invent numbers."
    else if (st$provider == "openai") "Use the provider's most capable model for the plan; tick 'Compact evidence' for small or local models."
    else ""
  }
  output$ai_model_hint <- renderUI({ st <- ai_st(); a <- model_advice(st); if (nzchar(a)) div(class = "small-note", style = "margin-top:-6px; margin-bottom:8px", a) })
  plan_scope <- function(x) switch(x %||% "hm", h = "High", hm = c("High", "Medium"), all = NULL)
  observeEvent(input$ai_go, {
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (Setup > Run): the AI works from its results.", type = "warning", duration = 8); return() }
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model ready: choose a connection and a model, or use the rule-based plan.", type = "warning"); return() }
    n <- table(factor(rv$sc$teams$priority, levels = c("High", "Medium", "Low")))
    showModal(modalDialog(title = sprintf("Send the action plan request to %s?", st$model), size = "m", easyClose = TRUE,
      radioButtons("ai_scope", "Teams to include", width = "100%", selected = if (n[["High"]] + n[["Medium"]] > 10) "h" else "hm",
                   choices = stats::setNames(c("hm", "h", "all"), c(sprintf("High and Medium priority teams (%d)", n[["High"]] + n[["Medium"]]),
                                                                    sprintf("High priority teams only (%d) - smaller, quicker, cheaper", n[["High"]]),
                                                                    sprintf("All teams (%d) - the model also looks at Low priority teams", sum(n))))),
      uiOutput("ai_est"),
      div(class = "small-note", style = "margin-top:10px", model_advice(st)),
      footer = tagList(modalButton("Cancel"), actionButton("ai_send", "Send", class = "btn-primary"))))
  })
  output$ai_est <- renderUI({
    req(input$ai_scope, rv$res, rv$sc); st <- ai_st()
    e <- ipm_ai_estimate(rv$res, rv$sc, st, language = input$ai_lang %||% "English", compact = isTRUE(input$ai_compact), priorities = plan_scope(input$ai_scope))
    k <- function(x) format(round(x, -2), big.mark = ",")
    mins <- function(m) if (m[2] < 1) "under a minute" else sprintf("%s to %s minutes", if (m[1] < 1) "under 1" else round(m[1]), ceiling(m[2]))
    big <- e$output_max > 24000
    div(class = "bf-card", style = "padding:10px 14px",
      tags$b(sprintf("%d team(s) in the plan", e$teams)), tags$br(),
      sprintf("Sent: about %s tokens (the aggregated figures, no records). Answer: about %s tokens, up to %s if the model thinks at length.", k(e$input_tokens), k(e$output_tokens), k(e$output_max)), tags$br(),
      if (e$local) sprintf("Cost: none (local model). Time: %s.", mins(e$minutes))
      else sprintf("Cost: %s. Time: %s.", if (is.na(e$usd[1])) "not known for this model" else sprintf("about $%.2f, at most $%.2f (list prices)", e$usd[1], e$usd[2]), mins(e$minutes)),
      if (big) div(class = "aiwarn", style = "margin-top:6px", "This is a large request: it may be slow or be cut off. Choose 'High priority teams only' or tick 'Compact evidence'."))
  })
  observeEvent(input$ai_send, {
    removeModal(); st <- ai_st(); req(isTRUE(st$ready))
    start_job("ai_plan", "plan", list(res = rv$res, sc = rv$sc, st = st, language = input$ai_lang, compact = isTRUE(input$ai_compact), priorities = plan_scope(input$ai_scope)),
      sprintf("%s is writing the action plan", st$model), if (st$provider == "ollama") "local models can take several minutes" else "1 to 5 minutes",
      done = function(p) {
        if (inherits(p, "error")) { showNotification(job_error(p), type = "error", duration = NULL); return() }
        rv$plan <- p; r <- rv$res; r$action_plan <- p; rv$res <- r })
  })
  observeEvent(input$rules_go, { if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (Setup > Run): the AI works from its results.", type = "warning", duration = 8); return() }; rv$plan <- ipm_plan_rules(rv$sc); r <- rv$res; r$action_plan <- NULL; rv$res <- r })
  output$plan_ui <- renderUI({
    p <- rv$plan; req(p)
    li <- function(x) tags$ul(lapply(x, tags$li))
    team_block <- function(t) {
      col <- prio_col[[t$priority %||% "Low"]] %||% cyan
      div(class = "teamplan", style = paste0("border-left-color:", col),
        tags$h5(sprintf("Team %s ", t$team), span(class = "prio", style = paste0("background:", col), toupper(t$priority))),
        div(t$headline),
        if (length(t$issues)) li(lapply(t$issues, function(x) tagList(tags$b(sub("[.][[:space:]]*$", "", x$issue %||% "")), ": ", x$evidence))),
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
      tags$h5(style = "color:#1CABE2; font-weight:bold", "Overall assessment"), tags$p(p$overall_assessment),
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
    if (is.null(rv$res) || is.null(rv$sc)) { showNotification("Run the analysis first (Setup > Run): the AI works from its results.", type = "warning", duration = 8); return() }
    st <- ai_st()
    if (!isTRUE(st$ready)) { showNotification("No AI model configured.", type = "warning"); return() }
    start_job("ai_ask", "ask", list(q = q, res = rv$res, sc = rv$sc, st = st, history = rv$chat, language = input$ai_lang, compact = isTRUE(input$ai_compact)),
      sprintf("%s is answering", st$model), if (st$provider == "ollama") "local models re-read the whole evidence: 1-5 minutes" else NULL,
      done = function(a) {
        if (inherits(a, "error")) { showNotification(job_error(a), type = "error", duration = NULL); return() }
        rv$chat <- c(rv$chat, list(list(q = q, a = a$answer, team = a$team))); updateTextAreaInput(session, "ai_q", value = "") })
  })
  output$chat <- renderUI({
    ch <- rv$chat; if (!length(ch)) return(tags$p(class = "small-note", "Answers use the same aggregated evidence as the plan (run the analysis first); when you name a team (e.g. 'T4' or 'team 4'), its flag tables (answer counts per interviewer) are added. Local models take 1-5 minutes per answer: watch the progress bar at the bottom right."))
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
    tagList(tags$p(class = "small-note", sprintf("%d graph(s) of '%s'%s.", length(nm), f$group, if (length(nm) > 40) " - first 40 shown" else "")), imgs)
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
    np <- fig_pt(); if (is.null(np)) return(tags$p(class = "small-note", "Point at (or tap) a circle to see the flag table of that interviewer and question."))
    tb <- Filter(function(z) z$dataset == np$dataset[1] && z$variable == np$varname[1] && z$interv == np$interv[1] && z$team == np$team[1], rv$res$outputs$flaganal$tables)
    if (!length(tb)) return(tags$p(class = "small-note", sprintf("Interviewer %s, question %s: flag table not available.", fmt_i(np$interv[1]), np$varname[1])))
    z <- tb[[1]]
    card(card_header(z$title), tags$p(class = "small-note", z$desc, " ", ex_link(z$dataset, z$variable, "Open in Data explorer")), div(class = "pattern", z$pattern), renderTable(z$table, digits = 0))
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
    card(card_header(z$title), tags$p(class = "small-note", z$desc, " ", ex_link(z$dataset, z$variable, "Open in Data explorer")), div(class = "pattern", z$pattern), renderTable(z$table, digits = 0))
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
      tags$p(class = "small-note", sprintf("%d flagged questions%s.", length(g), if (length(g) > 80) " - first 80 shown; download the Word file for all" else "")),
      lapply(shown, function(x) card(card_header(x$title),
        tags$p(class = "small-note", x$desc),
        lapply(seq_along(x$readings), function(k) div(class = "pattern", sprintf("p=%s: %s", signif(x$p[k], 2), x$readings[k]))),
        if (length(x$interviewers) > 1) tags$p(class = "small-note", "Several interviewers of this team are flagged on this question: their answers differ from each other (question possibly asked or understood differently within the team)."),
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

  # ---------- data explorer ----------
  ex_cfg <- reactive({ if (!is.null(rv$res)) rv$res$cfg else { req(rv$cfg); tryCatch(collect_cfg(), error = function(e) rv$cfg) } })
  ex_vars <- function(ds) {                         # variables offered: no long text (names), no utility variables
    m <- rv$light$meta[[ds]]; if (is.null(m)) return(character())
    v <- names(m$varlab); w <- suppressWarnings(as.integer(sub("^A", "", m$fmt[v]))); txt <- grepl("^A", m$fmt[v]) & !is.na(w) & w > 1
    v <- v[!txt & !v %in% c(toupper(.int$utility_vars), "DUMMY")]; l <- m$varlab[v]
    stats::setNames(v, ifelse(nzchar(l), paste0(v, " - ", substr(l, 1, 70)), v))
  }
  observe({
    cfg <- rv$cfg; req(cfg)
    ds <- names(cfg$datasets)[vapply(names(cfg$datasets), function(n) !is.null(rv$light$meta[[n]]), logical(1))]
    lab <- vapply(ds, function(n) { f <- cfg$datasets[[n]]$file %||% n; sprintf("%s (%s)", n, f) }, "")
    sel <- isolate(input$ex_ds); updateSelectInput(session, "ex_ds", choices = stats::setNames(ds, lab), selected = if (!is.null(sel) && sel %in% ds) sel else ds[1])
  })
  ex_weights <- function(ds) {                      # sample weight variables of a dataset (HHWEIGHT, WMWEIGHT, poids...)
    m <- rv$light$meta[[ds]]; if (is.null(m)) return(character())
    v <- names(m$varlab); l <- m$varlab[v]; txt <- grepl("^A", m$fmt[v])
    v[!txt & (grepl("WEIGHT$|WGT$", v) | grepl("sample weight|sampling weight|poids d.?.chantillon|poids de sondage|pond.ration|factor de expansi|peso muestral", l, ignore.case = TRUE))]
  }
  output$ex_weight_ui <- renderUI({
    req(input$ex_ds); wv <- ex_weights(input$ex_ds)
    if (!length(wv)) return(div(class = "small-note", style = "margin:-4px 0 10px 0", "No weight variable in this dataset (weights are usually added after fieldwork): figures are per record."))
    lb <- rv$light$meta[[input$ex_ds]]$varlab[wv]
    div(selectInput("ex_weight", "Weight", c("No weight (records)" = "none", stats::setNames(wv, ifelse(nzchar(lb), paste0(wv, " - ", substr(lb, 1, 40)), wv))),
                    selected = isolate(input$ex_weight) %||% "none", width = "100%"),
        div(class = "small-note", style = "margin:-10px 0 10px 0", "Weighted: % and means represent the population. To check the interviewers' work, use no weight (each record counts once)."))
  })
  ex_pending <- reactiveVal(NULL)                   # variable to select after the dataset changes ("Open in Data explorer")
  observeEvent(input$ex_ds, {
    req(input$ex_ds); ch <- ex_vars(input$ex_ds); p <- ex_pending()
    s <- if (!is.null(p) && p %in% ch) p else ""; ex_pending(NULL)
    updateSelectizeInput(session, "ex_var", choices = c("(choose)" = "", ch), selected = s, server = TRUE)
    updateSelectizeInput(session, "ex_byvar", choices = c("(choose)" = "", ch), selected = "", server = TRUE)
  })
  observe({
    cfg <- ex_cfg(); res <- rv$res
    ti <- if (!is.null(res$team_info)) res$team_info else data.table(team = seq_along(cfg$teams$ranges %||% character()), team_label = sprintf("T%d", seq_along(cfg$teams$ranges %||% character())))
    updateSelectizeInput(session, "ex_teams", choices = stats::setNames(ti$team, ti$team_label), selected = isolate(input$ex_teams))
    if (!is.null(cfg$period$start)) { pb <- .int$period_bounds(cfg); updateDateRangeInput(session, "ex_dates", start = as.Date(pb$start), end = as.Date(pb$end)) }
  })
  ex_cache <- reactiveVal(NULL)
  ex_data <- reactive({
    ds <- input$ex_ds; v <- input$ex_var; req(ds, nzchar(v %||% ""))
    need <- c(v, if (identical(input$ex_by, "var") && nzchar(input$ex_byvar %||% "")) input$ex_byvar,
              if ((input$ex_weight %||% "none") %in% ex_weights(ds)) input$ex_weight)
    cfg <- ex_cfg(); c0 <- isolate(ex_cache())
    if (!is.null(c0) && identical(c0$ds, ds) && identical(c0$key, cfg_key(cfg)) && all(need %in% c0$vars)) return(c0$ed)
    vars <- unique(c(need, if (!is.null(c0) && identical(c0$ds, ds)) utils::head(c0$vars, 8)))
    ed <- withProgress(message = sprintf("Reading %s from %s...", paste(need, collapse = ", "), ds), value = 0.4,
                       tryCatch(ipm_explore_data(cfg, ds, vars), error = function(e) e))
    if (inherits(ed, "error")) { showNotification(conditionMessage(ed), type = "error", duration = 10); req(FALSE) }
    ex_cache(list(ds = ds, vars = vars, ed = ed, key = cfg_key(cfg))); ed
  })
  cfg_key <- function(cfg) paste(cfg$data_dir, paste(cfg$teams$ranges, collapse = ","), cfg$teams$variable %||% "", cfg$options$woi_anchor %||% "")
  observe({                                         # interviewers of the dataset read
    ed <- ex_data(); il <- unique(ed$x[!is.na(interv), .(interv, interv_label)])[order(interv)]
    updateSelectizeInput(session, "ex_intervs", choices = stats::setNames(il$interv, il$interv_label), selected = isolate(input$ex_intervs), server = TRUE)
  })
  ex_tab <- reactive({
    ed <- ex_data(); v <- input$ex_var; req(v %in% names(ed$x))
    by <- input$ex_by %||% "none"; if (by == "var" && !nzchar(input$ex_byvar %||% "")) by <- "none"
    pr <- input$ex_period %||% "all"; cfg <- ex_cfg()
    fr <- to <- NULL
    if (pr == "period" && !is.null(cfg$period$start)) { pb <- .int$period_bounds(cfg); fr <- pb$start; to <- pb$end }
    if (pr == "custom" && length(input$ex_dates) == 2) { fr <- input$ex_dates[1]; to <- input$ex_dates[2] }
    tryCatch(ipm_tabulate(ed, v, by = by, by_var = input$ex_byvar, completed = isTRUE(input$ex_completed), from = fr, to = to,
                          teams = input$ex_teams, intervs = input$ex_intervs, type = input$ex_type %||% "auto", skip_blank = !isFALSE(input$ex_blank),
                          by_groups = as.integer(input$ex_bins %||% 5),
                          weight = if (!(input$ex_weight %||% "none") %in% c("none", "") && input$ex_weight %in% names(ed$x)) input$ex_weight),
             error = function(e) { showNotification(conditionMessage(e), type = "error"); NULL })
  })
  fmt_n <- function(x, d = 1) ifelse(is.na(x), "", formatC(x, format = "f", digits = d, big.mark = ","))
  ex_tables <- reactive({                           # the tables shown (and downloaded / sent to the AI)
    t <- ex_tab(); req(t); out <- list()
    if (!is.null(t$freq)) out$frequencies <- data.frame(Value = ifelse(is.na(t$freq$value), "", as.character(t$freq$value)), Label = ifelse(is.na(t$freq$label), "", t$freq$label),
                                                        N = t$freq$n, `%` = round(t$freq$pct, 1), `Cumulative %` = round(t$freq$cum_pct, 1), check.names = FALSE)
    if (!is.null(t$freq) && !is.null(t$weight)) names(out$frequencies)[4:5] <- c("% (weighted)", "Cumulative % (weighted)")
    if (!is.null(t$stats)) out$summary <- as.data.frame(t$stats[, lapply(.SD, function(z) round(z, 2))])
    if (!is.null(t$special)) out$special_codes <- as.data.frame(t$special)
    if (!is.null(t$digits)) out$last_digit <- data.frame(Digit = t$digits$digit, N = t$digits$n, `%` = round(t$digits$pct, 1), check.names = FALSE)
    if (!is.null(t$cross)) {
      w <- data.table::dcast(t$cross, group + key ~ value_label, value.var = "pct", fill = 0)
      w <- w[order(key)]; n <- t$groups[match(w$key, t$groups$key)]$n
      w[, key := NULL]; nm <- setdiff(names(w), "group"); w[, (nm) := lapply(.SD, round, 1), .SDcols = nm]
      out$by_group <- data.frame(stats::setNames(w[, "group"], t$by_label), N = n, as.data.frame(w[, nm, with = FALSE]), check.names = FALSE)
    }
    if (!is.null(t$num_by)) { cs <- copy(t$num_by)[order(key)]; cs[, key := NULL]
      nm <- setdiff(names(cs), c("group", "n")); dg <- if (max(abs(unlist(cs[, nm, with = FALSE])), na.rm = TRUE) < 10) 2 else 1
      cs[, (nm) := lapply(.SD, round, dg), .SDcols = nm]
      if (all(is.na(cs$pct_0_5))) cs[, pct_0_5 := NULL]
      old <- c("group", "n", "mean", "sd", "min", "p5", "median", "p95", "max", "pct_0_5"); new <- c(t$by_label, "N", "Mean", "SD", "Min", "5th pct", "Median", "95th pct", "Max", "% ending in 0 or 5")
      setnames(cs, intersect(old, names(cs)), new[old %in% names(cs)]); out$by_group <- as.data.frame(cs) }
    out
  })
  ex_dt <- function(df, pct_cols = NULL) DT::datatable(df, rownames = FALSE, class = "compact stripe", options = list(dom = if (nrow(df) > 15) "tip" else "t", pageLength = 15, scrollX = TRUE))
  output$ex_body <- renderUI({
    if (is.null(rv$cfg)) return(div(class = "bf-empty", tags$h4("No data loaded"), tags$p("Load the survey data under Setup first: the explorer reads the variables from those files.")))
    if (!nzchar(input$ex_var %||% "")) return(tags$p(class = "small-note", style = "margin-top:20px", "Choose a dataset and a variable on the left."))
    t <- ex_tab(); req(t)
    filt <- c(if (isTRUE(input$ex_completed)) "completed interviews" else "all records",
              switch(input$ex_period %||% "all", all = "whole fieldwork", period = paste("period of analysis", tryCatch(.int$period_bounds(ex_cfg())$label, error = function(e) "")),
                     custom = paste(format(input$ex_dates, "%d %b %Y"), collapse = " - ")),
              if (length(input$ex_teams)) paste(length(input$ex_teams), "team(s)"), if (length(input$ex_intervs)) paste(length(input$ex_intervs), "interviewer(s)"))
    head <- div(class = "bf-card", tags$h4(style = "margin:0", t$var, if (nzchar(t$label)) tags$span(style = "font-weight:normal", paste("-", t$label))),
                div(class = "small-note", sprintf("%s records (%s)%s; %s.", format(t$n, big.mark = ","), paste(filt, collapse = ", "),
                                                  paste0(if (!isFALSE(input$ex_blank) && t$blank) sprintf("; %s blank records left out (question not asked)", format(t$blank, big.mark = ",")) else "",
                                                         if (!is.null(t$weight)) sprintf("; %% and means weighted with %s (N = records)%s", t$weight,
                                                                                         if (t$no_weight) sprintf(", %s records without a weight left out", format(t$no_weight, big.mark = ",")) else "") else ""),
                                                  if (t$type == "numeric") "shown as numbers" else "shown as categories")),
                div(style = "margin-top:8px", downloadButton("ex_dl", "Download the tables (Excel)", class = "btn-sm"),
                    actionButton("ex_read", tagList(icon("wand-magic-sparkles"), "Read with AI"), class = "btn-sm btn-outline-primary")))
    if (!t$n) return(tagList(head, tags$p("No record with these filters.")))
    tb <- ex_tables()
    main <- if (t$type == "categorical") layout_columns(col_widths = c(6, 6),
        card(card_header("Frequencies"), DT::renderDT(ex_dt(tb$frequencies)),
             if (any(t$freq$special)) div(class = "small-note", sprintf("Don't know, missing, refused, 'other' and blanks (orange in the graph): %.1f%% of records.", sum(t$freq$pct[t$freq$special])))),
        card(card_header("Distribution"), renderPlot(ipm_tab_plot(t, "dist"), height = max(260, 24 * min(40, nrow(t$freq)) + 90))))
      else tagList(
        layout_columns(col_widths = c(6, 6),
          card(card_header("Summary"), tableOutput("ex_sum"),
               if (!is.null(tb$special_codes)) div(class = "small-note", "Codes labelled don't know / missing / other, left out of the summary:", tableOutput("ex_spec"))),
          card(card_header("Histogram"), renderPlot(ipm_tab_plot(t, "dist"), height = 280))),
        if (!is.null(t$digits)) card(card_header("Last digit (heaping)"), renderPlot(ipm_tab_plot(t, "digits"), height = 250),
             div(class = "small-note", "For ages, weights, heights, times and amounts, each last digit should appear about 10% of the time. Many more 0s and 5s mean the values were rounded or guessed (heaping).")))
    byp <- if (t$by != "none" && !is.null(t$groups)) {
      ng <- nrow(t$groups)
      card(card_header(paste("By", (if (t$by == "var") t$by_label else tolower(t$by_label)))),
           if (!is.null(t$binned)) div(class = "small-note", style = "margin-bottom:6px", icon("circle-info"),
             sprintf(" %s is a number with many values: it is grouped in %d %ss of about equal size (choose the number of groups on the left). To see the average, minimum and maximum of %s for each answer of %s instead, use 'Swap the two variables'.",
                     t$binned$var, t$binned$groups, tolower(t$binned$name), t$binned$var, t$var)),
           if (is.null(t$test)) div(class = "small-note", style = "margin-bottom:6px", "No test: there is only one group (or one answer) with records.")
           else div(class = "small-note", style = "margin-bottom:6px",
             sprintf("%s test of the differences between the %d groups%s: %s. %s", t$test$name, ng, if (!is.null(t$weight)) " (on the records, unweighted)" else "", if (t$test$p < 1e-4) "p < 0.0001" else paste("p =", signif(t$test$p, 2)),
                     if (t$test$p < 0.001) "The groups differ more than chance would explain: look at the groups far from the others."
                     else if (t$test$p < 0.05) "Some difference between the groups, but it could be chance (the flag analysis uses p < 0.001)." else "No clear difference between the groups.")),
           renderPlot(ipm_tab_plot(t, "by"), height = max(260, 22 * min(ng, 60) + 110)),
           DT::renderDT(ex_dt(tb$by_group)))
    }
    tagList(head, uiOutput("ex_read_out"), main, byp)
  })
  output$ex_sum <- renderTable({ t <- ex_tab(); req(t$stats); s <- t$stats
    data.frame(` ` = c("Values", "Blank", "Mean", "SD", "Min", "5th percentile", "Median", "95th percentile", "Max"),
               Value = c(fmt_n(s$n, 0), fmt_n(t$blank, 0), fmt_n(s$mean, 2), fmt_n(s$sd, 2), fmt_n(s$min, 2), fmt_n(s$p5, 2), fmt_n(s$median, 2), fmt_n(s$p95, 2), fmt_n(s$max, 2)), check.names = FALSE) })
  output$ex_spec <- renderTable({ t <- ex_tab(); req(t$special); as.data.frame(t$special) })
  output$ex_dl <- downloadHandler(filename = function() sprintf("explorer_%s_%s_%s.xlsx", input$ex_ds, input$ex_var, format(Sys.Date(), "%Y%m%d")),
    content = function(f) { tb <- ex_tables(); wb <- openxlsx::createWorkbook()
      for (n in names(tb)) { openxlsx::addWorksheet(wb, substr(n, 1, 31)); openxlsx::writeData(wb, substr(n, 1, 31), tb[[n]]) }
      openxlsx::saveWorkbook(wb, f, overwrite = TRUE) })
  observeEvent(input$ex_read, {
    t <- ex_tab(); req(t)
    ai_read(paste0("ex:", input$ex_ds, ":", t$var), sprintf("Distribution of %s (%s) in the %s dataset%s", t$var, t$label, input$ex_ds,
                                                             if (t$by != "none") paste(", by", (if (t$by == "var") t$by_label else tolower(t$by_label))) else ""),
            c(ex_tables(), list(test = t$test)),
            "Say what stands out: unusual answers, many don't know / missing, heaping, and groups (teams, interviewers, weeks) far from the others. Say what a supervisor should check.")
  })
  output$ex_read_out <- renderUI({ t <- ex_tab(); req(t); read_box(paste0("ex:", input$ex_ds, ":", t$var)) })
  observeEvent(input$ex_swap, {                    # the break-down variable becomes the variable, and the other way round
    a <- input$ex_var; b <- input$ex_byvar; req(nzchar(a %||% ""), nzchar(b %||% ""))
    ch <- c("(choose)" = "", ex_vars(input$ex_ds))
    updateSelectizeInput(session, "ex_var", choices = ch, selected = b, server = TRUE)
    updateSelectizeInput(session, "ex_byvar", choices = ch, selected = a, server = TRUE)
  })
  open_explorer <- function(ds, var) {             # from the flag explorer and the Questionnaire page
    req(ds, var); ex_pending(var)
    if (identical(input$ex_ds, ds)) { ex_pending(NULL); updateSelectizeInput(session, "ex_var", choices = c("(choose)" = "", ex_vars(ds)), selected = var, server = TRUE) }
    else updateSelectInput(session, "ex_ds", selected = ds)
    bslib::nav_select("nav", "explore")
  }
  observeEvent(input$ex_open, { o <- input$ex_open; open_explorer(o$ds, o$var) })

  # ---------- field check tables (MICS7 FCT) ----------
  FSPEC <- .int$fct_param_spec()
  fct_data <- reactiveVal(NULL)                     # data read for the tables: list(fd, key)
  fct_keep <- reactiveVal(NULL)                     # parameters remembered with the survey settings
  fct_par <- reactiveVal(ipm_fct_defaults())        # parameters applied to the tables
  observeEvent(rv$cfg, {                            # new data or settings: their FCT parameters
    k <- rv$cfg$fct; fct_keep(k); fct_par(utils::modifyList(ipm_fct_defaults(), k %||% list()))
    if (!is.null(fct_data()) && !identical(fct_data()$key, cfg_key(rv$cfg))) fct_data(NULL)
  }, ignoreNULL = FALSE)
  output$fct_read_ui <- renderUI({
    f <- fct_data()
    tagList(actionButton("fct_read", tagList(icon(if (is.null(f)) "play" else "rotate"), if (is.null(f)) " Read the data" else " Read the data again"),
                         class = if (is.null(f)) "btn-primary w-100" else "btn-sm btn-outline-primary w-100"),
            div(class = "small-note", style = "margin:6px 0 12px 0",
                if (is.null(f)) "Reads the variables of the tables from the data files (seconds to a minute)."
                else sprintf("Read at %s: %s.", format(f$fd$read_at, "%H:%M"), paste(sprintf("%s %s", format(vapply(f$fd$ds, nrow, 0L), big.mark = ","), names(f$fd$ds)), collapse = ", "))))
  })
  fct_do_read <- function() {
    if (is.null(rv$cfg)) { showNotification("Load the survey data under Setup first.", type = "warning"); return() }
    cfg <- ex_cfg()
    fd <- withProgress(message = "Reading the data for the field check tables...", value = 0.4, tryCatch(ipm_fct_data(cfg), error = function(e) e))
    if (inherits(fd, "error")) { showNotification(conditionMessage(fd), type = "error", duration = 10); return() }
    fct_data(list(fd = fd, key = cfg_key(cfg)))
  }
  observeEvent(input$fct_read, fct_do_read())
  observeEvent(input$fct_read2, fct_do_read())
  observe({
    f <- fct_data(); cfg <- ex_cfg()
    ti <- if (!is.null(f)) f$fd$team_info else if (!is.null(rv$res$team_info)) rv$res$team_info else NULL
    if (!is.null(ti)) updateSelectizeInput(session, "fct_teams", choices = stats::setNames(ti$team, ti$team_label), selected = isolate(input$fct_teams))
    if (!is.null(cfg$period$start)) { pb <- .int$period_bounds(cfg); updateDateRangeInput(session, "fct_dates", start = as.Date(pb$start), end = as.Date(pb$end)) }
  })
  fct_res <- reactive({
    f <- fct_data(); req(f); p <- fct_par()
    pr <- input$fct_period %||% "all"; cfg <- ex_cfg(); fr <- to <- NULL
    if (pr == "period" && !is.null(cfg$period$start)) { pb <- .int$period_bounds(cfg); fr <- pb$start; to <- pb$end }
    if (pr == "custom" && length(input$fct_dates) == 2) { fr <- input$fct_dates[1]; to <- input$fct_dates[2] }
    if (!nzchar(p$survey_label %||% "")) p$survey_label <- trimws(input$survey_name %||% cfg$survey_name %||% "")
    withProgress(message = "Building the field check tables...", value = 0.5,
      tryCatch(ipm_fct(f$fd, p, rows = input$fct_rows %||% "team", from = fr, to = to, teams = input$fct_teams),
               error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 10); NULL }))
  })
  observe({                                          # table picker, grouped by section
    fc <- fct_res(); req(fc)
    tb <- fc$tables; sec <- vapply(tb, `[[`, "", "section")
    ch <- lapply(split(names(tb), factor(sec, levels = unique(sec))), function(ids)
      stats::setNames(ids, vapply(tb[ids], function(t) paste0(t$title, if (!isTRUE(t$available)) " (not in the data)" else ""), "")))
    s <- isolate(input$fct_pick); updateSelectInput(session, "fct_pick", choices = ch, selected = if (!is.null(s) && s %in% names(tb)) s else names(tb)[1])
  })
  observeEvent(input$fct_open, { updateSelectInput(session, "fct_pick", selected = input$fct_open); bslib::nav_select("fct_tabs", "tab") })
  fct_step <- function(k) { fc <- fct_res(); req(fc); ids <- names(fc$tables); i <- match(input$fct_pick, ids)
    if (!is.na(i) && i + k >= 1 && i + k <= length(ids)) updateSelectInput(session, "fct_pick", selected = ids[i + k]) }
  observeEvent(input$fct_prev, fct_step(-1)); observeEvent(input$fct_next, fct_step(1))

  # summary: tables x teams, target met or not
  output$fct_sum <- renderUI({
    if (is.null(rv$cfg)) return(div(class = "bf-empty", tags$h4("No data loaded"), tags$p("Load the survey data under Setup first: the field check tables are produced from those files.")))
    if (is.null(fct_data())) return(div(class = "bf-empty", tags$h4(icon("clipboard-check"), " Field check tables"),
      tags$p("The MICS7 standard Field Check Tables (F1 to F20), by team or interviewer, with the targets not met in red."),
      actionButton("fct_read2", tagList(icon("play"), " Read the data and build the tables"), class = "btn-primary")))
    fc <- fct_res(); req(fc)
    tb <- fc$tables; av <- vapply(tb, function(t) isTRUE(t$available), TRUE); s <- fc$summary
    who <- if (fc$rows == "interv") "interviewers" else "teams"
    na_list <- if (any(!av)) tags$details(style = "margin-top:10px", tags$summary(sprintf("%d table%s not produced (questions not in the data)", sum(!av), if (sum(!av) == 1) "" else "s")),
      tags$ul(class = "small-note", lapply(tb[!av], function(t) tags$li(tags$b(t$title), ": ", t$why))))
    if (is.null(s) || !nrow(s)) return(tagList(tags$p("No table with a target could be computed."), na_list))
    ids <- unique(s$id); rk <- unique(s[, .(row, rkey)]); rk <- rk[order(row != "Total", rkey)]
    m <- s[, .(nm = any(not_met)), by = .(id, row)]
    cnt <- s[row != "Total", .(n = sum(not_met)), by = .(row)]
    tot_bad <- s[row == "Total" & not_met, unique(id)]
    worst <- utils::head(cnt[order(-n)][n > 0], 3)
    open_js <- function(id) sprintf("Shiny.setInputValue('fct_open', '%s', {priority: 'event'})", id)
    hdr <- tags$tr(tags$th(""), lapply(ids, function(i) tags$th(class = "t", title = tb[[i]]$title, onclick = open_js(i), i)), tags$th("Not met"))
    body <- lapply(seq_len(nrow(rk)), function(j) { r <- rk$row[j]
      cells <- lapply(ids, function(i) { v <- m[id == i & row == r]$nm
        cl <- if (!length(v)) "na" else if (isTRUE(v)) "bad" else "ok"
        tags$td(class = paste("c", cl), title = sprintf("%s - %s: %s", r, tb[[i]]$title, c(na = "no data", bad = "target not met", ok = "target met")[[cl]]), onclick = open_js(i),
                if (cl == "bad") HTML("&#10007;") else "") })
      nn <- sum(m[row == r]$nm)
      tags$tr(class = if (r == "Total") "tot" else NULL, tags$td(class = "r", r), cells, tags$td(class = "n", nn)) })
    tagList(
      div(class = "fct-kpi",
          div(tags$b(sprintf("%d / %d", sum(av), length(av))), "tables produced"),
          div(class = if (length(tot_bad)) "r" else NULL, tags$b(length(tot_bad)), "tables with the target not met for the whole survey"),
          div(class = if (sum(cnt$n > 0)) "r" else NULL, tags$b(sprintf("%d / %d", sum(cnt$n > 0), nrow(cnt))), sprintf("%s with a target not met", who)),
          if (nrow(worst)) div(tags$b(style = "font-size:1em", paste(sprintf("%s (%d)", worst$row, worst$n), collapse = ", ")), sprintf("%s with the most targets not met", who))),
      card(card_header(sprintf("Targets by table and %s", if (fc$rows == "interv") "interviewer" else "team"),
                       tags$span(class = "small-note", style = "float:right", "Red = target not met, green = met, grey = no data. Click a table to open it.")),
           div(style = "overflow-x:auto", tags$table(class = "fct-grid", tags$thead(hdr), tags$tbody(body))),
           div(class = "small-note", style = "margin-top:6px", sprintf("Survey: %s. %s", fc$survey_label,
               if (!is.null(fc$filters$from)) sprintf("Interviews from %s to %s.", format(as.Date(fc$filters$from), "%d %b %Y"), format(as.Date(fc$filters$to %||% Sys.Date()), "%d %b %Y")) else "Whole fieldwork to date."))),
      na_list)
  })

  # one table: figures, targets not met in red, graphs
  fct_cur <- reactive({ fc <- fct_res(); req(fc, input$fct_pick); fc$tables[[input$fct_pick]] })
  fct_header <- function(cl, first) {
    g <- cl$group
    if (!any(nzchar(g))) return(tags$table(class = "display", tags$thead(tags$tr(tags$th(first), lapply(cl$label, tags$th)))))
    r <- rle(g); row1 <- list(tags$th(rowspan = 2, first)); i <- 1
    for (k in seq_along(r$values)) { n <- r$lengths[k]
      if (nzchar(r$values[k])) row1[[length(row1) + 1]] <- tags$th(colspan = n, class = "dt-center", style = "background:#DCEFF8", r$values[k])
      else for (j in i:(i + n - 1)) row1[[length(row1) + 1]] <- tags$th(rowspan = 2, cl$label[j])
      i <- i + n }
    tags$table(class = "display", tags$thead(tags$tr(row1), tags$tr(lapply(cl$label[nzchar(g)], tags$th))))
  }
  output$fct_tab_ui <- renderUI({
    if (is.null(fct_data())) return(tags$p(class = "small-note", style = "margin-top:20px", "Read the data first (button on the left)."))
    t <- fct_cur(); req(t); fc <- fct_res()
    by <- if (fc$rows == "interv") "interviewer" else "interviewer team"
    head <- div(class = "bf-card", tags$h4(style = "margin:0", t$title), div(class = "small-note", sprintf("%s, according to %s, %s.", t$subtitle, by, fc$survey_label)),
                if (isTRUE(t$available)) div(style = "margin-top:8px", actionButton("fct_ai", tagList(icon("wand-magic-sparkles"), "Read with AI"), class = "btn-sm btn-outline-primary")))
    if (!isTRUE(t$available)) return(tagList(head, div(class = "alert alert-secondary", icon("circle-info"), sprintf(" Not produced: it %s. The topic may not be in this survey's questionnaires.", t$why))))
    nr <- nrow(t$data)
    pl <- if (nr > 80) list(div(class = "small-note", "Graphs are drawn for up to 80 rows: choose teams on the left, or one row per team."))
          else lapply(seq_along(t$plots), function(i) { k <- t$plots[[i]]$kind; h <- if (k == "profile") 300 else max(260, 19 * nr + 130)
            card(renderPlot(ipm_fct_plot(t, i), height = h)) })
    tagList(head, uiOutput("fct_ai_out"),
            card(DT::DTOutput("fct_dt", height = "auto", fill = FALSE), if (!is.null(t$note)) div(class = "small-note", style = "margin-top:6px", icon("bullseye"), " ", t$note)),
            if (length(pl) == 2) layout_columns(col_widths = c(6, 6), pl[[1]], pl[[2]]) else pl)
  })
  output$fct_dt <- DT::renderDT({
    t <- fct_cur(); req(isTRUE(t$available)); x <- .int$fct_display(t); d <- as.data.frame(x$data, check.names = FALSE)
    d[[1]] <- htmltools::htmlEscape(d[[1]])
    for (j in seq_len(ncol(x$bad))) { b <- x$bad[, j]; if (any(b)) d[[j + 1]][b] <- sprintf("<span class='fct-bad'>%s</span>", d[[j + 1]][b]) }
    DT::datatable(d, container = fct_header(x$cols, if (fct_res()$rows == "interv") "Interviewer" else "Team"), rownames = FALSE, escape = FALSE, fillContainer = FALSE,
                  class = "compact stripe cell-border",
                  options = list(dom = if (nrow(d) > 40) "tip" else "t", pageLength = 40, scrollX = TRUE, ordering = FALSE,
                                 columnDefs = list(list(className = "dt-right", targets = seq_len(ncol(d) - 1))),
                                 rowCallback = DT::JS("function(row, data) { if (data[0] === 'Total') $(row).addClass('fct-total'); }")))
  })
  observeEvent(input$fct_ai, {
    t <- fct_cur(); req(isTRUE(t$available)); x <- .int$fct_display(t); d <- as.data.frame(x$data, check.names = FALSE)
    names(d) <- c("Row", trimws(paste(ifelse(nzchar(x$cols$group), paste0(x$cols$group, ":"), ""), x$cols$label)))
    nb <- apply(x$bad, 1, function(z) paste(names(d)[-1][z], collapse = "; ")); d$`Target not met in` <- nb
    ai_read(paste0("fct:", t$id), t$title, list(table = d, note = t$note %||% ""),
            "This is a MICS Field Check Table, produced during fieldwork to check data quality by team. Say which teams are far from the others or miss the target, what it may mean (e.g. members or births left out, ages moved out of the eligible range, documents not asked to be seen), and what supervisors should check or retrain.")
  })
  output$fct_ai_out <- renderUI({ t <- fct_cur(); req(t); read_box(paste0("fct:", t$id)) })

  # performance overview: first and second half of each team's interviews
  output$fct_ov <- renderUI({
    if (is.null(fct_data())) return(tags$p(class = "small-note", style = "margin-top:20px", "Read the data first (button on the left)."))
    fc <- fct_res(); req(fc); o <- fc$overview
    if (is.null(o)) return(tags$p("No indicator could be computed."))
    ind <- unique(o$indicator); nr <- length(unique(o$row))
    tagList(div(class = "small-note", style = "margin-bottom:8px", "As in the MICS7 Performance overview: each team's interviews to date are split in two halves by date (not by days of fieldwork), to see whether the team improves or gets worse. Age ratios close to 1 are expected."),
            div(class = "tp-head", selectInput("fct_ov_ind", NULL, ind, selected = if (isTRUE(isolate(input$fct_ov_ind) %in% ind)) isolate(input$fct_ov_ind) else ind[1], width = "420px")),
            card(if (nr > 80) div(class = "small-note", "Graph drawn for up to 80 rows.") else plotOutput("fct_ov_plot", height = max(260, 19 * nr + 130))),
            card(DT::DTOutput("fct_ov_dt", height = "auto", fill = FALSE)))
  })
  output$fct_ov_plot <- renderPlot({ fc <- fct_res(); req(fc, input$fct_ov_ind); .int$fct_ov_plot(fc, input$fct_ov_ind) })
  output$fct_ov_dt <- DT::renderDT({
    fc <- fct_res(); req(fc$overview); o <- fc$overview
    w <- data.table::dcast(o, row + rkey ~ indicator, value.var = c("first", "second")); w <- w[order(row != "Total", rkey)]
    ind <- unique(o$indicator); d <- data.frame(row = w$row, check.names = FALSE)
    for (i in ind) for (h in c("first", "second")) d[[paste(h, i)]] <- round(w[[paste0(h, "_", i)]], 2)
    cl <- data.table(name = names(d)[-1], label = rep(c("1st half", "2nd half"), length(ind)), group = rep(ind, each = 2))
    DT::datatable(d, container = fct_header(cl, if (fc$rows == "interv") "Interviewer" else "Team"), rownames = FALSE, class = "compact stripe cell-border", fillContainer = FALSE,
                  options = list(dom = if (nrow(d) > 40) "tip" else "t", pageLength = 40, scrollX = TRUE, ordering = FALSE,
                                 rowCallback = DT::JS("function(row, data) { if (data[0] === 'Total') $(row).addClass('fct-total'); }")))
  })

  # parameters (targets)
  output$fct_par_ui <- renderUI({
    p <- fct_par(); f <- fct_data(); cfg <- isolate(ex_cfg())
    det <- if (!is.null(f$fd$meta$hh$vallab$HH6)) { vl <- f$fd$meta$hh$vallab$HH6; paste(sprintf("%s = %s", unname(vl), names(vl)), collapse = ", ") } else NULL
    num <- function(n, lab = FSPEC[[n]]$label) { s <- FSPEC[[n]]; v <- p[[n]]
      numericInput(paste0("fp_", n), lab, value = if (is.null(v) || all(is.na(v))) NA else v, min = s$min, max = s$max, step = s$step, width = "100%") }
    pair <- function(a, lab) tagList(tags$b(style = "font-size:0.9em", lab), layout_columns(col_widths = c(6, 6), num(paste0(a, "_u"), "Urban"), num(paste0(a, "_r"), "Rural")))
    tagList(
      div(class = "small-note", style = "margin-bottom:10px", "The targets of 'targets.sps' and of the MICS7 table syntaxes. Set the expected numbers of eligible members from the previous survey or census. 'Apply' updates the tables; 'Apply and remember' also keeps them with the settings of this survey for the next data export."),
      layout_columns(col_widths = c(4, 4, 4),
        card(card_header("Survey and area"),
             textInput("fp_survey_label", "Survey name and year in the titles", p$survey_label, placeholder = cfg$survey_name %||% "e.g. Madagascar MICS7, 2025", width = "100%"),
             textInput("fp_urban", "Codes of HH6 (area) for urban", p$urban, placeholder = "from the value labels", width = "100%"),
             textInput("fp_rural", "Codes of HH6 for rural", p$rural, placeholder = "from the value labels", width = "100%"),
             div(class = "small-note", "Empty: read from the value labels of HH6", if (!is.null(det)) sprintf(" (in this data: %s)", det), ". Several codes with spaces, e.g. '1 2' for urban and '3 4' for rural when HH6 has 4 categories (as in targets.sps).")),
        card(card_header("Eligible members per household (F2)"),
             pair("t2w", "Women 15-49"), pair("t2m", "Men 15-49 (selected households)"), pair("t2c", "Children under 5"), pair("t2a", "Children 5-17"), num("elig_share")),
        card(card_header("Ages and dates of birth (F4, F5, F6, F11)"), num("t5w"), num("t5m"), num("t5u"), num("t5c"), num("age_int"), num("dob"))),
      card(card_header("Other targets"), layout_columns(col_widths = c(4, 4, 4), tagList(num("ceb_prev"), num("ceb_share")), tagList(num("seen"), num("heap")), tagList(num("death12"), num("mm_missing"), num("bh_first_year")))),
      div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:6px",
          actionButton("fct_apply", tagList(icon("check"), " Apply"), class = "btn-primary"),
          actionButton("fct_save", tagList(icon("floppy-disk"), " Apply and remember with the survey settings"), class = "btn-outline-primary"),
          actionButton("fct_reset", "Back to the MICS7 defaults", class = "btn-outline-secondary")))
  })
  fct_read_par <- function() {
    p <- fct_par()
    for (n in names(FSPEC)) { v <- input[[paste0("fp_", n)]]; if (!is.null(v)) p[[n]] <- if (is.na(v)) (if (n == "bh_first_year") NA else p[[n]]) else v }
    for (n in c("survey_label", "urban", "rural")) { v <- input[[paste0("fp_", n)]]; if (!is.null(v)) p[[n]] <- trimws(v) }
    p
  }
  observeEvent(input$fct_apply, { fct_par(fct_read_par()); showNotification("Parameters applied to the tables.", type = "message", duration = 3) })
  observeEvent(input$fct_save, {
    p <- fct_read_par(); fct_par(p); fct_keep(p)
    f <- if (!is.null(rv$cfg)) tryCatch(ipm_settings_save(collect_cfg()), error = function(e) NULL)
    showNotification(if (is.null(f)) "Parameters applied. They are remembered with the survey settings once the survey has a name (Setup) - and with the next run."
                     else "Parameters applied and remembered with the settings of this survey.", type = "message", duration = 6)
  })
  observeEvent(input$fct_reset, { fct_par(ipm_fct_defaults()); showNotification("MICS7 default targets applied (not remembered until you click 'Apply and remember').", type = "message", duration = 5) })
  output$fct_dl <- downloadHandler(filename = function() sprintf("MICS7_field_check_tables_%s.xlsx", format(Sys.Date(), "%Y%m%d")),
    content = function(f) { fc <- fct_res(); req(fc); ipm_fct_xlsx(fc, f) })

  # ---------- downloads ----------
  # the parts of the report to include (all ticked by default)
  output$rep_parts <- renderUI({
    res <- rv$res
    if (is.null(res)) return(tags$p(class = "small-note", "Run the analysis first: the parts of the report are listed here once the results are ready."))
    pt <- ipm_report_parts(res)
    layout_columns(col_widths = c(7, 5),
      div(checkboxGroupInput("rep_sections", tags$b("Sections of the Word report"), choiceNames = unname(pt$sections), choiceValues = names(pt$sections), selected = names(pt$sections)),
          div(style = "margin-top:-8px; margin-bottom:10px", actionLink("rep_all", "All"), " | ", actionLink("rep_none", "None"))),
      checkboxGroupInput("rep_files", tags$b("Companion files"), choiceNames = unname(pt$files), choiceValues = names(pt$files), selected = names(pt$files)))
  })
  observeEvent(input$rep_all, { req(rv$res); updateCheckboxGroupInput(session, "rep_sections", selected = names(ipm_report_parts(rv$res)$sections)) })
  observeEvent(input$rep_none, updateCheckboxGroupInput(session, "rep_sections", selected = character()))
  rep_built <- reactiveVal(NULL)                                  # what the report on the server contains, and when it was built
  rep_building <- reactiveVal(FALSE)
  observeEvent(input$build, {
    res <- rv$res; req(res)
    secs <- input$rep_sections %||% character(); fils <- input$rep_files %||% character()
    if (!length(secs) && !length(fils)) { showNotification("Tick at least one section or file.", type = "warning"); return() }
    if (job_running("report")) { showNotification("The report is already being built: wait for it to finish.", type = "warning"); return() }
    before <-if (!BG_OK) list.files(tempdir(), full.names = TRUE)
    rv$report <- NULL; rv$zip <- NULL; rep_built(NULL); rep_building(TRUE)
    started <- Sys.time()
    ok <- start_job("report", "report", list(res = res, dir = res$cfg$results_dir, zip = file.path(sdir, "IPM_results.zip"), sections = secs, files = fils), "Building the report",
      if (BG_OK) "you can keep using the app while it is built" else NULL, step = 1 / 16, hold = "^Writing (flag tables|report: flag graphs)",
      done = function(r) {
        rep_building(FALSE)
        if (!is.null(before)) { new <- setdiff(list.files(tempdir(), full.names = TRUE), before); wipe(new[!grepl("^ipm_", basename(new))]) }
        if (inherits(r, "error")) { showNotification(job_error(r), type = "error", duration = NULL); return() }
        rv$report <- r$report; rv$zip <- r$zip
        rep_built(list(sections = secs, files = fils, at = Sys.time(), mins = as.numeric(difftime(Sys.time(), started, units = "mins"))))
        showModal(modalDialog(title = tagList(icon("circle-check", style = "color:#198754"), " Report complete"), easyClose = TRUE,
          tags$p(sprintf("The IPM report was built in %s, with %d section%s and %d companion file%s.", fmt_mins(rep_built()$mins),
                         length(secs), if (length(secs) == 1) "" else "s", length(fils), if (length(fils) == 1) "" else "s")),
          footer = tagList(downloadButton("dl_report_m", "Download the report (Word)", class = "btn-primary"),
                           downloadButton("dl_zip_m", "Download all results (.zip)"), modalButton("Close"))))
      })
    if (!isTRUE(ok)) rep_building(FALSE)
  })
  fmt_mins <- function(m) if (m < 1) sprintf("%d seconds", max(1L, round(m * 60))) else sprintf("%.1f minutes", m)
  output$dl_status <- renderUI({
    b <- rep_built()
    if (isTRUE(rep_building())) return(div(class = "alert alert-info", style = "padding:8px 12px", icon("hourglass-half"), " Building the report... a 'Report complete' message appears here when it is ready to download."))
    if (is.null(b) || is.null(rv$report)) return(tags$p(class = "small-note", "No report built yet in this session."))
    same <- setequal(b$sections, input$rep_sections %||% character()) && setequal(b$files, input$rep_files %||% character())
    div(class = "alert alert-success", style = "padding:10px 14px",
      tags$b(icon("circle-check"), sprintf(" Report complete (built at %s, in %s).", format(b$at, "%H:%M"), fmt_mins(b$mins))),
      if (!same) tags$div(class = "small-note", style = "margin-top:4px", "The ticked parts have changed since: click 'Build report' again to include them."),
      div(style = "margin-top:8px", downloadButton("dl_report", "Download IPM_report.docx", class = "btn-primary"), downloadButton("dl_zip", "Download all results (.zip)")))
  })
  dl_rep <- downloadHandler(filename = function() sprintf("IPM_report_%s.docx", format(Sys.Date(), "%Y%m%d")),
                            content = function(f) { req(rv$report); file.copy(rv$report, f) })
  dl_zip <- downloadHandler(filename = function() sprintf("IPM_results_%s.zip", format(Sys.Date(), "%Y%m%d")),
                            content = function(f) { req(rv$zip); file.copy(rv$zip, f) })
  output$dl_report <- dl_rep; output$dl_report_m <- dl_rep
  output$dl_zip <- dl_zip; output$dl_zip_m <- dl_zip
}
