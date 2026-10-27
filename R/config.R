# Configuration: replaces the Stata dialog (ipm_settings.dlg) and default_settings.do

#' Default parameters (port of default_settings.do plus R-port options)
#' @export
ipm_defaults <- function() {
  list(
    # --- default_settings.do ---
    scatmaxrow   = 7,      # max rows of interviewer panels per page
    scatmaxcol   = 12,     # max columns of interviewer panels per page
    maxquestime  = 60,     # cap (minutes) for questionnaire duration
    hhunit       = c("HH1", "HH2"),
    momunit      = c("HH1", "HH2", "LN"),
    antunit      = c("HH1", "HH2", "LN"),
    max_uniq     = 34,     # > max_uniq unique values -> continuous (t-test)
    test_min     = 5,      # min obs per interviewer (team needs 2x)
    levelsig     = 0.001,  # flag threshold
    max_flag_var = 50,     # max variables per flag scatter page
    highly_flag  = 10,     # % flags for "highly flagged" list
    interco      = 1,      # result code: completed
    hhmsexm      = 1,
    hhmsexf      = 2,
    bhdeath      = 2,
    outrange     = 1,
    woi_min_daily = 20,    # first day with >= this many records defines week 1
    # --- R-port options (defaults reproduce Stata unless a clear bug) ---
    woi_anchor           = "dataset", # "dataset" (Stata) or "survey" (one week-1 for all datasets)
    test_min_nonmissing  = FALSE,     # TRUE: min-obs checks use non-missing obs on the tested variable
    exclude_height_codes = TRUE,      # drop heights >= 900 (e.g. 999.6 not-measured codes) from heaping
    filter_parent_result = TRUE,      # linked datasets keep only records whose parent interview is completed
    drop_long_strings    = FALSE,     # TRUE: drop text variables longer than 1 character on load (names; saves memory, protects privacy)
    fig_dpi              = 150
  )
}

ipm_output_names <- c(
  quesperday = "Total number of questionnaires per team and per day of fieldwork",
  quescrates = "Completion rates of each questionnaire per interviewer and per week of fieldwork",
  quesmtimes = "Median time to complete each questionnaire per interviewer and per week of fieldwork",
  meannohhm  = "Mean number of household members per interviewer and per week of fieldwork",
  everborn   = "Mean number of children ever born in birth histories per interviewer and per week of fieldwork",
  u5deaths   = "Percentage of under-5 deaths in birth histories per interviewer and per week of fieldwork",
  ant_heap   = "Anthropometry - heaping on height per team and per week of fieldwork",
  ant_oor    = "Anthropometry - out-of-range/flagged results per team and per week of fieldwork",
  flaganal   = "Flag analysis of interviewer bias in specified modules and datasets over specified period"
)

hhm_group_defs <- list(
  total   = list(label = "total",          lo = NA, hi = NA, sex = NA),
  a00_01  = list(label = "age 00-01y",     lo = 0,  hi = 1,  sex = NA),
  a00_04  = list(label = "age 00-04y",     lo = 0,  hi = 4,  sex = NA),
  a05_17  = list(label = "age 05-17y",     lo = 5,  hi = 17, sex = NA),
  f15_17  = list(label = "females 15-17y", lo = 15, hi = 17, sex = "f"),
  f46_49  = list(label = "females 46-49y", lo = 46, hi = 49, sex = "f"),
  f15_49  = list(label = "females 15-49y", lo = 15, hi = 49, sex = "f"),
  m15_49  = list(label = "males 15-49y",   lo = 15, hi = 49, sex = "m")
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

normalise_config <- function(cfg) {
  cfg$options <- utils::modifyList(ipm_defaults(), cfg$options %||% list())
  outs <- stats::setNames(as.list(rep(FALSE, length(ipm_output_names))), names(ipm_output_names))
  cfg$outputs <- utils::modifyList(outs, cfg$outputs %||% list())
  cfg$hhm_groups <- unlist(cfg$hhm_groups %||% names(hhm_group_defs))
  cfg$survey_name <- cfg$survey_name %||% "MICS survey"
  cfg$results_dir <- cfg$results_dir %||% file.path(cfg$data_dir %||% ".", "ipm_results")
  for (nm in names(cfg$datasets)) {
    d <- cfg$datasets[[nm]]
    d$file <- d$file %||% paste0(nm, ".sav")
    for (k in c("flag_modules", "flag_ignore", "flag_units", "keys"))
      d[[k]] <- if (is.null(d[[k]])) character() else unlist(d[[k]])
    cfg$datasets[[nm]] <- d
  }
  if (!is.null(cfg$teams$ranges)) cfg$teams$ranges <- unlist(cfg$teams$ranges)
  cfg
}

#' Read an IPM configuration (YAML)
#' @param path YAML file
#' @export
read_config <- function(path) {
  cfg <- yaml::read_yaml(path)
  if (!is.null(cfg$data_dir) && !dir.exists(cfg$data_dir)) {
    alt <- file.path(dirname(path), cfg$data_dir)
    if (dir.exists(alt)) cfg$data_dir <- alt
  }
  normalise_config(cfg)
}

#' Write an IPM configuration (YAML)
#' @param cfg configuration list
#' @param path YAML file
#' @export
write_config <- function(cfg, path) {
  cfg$options <- cfg$options[!vapply(cfg$options, is.null, logical(1))]
  yaml::write_yaml(cfg, path)
  invisible(path)
}

# ---- Varlist handling (Stata-like: wildcards, A-B ranges by dataset order) ----

#' Expand a Stata-style varlist against dataset variable names
#' @param spec Stata-style varlist (wildcards *, ?, ranges A-B)
#' @param vars variable names in dataset order
#' @export
expand_varlist <- function(spec, vars) {
  spec <- unlist(strsplit(paste(spec, collapse = " "), "[[:space:]]+"))
  spec <- spec[nzchar(spec)]
  out <- character()
  for (tok in spec) {
    if (grepl("^[^*?-]+-[^*?-]+$", tok)) {
      ab <- strsplit(tok, "-", fixed = TRUE)[[1]]
      i <- match(ab[1], vars); j <- match(ab[2], vars)
      if (!is.na(i) && !is.na(j)) out <- c(out, vars[i:j])
    } else if (grepl("[*?]", tok)) {
      out <- c(out, vars[grepl(utils::glob2rx(tok), vars)])
    } else if (tok %in% vars) out <- c(out, tok)
  }
  unique(out)
}

#' List modules by MICS variable naming convention (port of "As MICS" button)
#'
#' Variables with a digit in position 3 give a 2-letter module, digit in
#' position 4 a 3-letter module. Returns patterns such as "HC*".
#' @param vars variable names
#' @param exclude module prefixes to leave out
#' @export
suggest_mics_modules <- function(vars, exclude = character()) {
  m <- ifelse(grepl("^..[0-9]", vars), substr(vars, 1, 2),
         ifelse(grepl("^...[0-9]", vars), substr(vars, 1, 3), NA))
  m <- unique(m[!is.na(m)])
  m <- setdiff(m, exclude)
  if (length(m)) paste0(m, "*") else character()
}

# ---- Validation (replaces the dialog "Check" buttons) ----

#' Validate a configuration against the data; returns a data.frame of issues
#' @param cfg configuration list
#' @param data optional object from ipm_load()
#' @export
validate_config <- function(cfg, data = NULL) {
  cfg <- normalise_config(cfg)
  iss <- list()
  add <- function(level, where, msg) iss[[length(iss) + 1]] <<- data.frame(level = level, where = where, message = msg)
  if (is.null(cfg$data_dir) || !dir.exists(cfg$data_dir)) add("error", "main", "data_dir does not exist")
  if (!length(cfg$datasets)) add("error", "datasets", "no datasets configured")
  if (is.null(cfg$teams$ranges) && is.null(cfg$teams$variable)) add("error", "teams", "define teams: ranges or variable")
  if (!is.null(cfg$teams$ranges)) {
    bad <- !grepl("^\\s*[0-9]+\\s*-\\s*[0-9]+\\s*$", cfg$teams$ranges) & nzchar(trimws(cfg$teams$ranges))
    if (any(bad)) add("error", "teams", paste("invalid ranges:", paste(cfg$teams$ranges[bad], collapse = ", ")))
  }
  needs_period <- isTRUE(cfg$outputs$flaganal) || isTRUE(cfg$outputs$ant_oor)
  if (needs_period) {
    st <- tryCatch(as.Date(cfg$period$start), error = function(e) NA)
    if (is.null(cfg$period$start) || is.na(st)) add("error", "main", "period$start must be YYYY-MM-DD (needed for flag/anthro outputs)")
    else if (st > Sys.Date()) add("warning", "main", "period start is later than today")
  }
  if (is.null(data)) data <- tryCatch(ipm_load(cfg, quiet = TRUE), error = function(e) {add("error", "load", conditionMessage(e)); NULL})
  if (is.null(data)) return(do.call(rbind, iss))
  chk <- function(ds, v, what, numeric = TRUE) {
    if (is.null(v) || !nzchar(v)) return(invisible())
    if (!ds %in% names(data$raw)) return(invisible())
    x <- data$raw[[ds]]
    if (!v %in% names(x)) add("error", ds, sprintf("%s variable '%s' not found", what, v))
    else if (numeric && !is.numeric(x[[v]])) add("error", ds, sprintf("%s variable '%s' must be numeric", what, v))
    else if (all(is.na(x[[v]]))) add("error", ds, sprintf("%s variable '%s' is empty", what, v))
  }
  for (ds in names(cfg$datasets)) {
    d <- cfg$datasets[[ds]]
    if (!ds %in% names(data$raw)) { add("error", ds, paste("file not found:", d$file)); next }
    if (is.null(d$parent)) {
      if (is.null(d$interviewer)) add("warning", ds, "no interviewer variable: dataset excluded")
      if (is.null(d$day)) add("warning", ds, "no day-of-interview variable: dataset excluded")
    } else {
      if (!d$parent %in% names(cfg$datasets)) add("error", ds, paste("parent dataset not configured:", d$parent))
      miss <- setdiff(d$keys, names(data$raw[[ds]]))
      if (!length(d$keys)) add("error", ds, "linked dataset needs keys")
      if (length(miss)) add("error", ds, paste("keys not found:", paste(miss, collapse = ",")))
    }
    chk(ds, d$interviewer, "interviewer"); chk(ds, d$result, "result"); chk(ds, d$day, "day")
    chk(ds, d$start_hour, "start hour"); chk(ds, d$end_hour, "end hour")
    vars <- names(data$raw[[ds]])
    for (m in d$flag_modules) if (!length(expand_varlist(m, vars)) && m != "*") add("warning", ds, paste("module matches no variable:", m))
    for (u in d$flag_units) if (!u %in% vars) add("error", ds, paste("unit variable not found:", u))
  }
  ex <- cfg$extras %||% list()
  if (isTRUE(cfg$outputs$meannohhm)) { e <- ex$hhm; if (is.null(e$dataset)) add("error", "extras", "hhm dataset not set") else { chk(e$dataset, e$sex, "sex"); chk(e$dataset, e$age, "age") } }
  if (isTRUE(cfg$outputs$everborn) || isTRUE(cfg$outputs$u5deaths)) { e <- ex$bh; if (is.null(e$dataset)) add("error", "extras", "birth history dataset not set") else { chk(e$dataset, e$death, "death"); chk(e$dataset, e$age_death, "age at death"); if (!is.null(e$age_death_unit)) chk(e$dataset, e$age_death_unit, "age at death unit") } }
  if (isTRUE(cfg$outputs$ant_heap) || isTRUE(cfg$outputs$ant_oor)) { e <- ex$ant; if (is.null(e$dataset)) add("error", "extras", "anthropometry dataset not set") else { chk(e$dataset, e$height, "height"); for (f in unlist(e$flags)) chk(e$dataset, f, "anthro flag") }
    if (isTRUE(cfg$outputs$ant_oor) && !length(unlist(ex$ant$flags))) add("error", "extras", "no anthropometry flag variable set") }
  if (!length(iss)) return(data.frame(level = character(), where = character(), message = character()))
  do.call(rbind, iss)
}

# ---- MICS auto-configuration (language-agnostic) ----

# fold accents / curly quotes so label regexes are plain ASCII (platform independent)
ascii_fold <- function(x) {
  x <- as.character(x)
  cp <- c(0xe0:0xe5, 0xe7:0xef, 0xf1:0xf6, 0xf9:0xfd, 0xff, 0xc0:0xc5, 0xc7:0xcf, 0xd1:0xd6, 0xd9:0xdd, 0x2018, 0x2019, 0x02bc)
  to <- strsplit("aaaaaaceeeeiiiinooooouuuuyyAAAAAACEEEEIIIINOOOOOUUUUY'''", "")[[1]]
  from <- intToUtf8(cp, multiple = TRUE)
  for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE, useBytes = TRUE)
  x <- iconv(x, "UTF-8", "ASCII", sub = " ")
  x[is.na(x)] <- ""
  tolower(x)
}

# multilingual label patterns (EN / FR / ES / PT), matched on ascii_fold()ed labels
mics_lab <- list(
  interviewer = "interview(er|eur)|enquet(eur|rice|eu)|entrevistador|encuestador|inquiridor|recenseu|agent enqu",
  hh_interviewer = "household interviewer|enqueteu?r?(rice)? menage|entrevistador del hogar|hint",
  result = "^(result|resultat|resultado)|result of|resultat de|resultado d",
  start = "(start|debut|inicio|comienzo|commencement).*(hour|heure|hora)|(hour|heure|hora).*(start|debut|inicio)",
  end = "(end|fin|final|termino).*(hour|heure|hora)|(hour|heure|hora).*(end|fin|termino)",
  supervisor = "supervis|chef d.?equipe|chef d.?quipe|team leader|jefe|chefe|controleu",
  birth = "birth|naissance|nacimiento|nascimento",
  completed = "^complet|rempli|achev|conclu|termin",
  line = "line number|^line|numero de (la )?ligne|numero de linea|numero da linha|^net number|^numero du moustiquaire|^name$|'s name|^nom$|^nombre$"
)

# standard MICS questionnaire panels and their usual variable names (MICS5-7)
mics_panels <- list(
  HH  = list(interviewer = "HH3",  result = "HH46",  supervisor = "HH4"),
  WM  = list(interviewer = "WM5",  result = "WM17",  supervisor = "WM4"),
  MWM = list(interviewer = "MWM5", result = "MWM17", supervisor = "MWM4"),
  UF  = list(interviewer = "UF5",  result = "UF17",  supervisor = "UF6"),
  FS  = list(interviewer = "FS5",  result = "FS17",  supervisor = "FS6")
)

find_by_label <- function(lab, pattern, vars = names(lab), not = NULL) {
  fl <- ascii_fold(lab[vars])
  hit <- vars[grepl(pattern, fl)]
  if (!is.null(not)) hit <- hit[!grepl(not, ascii_fold(lab[hit]))]
  if (length(hit)) hit[1] else NULL
}

# day/month/year triples (XnD, XnM, XnY consecutive) -> list of D variables
dmy_triples <- function(v) {
  d <- v[grepl("^[A-Z]+[0-9]+[A-Z]?D$", v)]
  d[vapply(d, function(x) { i <- match(x, v); i + 2 <= length(v) && v[i + 1] == sub("D$", "M", x) && v[i + 2] == sub("D$", "Y", x) }, logical(1))]
}
# hour/minute pairs (XnH, XnM consecutive)
hm_pairs <- function(v, prefix) {
  h <- v[grepl(paste0("^", prefix, "[0-9]+[A-Z]?H$"), v)]
  h[vapply(h, function(x) { i <- match(x, v); i + 1 <= length(v) && v[i + 1] == sub("H$", "M", x) }, logical(1))]
}
panel_of <- function(x) sub("^([A-Z]+?)[0-9].*$", "\\1", x)
in_panel <- function(v, P) v[grepl(paste0("^", P, "[0-9]"), v) | v == paste0(P, "INT")]

# completed-code value label test (for result variables without a usable label)
has_completed_code <- function(vl) {
  !is.null(vl) && length(vl) && any(unname(vl) == 1 & grepl(mics_lab$completed, ascii_fold(names(vl))))
}

detect_questionnaire <- function(m) {
  v <- m$vars; lab <- m$lab
  for (dd in dmy_triples(v)) {
    P <- panel_of(dd)
    if (grepl(mics_lab$birth, ascii_fold(lab[dd]))) next
    pv <- in_panel(v, P)
    std <- mics_panels[[P]]
    res <- if (!is.null(std) && std$result %in% v) std$result else find_by_label(lab, mics_lab$result, pv)
    if (is.null(res)) { cand <- pv[vapply(pv, function(x) has_completed_code(m$vallab[[x]]), logical(1))]; if (length(cand)) res <- cand[1] }
    if (is.null(res)) next
    int <- if (!is.null(std) && std$interviewer %in% v) std$interviewer else {
      cand <- pv[grepl(mics_lab$interviewer, ascii_fold(lab[pv])) & !grepl(mics_lab$hh_interviewer, ascii_fold(lab[pv])) & !grepl("HINT$", pv)]
      cand <- c(cand[!grepl("INT$", cand)], cand)
      if (length(cand)) cand[1] else if (paste0(P, "INT") %in% v) paste0(P, "INT") else NULL
    }
    if (is.null(int)) next
    hp <- hm_pairs(v, P)
    st <- find_by_label(lab, mics_lab$start, hp); en <- find_by_label(lab, mics_lab$end, hp)
    if (is.null(st) && length(hp) >= 2) st <- hp[1]
    if (is.null(en) && length(hp) >= 2) en <- hp[length(hp)]
    sup <- if (!is.null(std) && std$supervisor %in% v) std$supervisor else find_by_label(lab, mics_lab$supervisor, pv)
    return(list(panel = P, interviewer = int, result = res, day = dd, start_hour = st, end_hour = en, supervisor = sup))
  }
  NULL
}

# one string per variable, whatever the attribute holds (some exports store a label or format as a vector of 2+)
var_attr1 <- function(v, which = "label", alt = NULL) {
  a <- attr(v, which, exact = TRUE)
  if (is.null(a) && !is.null(alt)) a <- attr(v, alt, exact = TRUE)
  if (is.null(a) || !length(a)) return("")
  a <- as.character(a); a <- a[!is.na(a) & nzchar(a)]
  if (!length(a)) "" else if (which == "label") paste(unique(a), collapse = " ") else a[1]
}


#' Build a configuration automatically from a folder of MICS datasets
#'
#' Language-agnostic: detection relies first on MICS variable-name structure
#' (questionnaire panels HH/WM/MWM/UF/FS, day-month-year and hour-minute
#' variable triplets, *INT variables), then on variable labels in English,
#' French, Spanish or Portuguese, then on value labels (code 1 = completed).
#' Datasets without their own result variable (hl, bh, mm, fg, tn ...) are
#' linked to their parent questionnaire. Dataset roles (household members,
#' birth history, anthropometry) are found from content, not file names.
#' @param data_dir folder with .sav/.dta datasets
#' @param survey_name name shown in the report
#' @param files optional subset of file names
#' @export
mics_autoconfig <- function(data_dir, survey_name = basename(normalizePath(data_dir)), files = NULL) {
  files <- files %||% list.files(data_dir, pattern = "\\.(sav|dta)$", ignore.case = TRUE)
  if (!length(files)) stop("No .sav or .dta files found in ", data_dir)
  meta <- list()
  for (f in files) {
    nm <- make.names(tolower(tools::file_path_sans_ext(f)))
    x <- tryCatch(read_any(file.path(data_dir, f), n_max = 50), error = function(e) stop(f, ": ", conditionMessage(e), call. = FALSE))
    names(x) <- toupper(names(x))
    lab <- vapply(x, var_attr1, character(1), which = "label")
    meta[[nm]] <- list(file = f, vars = names(x), lab = lab, vallab = lapply(x, function(v) attr(v, "labels")))
  }
  ds <- list(); qinfo <- list()
  for (nm in names(meta)) {
    q <- detect_questionnaire(meta[[nm]])
    d <- list(file = meta[[nm]]$file)
    if (!is.null(q)) { qinfo[[nm]] <- q; d[c("interviewer", "result", "day", "start_hour", "end_hour")] <- q[c("interviewer", "result", "day", "start_hour", "end_hour")] }
    ds[[nm]] <- d
  }
  if (!length(qinfo)) stop("No MICS questionnaire detected (need interviewer, result and day-month-year variables). Configure datasets manually.")
  # most specific panels first, household last
  qn <- names(qinfo)[order(vapply(qinfo, function(q) q$panel == "HH", logical(1)))]
  hhq <- names(qinfo)[vapply(qinfo, function(q) q$panel == "HH", logical(1))][1]
  for (nm in setdiff(names(ds), names(qinfo))) {
    v <- meta[[nm]]$vars
    parent <- NULL
    for (p in qn) { P <- qinfo[[p]]$panel; if (P != "HH" && any(c(paste0(P, c("1", "INT")), qinfo[[p]]$interviewer) %in% v)) { parent <- p; break } }
    if (is.null(parent)) parent <- hhq
    if (is.null(parent)) next
    P <- qinfo[[parent]]$panel; pv <- meta[[parent]]$vars
    keys <- if (P == "HH") c("HH1", "HH2") else c("HH1", "HH2", "LN")
    if (!all(keys %in% v & keys %in% pv)) keys <- paste0(P, 1:(if (P == "HH") 2 else 3))
    keys <- keys[keys %in% v & keys %in% pv]
    ds[[nm]]$parent <- parent; ds[[nm]]$keys <- keys
    if (qinfo[[parent]]$day %in% v) ds[[nm]]$day <- qinfo[[parent]]$day
  }
  # flag modules: MICS naming; a module copied from another dataset is left to its home dataset
  panels <- c(names(mics_panels), unique(vapply(qinfo, `[[`, "", "panel")), "AN", "HAZ", "WAZ", "WHZ", "BMI", "ST")
  modvars <- lapply(meta, function(m) { mods <- suggest_mics_modules(m$vars, exclude = panels); stats::setNames(lapply(mods, function(md) expand_varlist(md, m$vars)), mods) })
  for (nm in names(ds)) {
    keep <- character()
    for (md in names(modvars[[nm]])) {
      a <- modvars[[nm]][[md]]; drop <- FALSE
      for (o in setdiff(names(meta), nm)) {
        b <- modvars[[o]][[md]]
        if (!is.null(b) && all(a %in% b) && (length(b) > length(a) || (length(b) == length(a) && length(meta[[o]]$vars) < length(meta[[nm]]$vars)))) { drop <- TRUE; break }
      }
      if (!drop) keep <- c(keep, md)
    }
    if (!length(keep)) next
    vars <- meta[[nm]]$vars; lab <- meta[[nm]]$lab
    ds[[nm]]$flag_modules <- keep
    inmod <- expand_varlist(keep, vars)
    ign <- inmod[grepl(mics_lab$line, ascii_fold(lab[inmod])) | grepl("LN$|_(FIRST|LAST)$", inmod)]
    if (length(ign)) ds[[nm]]$flag_ignore <- ign
    units <- inmod[grepl("U$", inmod)]
    units <- units[vapply(units, function(u) { i <- match(u, vars); i < length(vars) && vars[i + 1] == sub("U$", "N", u) }, logical(1))]
    if (length(units)) ds[[nm]]$flag_units <- units
  }
  # roles by content
  extras <- list()
  notq <- setdiff(names(meta), names(qinfo))
  pick <- function(cands) if (length(cands)) cands[order(!cands %in% notq)][1] else NULL
  hl <- pick(names(meta)[vapply(meta, function(m) all(c("HL1", "HL4", "HL6") %in% m$vars), logical(1))])
  if (!is.null(hl)) extras$hhm <- list(dataset = hl, sex = "HL4", age = "HL6")
  bh <- pick(names(meta)[vapply(meta, function(m) "BH5" %in% m$vars && any(c("BH9C", "BH9N") %in% m$vars), logical(1))])
  if (!is.null(bh)) extras$bh <- if ("BH9C" %in% meta[[bh]]$vars) list(dataset = bh, death = "BH5", age_death = "BH9C")
                                 else list(dataset = bh, death = "BH5", age_death = "BH9N", age_death_unit = "BH9U")
  an <- names(meta)[vapply(meta, function(m) "AN11" %in% m$vars, logical(1))]
  if (length(an)) {
    a <- an[1]; fl <- meta[[a]]$vars[grepl("^(HAZ|WAZ|WHZ)FLAG$", meta[[a]]$vars)]
    extras$ant <- list(dataset = a, height = "AN11", flags = fl)
  }
  teams <- derive_team_ranges(data_dir, ds, meta, qinfo)
  st <- NULL
  if (!is.null(hhq)) {
    d <- ds[[hhq]]; v <- meta[[hhq]]$vars; i <- match(d$day, v)
    hh <- read_any_cols(file.path(data_dir, meta[[hhq]]$file), v[i:(i + 2)])
    dd <- as.Date(sprintf("%04d-%02d-%02d", as.integer(hh[[3]]), as.integer(hh[[2]]), as.integer(hh[[1]])), optional = TRUE)
    if (any(!is.na(dd))) st <- format(max(min(dd, na.rm = TRUE), max(dd, na.rm = TRUE) - 20))
  }
  cfg <- list(
    survey_name = survey_name, data_dir = data_dir, results_dir = file.path(data_dir, "ipm_results"),
    period = list(start = st, days = 21),
    teams = teams,
    outputs = as.list(stats::setNames(rep(TRUE, length(ipm_output_names)), names(ipm_output_names))),
    hhm_groups = names(hhm_group_defs),
    extras = extras,
    datasets = ds
  )
  if (is.null(extras$hhm)) cfg$outputs$meannohhm <- FALSE
  if (is.null(extras$bh)) { cfg$outputs$everborn <- FALSE; cfg$outputs$u5deaths <- FALSE }
  if (is.null(extras$ant)) { cfg$outputs$ant_heap <- FALSE; cfg$outputs$ant_oor <- FALSE }
  else if (!length(extras$ant$flags)) cfg$outputs$ant_oor <- FALSE
  normalise_config(cfg)
}

#' @rdname mics_autoconfig
#' @export
mics7_autoconfig <- mics_autoconfig

read_any_cols <- function(path, cols) {
  ext <- tolower(tools::file_ext(path))
  x <- tryCatch(if (ext == "sav") haven::read_sav(path, col_select = tidyselect::any_of(c(cols, tolower(cols))))
                else haven::read_dta(path, col_select = tidyselect::any_of(c(cols, tolower(cols)))),
                error = function(e) read_any(path))
  names(x) <- toupper(names(x))
  x[, intersect(cols, names(x)), drop = FALSE]
}

# Derive interviewer ranges per team from the supervisor / team leader variable
derive_team_ranges <- function(data_dir, ds, meta, qinfo) {
  pairs <- list()
  for (nm in names(qinfo)) {
    sup <- qinfo[[nm]]$supervisor; int <- qinfo[[nm]]$interviewer
    if (is.null(sup)) next
    x <- read_any_cols(file.path(data_dir, meta[[nm]]$file), c(int, sup))
    if (ncol(x) < 2) next
    pairs[[nm]] <- data.table(interv = as.numeric(x[[int]]), sup = as.numeric(x[[sup]]))
  }
  if (!length(pairs)) return(list(ranges = character()))
  p <- rbindlist(pairs)[!is.na(interv) & !is.na(sup)]
  modal <- p[, .N, by = .(interv, sup)][order(-N)][, .SD[1], by = interv]
  r <- modal[, .(lo = min(interv), hi = max(interv)), by = sup][order(lo)]
  list(ranges = sprintf("%d-%d", as.integer(r$lo), as.integer(r$hi)), supervisor = as.character(r$sup))
}
