# Loading (port of the "REFORMAT DATASETS" action) and preparation (port of MASTER.do)

read_any <- function(path, n_max = Inf) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "sav") haven::read_sav(path, n_max = n_max, user_na = FALSE)
  else if (ext == "dta") haven::read_dta(path, n_max = n_max)
  else stop("Unsupported file format (use .sav or .dta): ", path)
}

utility_vars <- c("doi", "woi", "interv", "team", "result", "dummy", "presult")

# variables actually used by the analysis for one dataset (slim loading)
needed_vars <- function(cfg, nm, vars) {
  d <- cfg$datasets[[nm]]; o <- cfg$options
  if (any(trimws(d$flag_modules) == "*")) return(vars)
  nx <- function(v, k) { if (is.null(v) || !length(v)) return(NULL); i <- match(v[1], vars); if (is.na(i) || i + k > length(vars)) NULL else vars[i + k] }
  v <- c("HH1", "HH2", "LN", d$keys, d$interviewer, d$result, cfg$teams$variable,
         d$day, d$month %||% nx(d$day, 1), d$year %||% nx(d$day, 2),
         d$start_hour, d$start_min %||% nx(d$start_hour, 1), d$end_hour, d$end_min %||% nx(d$end_hour, 1))
  for (ch in names(cfg$datasets)) if (identical(cfg$datasets[[ch]]$parent, nm)) v <- c(v, cfg$datasets[[ch]]$keys)
  e <- cfg$extras
  if (identical(e$hhm$dataset, nm)) v <- c(v, e$hhm$sex, e$hhm$age, o$hhunit)
  if (identical(e$bh$dataset, nm)) v <- c(v, e$bh$death, e$bh$age_death, e$bh$age_death_unit, o$momunit)
  if (identical(e$ant$dataset, nm)) {
    h <- e$ant$height; mod <- if (!is.null(h) && grepl("^..[0-9]", h)) substr(h, 1, 2) else substr(h %||% "", 1, 3)
    v <- c(v, h, unlist(e$ant$flags), o$antunit, if (nzchar(mod)) expand_varlist(paste0(mod, "*"), vars))
  }
  fm <- expand_varlist(d$flag_modules, vars)
  v <- c(v, fm, unlist(lapply(d$flag_units, nx, 1)), d$flag_units)
  vars[vars %in% v]
}

integerize <- function(x) {
  for (v in names(x)) {
    z <- x[[v]]
    if (is.double(z)) {
      ok <- !is.na(z)
      if (all(z[ok] == trunc(z[ok])) && all(abs(z[ok]) < 2e9)) set(x, j = v, value = as.integer(z))
    }
  }
  x
}

#' Load all configured datasets
#'
#' Reads each .sav/.dta, upper-cases variable names, adds `dummy` = 1, and
#' separates metadata (variable/value labels, SPSS formats) from the data.
#' @param cfg configuration list
#' @param quiet suppress messages
#' @param slim keep only the variables used by the analysis (saves memory)
#' @param n_max maximum number of rows to read (for previews)
#' @export
ipm_load <- function(cfg, quiet = FALSE, slim = FALSE, n_max = Inf) {
  cfg <- normalise_config(cfg)
  raw <- list(); meta <- list()
  for (nm in names(cfg$datasets)) {
    f <- file.path(cfg$data_dir, cfg$datasets[[nm]]$file)
    if (!file.exists(f)) { if (!quiet) message("File not found, skipped: ", f); next }
    if (!quiet) message("Reading ", basename(f))
    if (slim) {
      hdr <- read_any(f, n_max = 0); orig <- names(hdr); up <- toupper(orig)
      keep <- orig[up %in% needed_vars(cfg, nm, up)]
      x <- if (tolower(tools::file_ext(f)) == "sav") haven::read_sav(f, col_select = tidyselect::all_of(keep), user_na = FALSE, n_max = n_max)
           else haven::read_dta(f, col_select = tidyselect::all_of(keep), n_max = n_max)
    } else x <- read_any(f, n_max = n_max)
    names(x) <- toupper(names(x))
    varlab <- vapply(x, var_attr1, character(1), which = "label")
    vallab <- lapply(x, function(v) attr(v, "labels"))
    fmt <- vapply(x, var_attr1, character(1), which = "format.spss", alt = "format.stata")
    if (isTRUE(cfg$options$drop_long_strings)) {
      w <- stats::setNames(suppressWarnings(as.integer(sub("^A", "", fmt))), names(fmt))
      chr <- names(x)[vapply(x, is.character, logical(1))]
      long <- chr[vapply(chr, function(v) if (!is.na(w[[v]])) w[[v]] > 1 else max(nchar(x[[v]]), 0, na.rm = TRUE) > 1, logical(1))]
      if (length(long)) x <- x[, setdiff(names(x), long)]
    }
    x <- haven::zap_formats(haven::zap_labels(haven::zap_missing(x)))
    x <- as.data.table(x)
    for (v in names(x)) if (is.character(x[[v]])) set(x, j = v, value = enc2utf8(x[[v]]))
    if (slim) x <- integerize(x)
    x[, dummy := 1L]
    raw[[nm]] <- x
    meta[[nm]] <- list(varlab = varlab, vallab = vallab, fmt = fmt)
    if (slim) invisible(gc(verbose = FALSE))
  }
  structure(list(raw = raw, meta = meta), class = "ipm_data")
}

# position-based companion variable (Stata convention: next variable(s) in file)
next_var <- function(vars, v, k = 1) {
  i <- match(v, vars)
  if (is.na(i)) stop("Variable ", v, " not found")
  if (i + k > length(vars)) stop("Variable ", v, " is last (or second last); cannot find companion variable")
  vars[i + k]
}

make_date <- function(x, d) {
  vars <- names(x)
  m <- d$month %||% next_var(vars, d$day, 1)
  y <- d$year %||% next_var(vars, d$day, 2)
  dd <- suppressWarnings(as.integer(x[[d$day]])); mm <- suppressWarnings(as.integer(x[[m]])); yy <- suppressWarnings(as.integer(x[[y]]))
  out <- as.IDate(rep(NA_integer_, nrow(x)))
  ok <- !is.na(dd) & !is.na(mm) & !is.na(yy)
  out[ok] <- as.IDate(sprintf("%04d-%02d-%02d", yy[ok], mm[ok], dd[ok]), optional = TRUE)
  out
}

week_start <- function(doi, min_daily = 20) {
  t <- data.table(doi = doi)[, .N, by = doi][order(doi)]
  t <- t[!is.na(doi)]
  if (!nrow(t)) return(as.IDate(NA))
  hit <- t[N >= min_daily]
  if (nrow(hit)) hit$doi[1] else t$doi[1]
}

assign_teams <- function(interv, sup, cfg, team_levels = NULL) {
  team <- rep(NA_integer_, length(interv))
  if (!is.null(cfg$teams$variable)) {
    if (is.null(sup)) return(team)
    team <- match(sup, team_levels)
  } else {
    rg <- cfg$teams$ranges
    for (j in seq_along(rg)) {
      if (!nzchar(trimws(rg[j]))) next
      ab <- as.numeric(strsplit(gsub("\\s", "", rg[j]), "-", fixed = TRUE)[[1]])
      team[!is.na(interv) & interv >= ab[1] & interv <= ab[2]] <- j
    }
  }
  team
}

#' Prepare analysis files (doi, woi, interv, team, result)
#'
#' Port of the work-file creation in MASTER.do. Datasets with a `parent` take
#' interviewer and date of interview from the parent questionnaire via `keys`.
#' @param data object from ipm_load()
#' @param cfg configuration list
#' @param quiet suppress messages
#' @param inplace modify the loaded data in place instead of copying (saves memory)
#' @export
ipm_prepare <- function(data, cfg, quiet = FALSE, inplace = FALSE) {
  cfg <- normalise_config(cfg); o <- cfg$options
  ds_cfg <- cfg$datasets[names(cfg$datasets) %in% names(data$raw)]
  # team levels for the "variable" method (global, sorted)
  team_levels <- NULL
  if (!is.null(cfg$teams$variable)) {
    tv <- cfg$teams$variable
    team_levels <- sort(unique(unlist(lapply(data$raw, function(x) if (tv %in% names(x)) as.numeric(x[[tv]]) else NULL))))
  }
  order_ds <- c(names(ds_cfg)[vapply(ds_cfg, function(d) is.null(d$parent), logical(1))],
                names(ds_cfg)[vapply(ds_cfg, function(d) !is.null(d$parent), logical(1))])
  out <- list(); stdates <- list()
  for (nm in order_ds) {
    d <- ds_cfg[[nm]]; x <- if (inplace) data$raw[[nm]] else copy(data$raw[[nm]])
    if (!is.null(d$parent)) {
      p <- out[[d$parent]]
      if (is.null(p)) { if (!quiet) message(nm, ": parent '", d$parent, "' unavailable - excluded"); next }
      pk <- unique(p[, c(d$keys, "interv", "doi", if ("result" %in% names(p)) "result"), with = FALSE], by = d$keys)
      setnames(pk, c("interv", "doi"), c(".p_interv", ".p_doi"))
      if ("result" %in% names(pk)) setnames(pk, "result", "presult")
      for (k in d$keys) if (!identical(class(x[[k]]), class(pk[[k]]))) { set(x, j = k, value = as.numeric(x[[k]])); set(pk, j = k, value = as.numeric(pk[[k]])) }
      orig_names <- copy(names(x))
      x[, .rowid := .I]
      x <- merge(x, pk, by = d$keys, all.x = TRUE, sort = FALSE)
      setorder(x, .rowid); x[, .rowid := NULL]
      setcolorder(x, c(orig_names, setdiff(names(x), orig_names)))
      if (o$filter_parent_result && "presult" %in% names(x)) x <- x[presult %in% o$interco]
    } else if (is.null(d$interviewer) || is.null(d$day)) {
      if (!quiet) message(nm, ": no interviewer or day variable - excluded from analysis"); next
    }
    # doi
    if (!is.null(d$day)) x[, doi := make_date(x, d)] else x[, doi := .p_doi]
    # interviewer
    if (!is.null(d$interviewer)) {
      if (!is.numeric(x[[d$interviewer]])) stop(nm, ": interviewer variable must be numeric")
      x[, interv := as.numeric(get(d$interviewer))]
    } else x[, interv := .p_interv]
    if (all(is.na(x$interv))) stop(nm, ": interviewer number is empty")
    dropc <- intersect(c(".p_doi", ".p_interv"), names(x)); if (length(dropc)) x[, (dropc) := NULL]
    # result
    if (!is.null(d$result)) {
      if (!is.numeric(x[[d$result]]) || all(is.na(x[[d$result]]))) stop(nm, ": result variable must be numeric and not empty")
      x[, result := as.numeric(get(d$result))]
    }
    stdates[[nm]] <- week_start(x$doi, o$woi_min_daily)
    # team
    sup <- if (!is.null(cfg$teams$variable) && cfg$teams$variable %in% names(x)) as.numeric(x[[cfg$teams$variable]]) else NULL
    x[, team := assign_teams(interv, sup, cfg, team_levels)]
    out[[nm]] <- x
  }
  # week of interview: int((doi - start)/7) + 1, missing if <= 0 (Stata truncation kept)
  anchor <- if (o$woi_anchor == "survey") {
    q <- intersect(c("hh", names(out)), names(out))[1]; stdates[[q]]
  } else NULL
  for (nm in names(out)) {
    s <- anchor %||% stdates[[nm]]
    out[[nm]][, woi := as.integer(trunc(as.numeric(doi - s) / 7) + 1)]
    out[[nm]][woi <= 0, woi := NA_integer_]
  }
  # team labels
  tl <- if (!is.null(cfg$teams$variable)) data.table(team = seq_along(team_levels), team_label = sprintf("T%d (%s)", seq_along(team_levels), team_levels))
        else data.table(team = seq_along(cfg$teams$ranges), team_label = sprintf("T%d", seq_along(cfg$teams$ranges)))
  for (nm in names(out)) {
    lab <- out[[nm]][!is.na(interv), .N, by = .(interv, team)][order(interv, -N)][, .SD[1], by = interv]
    lab[, interv_label := ifelse(is.na(team), sprintf("%g (no team)", interv), sprintf("%g (T%d)", interv, team))]
    out[[nm]] <- merge(out[[nm]], lab[, .(interv, interv_label)], by = "interv", all.x = TRUE, sort = FALSE)
    setcolorder(out[[nm]], c(setdiff(names(out[[nm]]), c(utility_vars, "interv_label")), intersect(c(utility_vars, "interv_label"), names(out[[nm]]))))
  }
  structure(list(ds = out, meta = data$meta, team_info = tl, stdates = stdates, cfg = cfg), class = "ipm_prepared")
}
