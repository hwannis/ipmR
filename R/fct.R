# Field Check Tables (FCT): the standard MICS7 tables of the central office (SPSS syntaxes 'MICS7 - Table Fxx.sps' of the
# MICS7 CAPI application 7.2.1), by team or interviewer, with the targets of 'targets.sps' set in the FCT parameters.
# Each table is a data.table (Total row, then one row per team), the columns described in `cols`, and the cells where the
# target is not met in `bad`. Counts and percentages only, no individual records.

#' Default parameters (targets) of the Field Check Tables
#'
#' The targets of 'targets.sps' and the thresholds written in the MICS7 table syntaxes. Expected numbers of eligible
#' members per household come from a previous survey or census; change them for the survey.
#' @return named list
#' @export
ipm_fct_defaults <- function() list(
  survey_label = "",                       # survey name and year in the titles ("" = the survey name of the settings)
  urban = "", rural = "",                  # codes of HH6 for urban and rural ("" = read from the value labels)
  t2w_u = 1.43, t2w_r = 1.26,              # expected eligible women per household (F2W)
  t2m_u = 1.37, t2m_r = 1.06,              # expected eligible men per selected household (F2M)
  t2c_u = 0.50, t2c_r = 0.83,              # expected children under 5 per household (F2UC)
  t2a_u = 1.24, t2a_r = 1.33,              # expected children 5-17 per household (F2UC)
  elig_share = 80,                         # target not met when the mean is below this % of the expected number
  t5w = 0.8, t5m = 0.8, t5u = 0.8, t5c = 0.8,   # age ratios (F5): target not met when the ratio is at or below
  ceb_prev = 2.01, ceb_share = 75,         # F10: mean children ever born in the previous survey, and % of it to reach
  age_int = 90,                            # F4: lowest % interviewed in any age group
  dob = 99,                                # F6, F11B, F11C: % with complete date of birth
  seen = 90,                               # F8: % of documents, nets and handwashing places seen
  heap = 1.5,                              # F9B, F9D: heaping ratio of 0 and 5 (at most)
  death12 = 1.5,                           # F11D: 12 months ratio of age at death (at most)
  mm_missing = 2,                          # F12: % of deceased sisters with missing age at death (below)
  bh_first_year = NA                       # F11A: first year of birth shown (NA = 13 years before the survey)
)

# parameters for the app: label, group, limits
fct_param_spec <- function() {
  p <- function(group, label, min = 0, max = 100, step = 0.01, help = "") list(group = group, label = label, min = min, max = max, step = step, help = help)
  list(
    t2w_u = p("elig", "Women 15-49 per household - urban", 0, 10), t2w_r = p("elig", "Women 15-49 per household - rural", 0, 10),
    t2m_u = p("elig", "Men 15-49 per selected household - urban", 0, 10), t2m_r = p("elig", "Men 15-49 per selected household - rural", 0, 10),
    t2c_u = p("elig", "Children under 5 per household - urban", 0, 10), t2c_r = p("elig", "Children under 5 per household - rural", 0, 10),
    t2a_u = p("elig", "Children 5-17 per household - urban", 0, 10), t2a_r = p("elig", "Children 5-17 per household - rural", 0, 10),
    elig_share = p("elig", "Target not met below (% of the expected number)", 10, 100, 1),
    t5w = p("age", "Age ratios of women (14/15, 25/24)", 0, 2, 0.05), t5m = p("age", "Age ratios of men (14/15, 25/24)", 0, 2, 0.05),
    t5u = p("age", "Age ratio of children 4/5", 0, 2, 0.05), t5c = p("age", "Age ratio of children 7/6", 0, 2, 0.05),
    age_int = p("age", "F4: lowest % interviewed in an age group", 50, 100, 1),
    dob = p("age", "F6, F11: % with month and year of birth", 50, 100, 1),
    ceb_prev = p("other", "F10: mean children ever born, previous survey", 0, 10), ceb_share = p("other", "F10: target = this % of it", 10, 100, 1),
    seen = p("other", "F8: % of cards, nets and handwashing places seen", 50, 100, 1),
    heap = p("other", "F9: heaping ratio of 0 and 5 (at most)", 1, 5, 0.1),
    death12 = p("other", "F11D: 12 months ratio of age at death (at most)", 1, 5, 0.1),
    mm_missing = p("other", "F12: % of sisters with missing age at death (below)", 0, 20, 0.5),
    bh_first_year = p("other", "F11A: first year of birth shown (empty = 13 years before)", 1900, 2100, 1))
}

#' The Field Check Tables
#' @return data.table(id, section, title, subtitle)
#' @export
ipm_fct_catalog <- function() {
  x <- data.table::rbindlist(lapply(fct_def, function(d) data.table(id = d$id, section = d$section, title = d$title, subtitle = d$sub, dataset = d$ds)))
  x
}

fct_sections <- c(resp = "Response rates", elig = "Eligible members per household", age = "Age and date of birth",
                  quest = "Answers to key questions", obs = "Observations", anthro = "Anthropometry",
                  bh = "Birth history and mortality", mod = "Privacy, food insecurity, phones")

fct_def <- local({
  d <- function(id, section, title, sub, ds) list(id = id, section = fct_sections[[section]], title = title, sub = sub, ds = ds)
  L <- list(
    d("F1", "resp", "Household response rate", "Percent distribution of sampled households by result of household interview", "hh"),
    d("F3W", "resp", "Response rate: Eligible women", "Percent distribution of eligible women age 15-49 years by result of individual interview", "wm"),
    d("F3M", "resp", "Response rate: Eligible men", "Percent distribution of eligible men age 15-49 years by result of individual interview", "mn"),
    d("F3U", "resp", "Response rate: Under 5", "Percent distribution of eligible children under age 5 by result of individual interview with mother/caretaker", "ch"),
    d("F3C", "resp", "Response rate: 5-17", "Percent distribution of selected children age 5-17 by result of individual interview with mother/caretaker", "fs"),
    d("F3C2", "resp", "Response rate: 7-14", "Percent distribution of selected children age 7-14 by result of learning assessment", "fs"),
    d("F3Q", "resp", "Response rate: Water Quality Testing", "Percent distribution of selected households by result of water quality testing", "hh"),
    d("F2W", "elig", "Eligible women per household", "Mean number of eligible women age 15-49 years per household", "hh"),
    d("F2M", "elig", "Eligible men per household", "Mean number of eligible men age 15-49 years per household selected for the men's questionnaire", "hh"),
    d("F2UC", "elig", "Eligible children per household", "Mean number of eligible children per household, and percentage of households with at least one child age 5-17 years", "hh"),
    d("F4W", "age", "Age distribution of women interviewed", "Number of women age 15-49 years listed in the household schedule and proportion successfully interviewed by age group", "hl"),
    d("F4M", "age", "Age distribution of men interviewed", "Number of men age 15-49 years listed in the household schedule and proportion successfully interviewed by age group", "hl"),
    d("F5W", "age", "Age displacement: women", "Number of women age 12-27 years listed in the household schedule by single year of age and age ratios 14/15 and 25/24", "hl"),
    d("F5M", "age", "Age displacement: men", "Number of men age 12-27 years listed in the household schedule by single year of age and age ratios 14/15 and 25/24", "hl"),
    d("F5UC", "age", "Age displacement: young children", "Number of children age 0-9 years listed in the household schedule by single year of age and age ratios 4/5 and 7/6", "hl"),
    d("F6W", "age", "Birth date reporting: Women", "Percent distribution of women age 15-49 years by completeness of date of birth/age reporting information", "wm"),
    d("F6W2", "age", "Birth date reporting: Women (month)", "Percent distribution of women age 15-49 years by month of birth", "wm"),
    d("F6M", "age", "Birth date reporting: Men", "Percent distribution of men age 15-49 years by completeness of date of birth/age reporting information", "mn"),
    d("F6U", "age", "Birth date reporting: Children under 5", "Percent distribution of children under age 5 by completeness of date of birth/age reporting information", "ch"),
    d("F6C", "age", "Birth date reporting: Children age 5-17", "Percent distribution of children age 5-17 years by completeness of date of birth/age reporting information", "fs"),
    d("F6B", "age", "Birth date reporting: First and last births", "Percent distribution of first and last births to women age 15-49 years by completeness of date of birth", "wm"),
    d("F7W", "quest", "Question responses: Women's questionnaire", "Percentage of certain responses to key questions in the women's questionnaire", "wm"),
    d("F7M", "quest", "Question responses: Men's questionnaire", "Percentage of certain responses to key questions in the men's questionnaire", "mn"),
    d("F7U", "quest", "Question responses: Under-5 questionnaire", "Percentage of certain responses to key questions in the under-5 questionnaire", "ch"),
    d("F7C", "quest", "Question responses: 5-17 questionnaire", "Percentage of certain responses to key questions in the 5-17 questionnaire", "fs"),
    d("F8A", "obs", "Observation of birth certificates", "Percentage of children under 5 with a birth certificate, whose certificate was seen, whose birth is registered, and proportion of certificates seen", "ch"),
    d("F8B", "obs", "Observation of vaccination cards", "Percentage of children under age 3 with a vaccination card, whose card was seen, and proportion of cards seen", "ch"),
    d("F8C", "obs", "Observation of women's health cards", "Percentage of women with a live birth in the last 2 years with a health card, whose card was seen, and proportion of cards seen", "wm"),
    d("F8D", "obs", "Observation of bednets", "Percentage of bednets observed by the interviewer in all interviewed households", "tn"),
    d("F8E", "obs", "Observation of places for handwashing", "Percentage of places for handwashing observed by the interviewer in all interviewed households", "hh"),
    d("F8Q", "obs", "Observation and collection of water quality samples", "Observed sources of the household water sample, collected source samples and blank tests with E. coli detected", "hh"),
    d("F9A", "anthro", "Height and weight of children (age 0-4)", "Percent distribution of children under 5 by result of measurement, and percentage of measured children with out of range values, interference or incomplete date of birth", "ch"),
    d("F9B", "anthro", "Heaping in anthropometric measurements (age 0-4)", "Percentage of weight and height/length measurements with last digit 0 and 5 among measured children age 0-4 years", "ch"),
    d("F9C", "anthro", "Height and weight of children (age 5-9)", "Percent distribution of children age 5-9 by result of measurement, and percentage of measured children with interference or incomplete date of birth", "fs"),
    d("F9D", "anthro", "Heaping in anthropometric measurements (age 5-9)", "Percentage of weight and height measurements with last digit 0 and 5 among measured children age 5-9 years", "fs"),
    d("F10", "bh", "Children ever born and children surviving", "Number of women with a completed interview, total number of children ever born (CEB), and mean number of CEB", "wm"),
    d("F11A", "bh", "Birth histories: Birth displacement", "Number of births by year of birth in birth histories", "bh"),
    d("F11B", "bh", "Birth histories: Date of birth of living children", "Percent distribution of living children by completeness of date of birth/age information in birth histories", "bh"),
    d("F11C", "bh", "Birth histories: Date of birth of deceased children", "Percent distribution of deceased children by completeness of date of birth information in birth histories", "bh"),
    d("F11D", "bh", "Birth histories: Age at death heaping", "Number of deaths in the last 15 years by reported months of age at death (including age at death reported as 'one year') and the 12 months ratio", "bh"),
    d("F11E", "bh", "Birth histories: Child mortality", "Number of births in the last 15 years by survival status and age at death, ratio of neonatal to infant deaths, and infant deaths per 1,000 births", "bh"),
    d("F12", "bh", "Maternal mortality", "Number of deceased sisters and percentage with missing age at death; sisters who died at age 12+ and percentage with missing information on death during pregnancy, delivery or after delivery, and on the time of death", "mm"),
    d("F13AW", "mod", "Mental health module: Privacy (women)", "Percentage of women age 15-24 to whom the mental health module, and its suicide questions, were not administered because privacy was not possible", "wm"),
    d("F13AM", "mod", "Mental health module: Privacy (men)", "Percentage of men age 15-24 to whom the mental health module, and its suicide questions, were not administered because privacy was not possible", "mn"),
    d("F13B", "mod", "Violence against women module: Privacy", "Percentage of women selected for the violence against women module to whom it was not administered because privacy was not possible", "wm"),
    d("F14", "mod", "Food insecurity", "Percentage of households with all answers missing, or all 'No', to the Food Insecurity Experience questions", "hh"),
    d("F20", "mod", "Phone ownership and MICS Plus consent", "Percentage of households with a fixed or mobile telephone, and with at least one consent and one phone number given for the MICS Plus survey", "hh"))
  stats::setNames(L, vapply(L, `[[`, "", "id"))
})

# ---------- reading the data ----------

# which dataset plays which role (household, members, women...), from the variables of each file
fct_role_test <- list(hh = "HH46", hl = c("HL4", "HL6"), wm = "WM17", mn = "MWM17", ch = "UF17", fs = "FS17", bh = c("BH4Y", "BH5"), mm = c("MM15", "MM16"), tn = "TN3")

# variables read per role (patterns), all optional
fct_vars <- list(
  hh = c("HH6", "HH7", "HH8", "HH9", "HH46", "HH48", "HH49", "HH50", "HH51", "HH52", "HW1", "FE[1-8]", "HC7A", "HC12", "HH44BC", "HH44DE",
         "WQ12", "WQ18", "WQ19", "WQ20", "WQ29", "WQ31"),
  hl = c("HL1", "HL4", "HL6", "HL9"),
  wm = c("LN", "WM3", "WM6Y", "WM17", "WB3M", "WB3Y", "WB4", "CM1", "CM8", "CM11", "CM17", "CP1", "FG1", "FG2", "SB1", "MN7", "CEB", "CSURV", "CDEAD",
         "MH1", "MH40", "VW1", "WM15EF", "WM15GH"),
  mn = c("LN", "MWM3", "MWM17", "MWB3M", "MWB3Y", "MWB4", "MSB1", "MMH1", "MMH40", "MWM15EF", "MWM15GH"),
  ch = c("LN", "UF3", "UF4", "UF17", "UB1M", "UB1Y", "UB2", "CAGE", "CA1", "CA14", "CA16", "UCD2[A-K]", "UCF7", "UCF9", "UCF1[2-8]", "BD3", "BD7[A-Z]", "BD8[A-Z]",
         "BR1", "BR2", "IM2", "IM5", "AN8", "AN9", "AN11", "AN13", "HAZFLAG", "WAZFLAG", "WHZFLAG", "UF15EF", "UF15GH"),
  fs = c("LN", "FS3", "FS4", "FS17", "CB2M", "CB2Y", "CB3", "FL28", "FCF6", "FCF8", "FCF1[4-9]", "FCF2[0-4]", "FCD2[A-K]", "FA8", "FA9", "FA11", "FA13", "FS15EF", "FS15GH"),
  bh = c("LN", "WM3", "BH0", "BH4M", "BH4Y", "BH5", "BH6", "BH9U", "BH9N"),
  mm = c("MM15", "MM16", "MM18", "MM19", "MM22", "MM23", "MM24"),
  tn = c("TN3"))

fct_find_roles <- function(cfg, headers) {
  roles <- list()
  for (r in names(fct_role_test)) {
    cand <- names(headers)[vapply(headers, function(v) all(fct_role_test[[r]] %in% v), logical(1))]
    if (r == "hh") cand <- cand[!vapply(headers[cand], function(v) any(c("HL4", "WM17", "UF17") %in% v), logical(1))]
    if (length(cand)) roles[[r]] <- if (r %in% cand) r else cand[1]
  }
  roles
}

#' Read the data for the Field Check Tables
#'
#' Finds the household, household members, women, men, under-5, 5-17, birth history, maternal mortality and bednet files
#' among the configured datasets (from their variables), and reads only the variables the tables use, with the team,
#' interviewer and date of interview as in the analysis. All records are kept (not only the completed interviews).
#' @param cfg configuration (with `data_dir`)
#' @return list(ds, meta, team_info, roles, cfg)
#' @export
ipm_fct_data <- function(cfg) {
  cfg <- normalise_config(cfg)
  headers <- list()
  for (nm in names(cfg$datasets)) {
    f <- file.path(cfg$data_dir, cfg$datasets[[nm]]$file)
    if (file.exists(f)) headers[[nm]] <- toupper(names(read_any(f, n_max = 0)))
  }
  roles <- fct_find_roles(cfg, headers)
  if (is.null(roles$hh)) stop("No household questionnaire (with HH46, result of the interview) among the datasets")
  use <- unique(unlist(roles))
  repeat { p <- unlist(lapply(use, function(n) cfg$datasets[[n]]$parent)); p <- setdiff(p, use); if (!length(p)) break; use <- c(use, p) }
  c2 <- cfg; c2$datasets <- cfg$datasets[names(cfg$datasets) %in% use]; c2$extras <- list()
  c2$options$drop_long_strings <- TRUE; c2$options$filter_parent_result <- FALSE
  for (nm in names(c2$datasets)) {
    r <- names(roles)[vapply(roles, identical, logical(1), nm)]
    v <- headers[[nm]]; want <- if (length(r)) v[Reduce(`|`, lapply(fct_vars[[r[1]]], function(p) grepl(paste0("^", p, "$"), v)), FALSE)] else character()
    c2$datasets[[nm]]$flag_modules <- paste(want, collapse = " "); c2$datasets[[nm]]$flag_units <- character()
  }
  dat <- ipm_load(c2, quiet = TRUE, slim = TRUE)
  pr <- ipm_prepare(dat, c2, quiet = TRUE, inplace = TRUE)
  roles <- roles[vapply(roles, function(n) !is.null(pr$ds[[n]]), logical(1))]
  list(ds = lapply(roles, function(n) pr$ds[[n]]), meta = lapply(roles, function(n) dat$meta[[n]]), team_info = pr$team_info,
       roles = roles, cfg = cfg, read_at = Sys.time())
}

# ---------- helpers ----------
lt <- function(a, b) !is.na(a) & a < b                 # SPSS comparisons: a missing value never meets the condition
ge <- function(a, b) !is.na(a) & a >= b
is1 <- function(a, v = 1) !is.na(a) & a %in% v
p100 <- function(cond) if (!length(cond)) NA_real_ else 100 * mean(cond)
ratio <- function(a, b) if (is.na(b) || b == 0) NA_real_ else a / b
near <- function(a, b) !is.na(a) & abs(a - b) < 1e-6
has <- function(x, v) !is.null(x) && all(v %in% names(x))

# Total row, then one row per team / interviewer
fct_rows <- function(x, f) {
  keys <- sort(unique(x$.g), na.last = TRUE)
  labs <- x$.lab[match(keys, x$.g)]
  f2 <- function(s) Filter(Negate(is.null), f(s))
  r <- lapply(keys, function(k) f2(x[if (is.na(k)) is.na(x$.g) else (!is.na(x$.g) & x$.g == k)]))
  out <- data.table::rbindlist(c(list(f2(x)), r), fill = TRUE)
  out[, `:=`(.row = c("Total", labs), .key = c(NA, keys))]
  data.table::setcolorder(out, c(".row", ".key"))
  out
}

# value labels of codes
fct_codelab <- function(meta, var, codes) {
  vl <- meta$vallab[[var]]
  l <- if (is.null(vl)) rep(NA_character_, length(codes)) else names(vl)[match(codes, unname(vl))]
  l <- ifelse(!is.na(l) & l == toupper(l), paste0(substr(l, 1, 1), tolower(substr(l, 2, nchar(l)))), l)   # "NOT AT HOME" -> "Not at home"
  ifelse(is.na(l), as.character(codes), sprintf("%s (%s)", l, codes))
}

# percent distribution of a coded variable (valid cases), Total = 100, and the number of records
fct_dist <- function(x, var, codes) {
  fct_rows(x, function(s) {
    v <- s[[var]]; n <- sum(!is.na(v))
    p <- if (n) 100 * tabulate(match(v, codes), length(codes)) / n else rep(NA_real_, length(codes))
    c(stats::setNames(as.list(p), paste0("c", codes)), list(tot = if (n) 100 else NA_real_, n = nrow(s)))
  })
}

cols_dt <- function(name, label, fmt, group = "") data.table(name = name, label = label, fmt = fmt, group = group)

fct_tab <- function(id, d, cols, bad = list(), plots = list(), note = NULL, ds = NULL) {
  def <- fct_def[[id]]
  cols <- cols[name %in% names(d)]
  plots <- Filter(function(z) any(z$cols %in% names(d)), plots)
  list(id = id, title = paste0(id, ". ", def$title), subtitle = def$sub, section = def$section, data = d, cols = cols,
       bad = bad[names(bad) %in% names(d)], plots = plots, note = note, available = TRUE)
}
fct_na <- function(id, why) { def <- fct_def[[id]]; list(id = id, title = paste0(id, ". ", def$title), subtitle = def$sub, section = def$section, available = FALSE, why = why) }

# "target not met" column: the value where it is not met (as in the MICS syntaxes)
target_col <- function(d, col, fail, name) { d[, (name) := ifelse(fail, d[[col]], NA_real_)]; invisible(d) }

# completeness of date of birth (F6W, F6M, F6U, F6C): month and year, year and age, age only, year only, other
fct_dob <- function(id, x, M, Y, A, n_label, p) {
  if (is.null(x) || !has(x, c(M, Y, A))) return(fct_na(id, sprintf("needs %s, %s and %s", M, Y, A)))
  x[, `:=`(.myb = lt(get(M), 98) & lt(get(Y), 9998), .yab = ge(get(M), 98) & lt(get(Y), 9998) & lt(get(A), 98),
           .ab = ge(get(M), 98) & ge(get(Y), 9998) & lt(get(A), 98), .yb = ge(get(M), 98) & lt(get(Y), 9998) & ge(get(A), 98))]
  d <- fct_rows(x, function(s) list(myb = p100(s$.myb), yab = p100(s$.yab), yb = p100(s$.yb), ab = p100(s$.ab),
                                    nd = p100(!(s$.myb | s$.yab | s$.yb | s$.ab)), tot = if (nrow(s)) 100 else NA_real_, n = nrow(s)))
  fail <- !is.na(d$myb) & d$myb < p$dob; target_col(d, "myb", fail, "tnm")
  g <- "Completeness of reporting"
  fct_tab(id, d, rbind(cols_dt(c("myb", "yab", "yb", "ab", "nd"), c("Year and month of birth", "Year of birth and age", "Year of birth only", "Age only", "Other/No data"), "pct", g),
                       cols_dt(c("tot", "n", "tnm"), c("Total", n_label, "Target not met"), c("pct", "n", "pct"))),
          bad = list(myb = fail, tnm = fail), plots = list(list(kind = "stack", cols = c("myb", "yab", "yb", "ab", "nd")),
                                                           list(kind = "bars", cols = "myb", target = p$dob, title = "Year and month of birth (%)")),
          note = sprintf("Target: at least %s%% with year and month of birth.", p$dob))
}

# heaping of the last decimal (F9B, F9D)
fct_heap <- function(id, x, W, H, age, p) {
  if (is.null(x) || !has(x, c(W, H))) return(fct_na(id, sprintf("needs %s and %s (anthropometry)", W, H)))
  dec <- function(v) round(v * 10) %% 10
  d <- fct_rows(x, function(s) {
    w <- s[[W]][lt(s[[W]], 99.3)]; h <- s[[H]][lt(s[[H]], 999.3)]
    w0 <- p100(dec(w) == 0); w5 <- p100(dec(w) == 5); h0 <- p100(dec(h) == 0); h5 <- p100(dec(h) == 5)
    list(w0 = w0, w5 = w5, wr = (w0 + w5) / 20, wn = length(w), h0 = h0, h5 = h5, hr = (h0 + h5) / 20, hn = length(h))
  })
  fw <- !is.na(d$wr) & d$wr > p$heap; fh <- !is.na(d$hr) & d$hr > p$heap
  target_col(d, "wr", fw, "wt"); target_col(d, "hr", fh, "ht")
  data.table::setcolorder(d, c(".row", ".key", "w0", "w5", "wr", "wt", "wn", "h0", "h5", "hr", "ht", "hn"))
  fct_tab(id, d, rbind(cols_dt(c("w0", "w5", "wr", "wt", "wn"), c("% with digit 0", "% with digit 5", "Heaping ratio", "Target not met", sprintf("Children %s with weight", age)), c("pct", "pct", "r2", "r2", "n"), "Weight measurements"),
                       cols_dt(c("h0", "h5", "hr", "ht", "hn"), c("% with digit 0", "% with digit 5", "Heaping ratio", "Target not met", sprintf("Children %s with height", age)), c("pct", "pct", "r2", "r2", "n"), "Height measurements")),
          bad = list(wr = fw, wt = fw, hr = fh, ht = fh),
          plots = list(list(kind = "bars", cols = c("wr", "hr"), labels = c("Weight", "Height"), target = p$heap, title = "Heaping ratio of the last digit (0 and 5)")),
          note = sprintf("Heaping ratio = (%% ending in 0 + %% ending in 5) / 20: 1 = no heaping. Target: at most %s.", p$heap))
}

# measurement result (F9A, F9C)
fct_measure <- function(id, x, W, H, cloth, hair, M, Y, flags, age, p) {
  if (is.null(x) || !has(x, c(W, H))) return(fct_na(id, sprintf("needs %s and %s (anthropometry)", W, H)))
  w <- x[[W]]
  x[, .m := data.table::fifelse(lt(w, 99) & lt(get(H), 999), 1, data.table::fifelse(near(w, 99.3), 2, data.table::fifelse(near(w, 99.4) | near(w, 99.5), 3, data.table::fifelse(near(w, 99.6), 6, 9))))]
  codes <- c(1, 2, 3, 6, 9)
  d <- fct_rows(x, function(s) {
    n <- nrow(s); m <- s[.m == 1]
    o <- c(stats::setNames(as.list(if (n) 100 * tabulate(match(s$.m, codes), 5) / n else rep(NA_real_, 5)), paste0("c", codes)), list(tot = if (n) 100 else NA_real_, n = n))
    if ("HAZFLAG" %in% flags && has(m, "HAZFLAG")) o$hout <- p100(!is.na(m$HAZFLAG) & m$HAZFLAG != 0)
    if (has(m, hair)) o$hair <- p100(is1(m[[hair]]))
    if ("WAZFLAG" %in% flags && has(m, "WAZFLAG")) o$wout <- p100(!is.na(m$WAZFLAG) & m$WAZFLAG != 0)
    if (has(m, cloth)) o$cloth <- p100(is1(m[[cloth]], 2))
    if ("WHZFLAG" %in% flags && has(m, "WHZFLAG")) o$whout <- p100(!is.na(m$WHZFLAG) & m$WHZFLAG != 0)
    if (has(m, c(M, Y))) o$dobinc <- p100(!is.na(m[[M]]) & !is.na(m[[Y]]) & !(m[[M]] < 97 & m[[Y]] < 9997))
    o
  })
  g <- "Result of height and weight measurement"
  fct_tab(id, d, rbind(cols_dt(paste0("c", codes), c("Either or both measured", "Child not present", "Child or mother/caretaker refused", "Other", "Missing"), "pct", g),
                       cols_dt(c("tot", "n"), c("Total", sprintf("Number of children age %s", age)), c("pct", "n")),
                       cols_dt(c("hout", "hair", "wout", "cloth", "whout", "dobinc"), c("Height out of range (%)", "Hair interference (%)", "Weight out of range (%)",
                                                                                      "Clothing interference (%)", "Weight for height out of range (%)", "Date of birth incomplete (%)"), "pct", "Among children measured")),
          plots = list(list(kind = "stack", cols = paste0("c", codes)),
                       list(kind = "bars", cols = intersect(c("hout", "wout", "whout", "dobinc"), names(d)), labels = c(hout = "Height out of range", wout = "Weight out of range", whout = "Weight for height out of range", dobinc = "Date of birth incomplete")[intersect(c("hout", "wout", "whout", "dobinc"), names(d))], title = "Among children measured (%)")))
}

# single years of age and two age ratios (F5W, F5M, F5UC)
fct_ages <- function(id, x, ages, r1, r2, t1, t2, lab, p) {
  if (is.null(x) || !has(x, c("HL4", "HL6"))) return(fct_na(id, "needs HL4 and HL6 (household members)"))
  d <- fct_rows(x, function(s) { a <- s$HL6; cnt <- vapply(ages, function(k) sum(a == k, na.rm = TRUE), 0)
    c(stats::setNames(as.list(cnt), paste0("a", ages)), list(tot = sum(cnt), r1 = ratio(cnt[ages == r1[1]], cnt[ages == r1[2]]), r2 = ratio(cnt[ages == r2[1]], cnt[ages == r2[2]]))) })
  f1 <- !is.na(d$r1) & d$r1 <= t1; f2 <- !is.na(d$r2) & d$r2 <= t2
  target_col(d, "r1", f1, "t1"); target_col(d, "r2", f2, "t2")
  data.table::setcolorder(d, c(".row", ".key", paste0("a", ages), "tot", "r1", "t1", "r2", "t2"))
  rl <- function(r) sprintf("Age ratio (%s/%s)", r[1], r[2])
  fct_tab(id, d, rbind(cols_dt(paste0("a", ages), as.character(ages), "n", lab), cols_dt(c("tot", "r1", "t1", "r2", "t2"), c("Total", rl(r1), "Target not met", rl(r2), "Target not met "), c("n", "r2", "r2", "r2", "r2"))),
          bad = list(r1 = f1, t1 = f1, r2 = f2, t2 = f2),
          plots = list(list(kind = "profile", cols = paste0("a", ages), labels = as.character(ages), mark = paste0("a", unique(c(r1, r2))), title = paste(lab, "- all teams")),
                       list(kind = "bars", cols = c("r1", "r2"), labels = c(rl(r1), rl(r2)), target = c(t1, t2), ref = 1, title = "Age ratios")),
          note = sprintf("Target not met when the ratio is at or below %s (%s) or %s (%s), as in the MICS7 syntax. A ratio close to 1 is expected; ages pushed out of the eligible range show as a gap.", t1, rl(r1), t2, rl(r2)))
}

# percentage of documents seen (F8A, F8B, F8C)
fct_seen <- function(id, x, has_card, seen, n_label, extra = NULL, p) {
  d <- fct_rows(x, function(s) {
    hc <- has_card(s); sn <- seen(s)
    c(list(card = p100(hc), cseen = p100(sn)), if (!is.null(extra)) extra(s), list(n = nrow(s), pseen = if (any(hc)) 100 * sum(sn & hc) / sum(hc) else NA_real_))
  })
  fail <- !is.na(d$pseen) & d$pseen < p$seen; target_col(d, "pseen", fail, "tnm")
  d
}

# ---------- the tables ----------
fct_build <- list(
  F1 = function(D, p) {
    x <- D$hh; if (!has(x, "HH46")) return(fct_na("F1", "needs HH46"))
    codes <- sort(unique(stats::na.omit(x$HH46)))
    d <- fct_dist(x, "HH46", codes)
    rr <- fct_rows(x, function(s) list(rr = { o <- sum(s$HH46 %in% c(1, 2, 4, 7)); if (o) 100 * sum(s$HH46 %in% 1) / o else NA_real_ }))
    d[, rr := rr$rr]
    fct_tab("F1", d, rbind(cols_dt(paste0("c", codes), fct_codelab(D$meta$hh, "HH46", codes), "pct", "Result of household interview"),
                           cols_dt(c("tot", "n", "rr"), c("Total", "Number of households", "Household response rate (%)"), c("pct", "n", "pct"))),
            plots = list(list(kind = "stack", cols = paste0("c", codes)), list(kind = "bars", cols = "rr", title = "Household response rate (%)")),
            note = "Household response rate = completed / (completed + no competent respondent + refused + dwelling not found), codes (1) / (1+2+4+7).")
  },
  F2W = function(D, p) fct_elig("F2W", D, list(list(v = "HH49", tu = p$t2w_u, tr = p$t2w_r, lab = "eligible women")), p),
  F2M = function(D, p) fct_elig("F2M", D, list(list(v = "HH50", tu = p$t2m_u, tr = p$t2m_r, lab = "eligible men", sel = "HH8")), p),
  F2UC = function(D, p) fct_elig("F2UC", D, list(list(v = "HH51", tu = p$t2c_u, tr = p$t2c_r, lab = "children under 5"),
                                                list(v = "HH52", tu = p$t2a_u, tr = p$t2a_r, lab = "children 5-17", any = TRUE)), p),
  F3W = function(D, p) fct_resp("F3W", D, "wm", "WM17", "Number of women", "Result of individual interview (Women's questionnaire)", age = "own", n15 = "Number of women age 15-17 years"),
  F3M = function(D, p) fct_resp("F3M", D, "mn", "MWM17", "Number of men", "Result of individual interview (Men's questionnaire)", age = "own", n15 = "Number of men age 15-17 years"),
  F3U = function(D, p) fct_resp("F3U", D, "ch", "UF17", "Number of children", "Result of individual interview (Under-5 questionnaire)", age = "UF4", n15 = "Number of mothers/caretakers age 15-17 years"),
  F3C = function(D, p) {
    t <- fct_resp("F3C", D, "fs", "FS17", "Number of children", "Result of individual interview (5-17 questionnaire)", age = "all")
    x <- D$fs; if (!isTRUE(t$available) || !has(x, c("CB3", "FS4"))) return(t)
    e <- fct_rows(x, function(s) { k <- s[CB3 >= 15 & CB3 <= 17]; list(eman = p100(is1(k$FS4, 90)), n1517 = nrow(k)) })
    t$data[, `:=`(eman = e$eman, n1517 = e$n1517)]
    t$cols <- rbind(t$cols, cols_dt(c("eman", "n1517"), c("Percentage emancipated children", "Number of selected children age 15-17 years"), c("pct", "n")))
    t
  },
  F3C2 = function(D, p) {
    x <- D$fs; if (!has(x, c("CB3", "FL28"))) return(fct_na("F3C2", "needs CB3 and FL28 (foundational learning)"))
    x <- x[!is.na(CB3) & CB3 >= 7 & CB3 <= 14]; codes <- sort(unique(stats::na.omit(x$FL28)))
    d <- fct_dist(x, "FL28", codes)
    fct_tab("F3C2", d, rbind(cols_dt(paste0("c", codes), fct_codelab(D$meta$fs, "FL28", codes), "pct", "Result of learning assessment (age 7-14)"),
                             cols_dt(c("tot", "n"), c("Total", "Number of children"), c("pct", "n"))),
            plots = list(list(kind = "stack", cols = paste0("c", codes))))
  },
  F3Q = function(D, p) {
    x <- D$hh; if (!has(x, c("HH9", "WQ31"))) return(fct_na("F3Q", "needs HH9 and WQ31 (water quality testing)"))
    x <- x[is1(HH9) & is1(HH46)]; x[is.na(WQ31), WQ31 := 95]
    codes <- sort(unique(x$WQ31)); lab <- fct_codelab(D$meta$hh, "WQ31", codes); lab[codes == 95] <- "Results not yet entered (95)"
    d <- fct_dist(x, "WQ31", codes)
    fct_tab("F3Q", d, rbind(cols_dt(paste0("c", codes), lab, "pct", "Result of water quality testing"), cols_dt(c("tot", "n"), c("Total", "Number of households selected"), c("pct", "n"))),
            plots = list(list(kind = "stack", cols = paste0("c", codes))))
  },
  F4W = function(D, p) fct_agedist("F4W", D, 2, "wm", "WM17", "women", p),
  F4M = function(D, p) fct_agedist("F4M", D, 1, "mn", "MWM17", "men", p),
  F5W = function(D, p) fct_ages("F5W", if (has(D$hl, "HL4")) D$hl[is1(HL4, 2)], 12:27, c(14, 15), c(25, 24), p$t5w, p$t5w, "Women's age (12-27 years)", p),
  F5M = function(D, p) fct_ages("F5M", if (has(D$hl, "HL4")) D$hl[is1(HL4, 1)], 12:27, c(14, 15), c(25, 24), p$t5m, p$t5m, "Men's age (12-27 years)", p),
  F5UC = function(D, p) fct_ages("F5UC", D$hl, 0:9, c(4, 5), c(7, 6), p$t5u, p$t5c, "Children's age (0-9 years)", p),
  F6W = function(D, p) fct_dob("F6W", if (has(D$wm, "WM17")) D$wm[is1(WM17)], "WB3M", "WB3Y", "WB4", "Number of women", p),
  F6W2 = function(D, p) {
    x <- D$wm; if (!has(x, c("WM17", "WB3M"))) return(fct_na("F6W2", "needs WB3M"))
    x <- x[is1(WM17)]; codes <- sort(unique(stats::na.omit(x$WB3M)))
    d <- fct_dist(x, "WB3M", codes)
    fct_tab("F6W2", d, rbind(cols_dt(paste0("c", codes), fct_codelab(D$meta$wm, "WB3M", codes), "pct", "Month of birth"), cols_dt(c("tot", "n"), c("Total", "Number of women"), c("pct", "n"))),
            plots = list(list(kind = "profile", cols = paste0("c", codes), labels = as.character(codes), title = "Month of birth (%) - all teams")),
            note = "Each month should have about 8% of the women; peaks on January or on 'don't know' mean the month was guessed.")
  },
  F6M = function(D, p) fct_dob("F6M", if (has(D$mn, "MWM17")) D$mn[is1(MWM17)], "MWB3M", "MWB3Y", "MWB4", "Number of men with completed interview", p),
  F6U = function(D, p) fct_dob("F6U", if (has(D$ch, "UF17")) D$ch[is1(UF17)], "UB1M", "UB1Y", "UB2", "Number of children under 5", p),
  F6C = function(D, p) fct_dob("F6C", if (has(D$fs, "FS17")) D$fs[is1(FS17)], "CB2M", "CB2Y", "CB3", "Number of children age 5-17", p),
  F6B = function(D, p) {
    x <- D$wm; b <- D$bhx
    if (!has(x, c("WM17", "CM1")) || !has(b, c("BH4M", "BH4Y")) || is.null(D$wline) || is.null(D$bline)) return(fct_na("F6B", "needs the birth history (BH4M, BH4Y) and CM1"))
    o <- if ("BH0" %in% names(b)) "BH0" else NULL
    bb <- b[, c("HH1", "HH2", D$bline, o, "BH4M", "BH4Y", intersect("BH6", names(b))), with = FALSE]
    data.table::setnames(bb, D$bline, ".ln"); for (z in c("HH1", "HH2", ".ln")) bb[, (z) := as.numeric(get(z))]
    if (!is.null(o)) data.table::setorderv(bb, c("HH1", "HH2", ".ln", o))
    fl <- bb[, .(fM = BH4M[1], fY = BH4Y[1], f6 = if ("BH6" %in% names(bb)) BH6[1] else NA_real_, lM = BH4M[.N], lY = BH4Y[.N]), by = .(HH1, HH2, .ln)]
    x <- x[is1(WM17) & (is1(CM1) | (is1(CM1, 2) & is1(CM8)))]
    x[, .ln := as.numeric(get(D$wline))]; x[, `:=`(HH1 = as.numeric(HH1), HH2 = as.numeric(HH2))]; x <- merge(x, fl, by = c("HH1", "HH2", ".ln"), all.x = TRUE, sort = FALSE)
    last <- function(s) if ("CM11" %in% names(s)) s[!is.na(CM11) & CM11 > 1] else s[0]
    d <- fct_rows(x, function(s) {
      fm <- lt(s$fM, 97) & lt(s$fY, 9997); fy <- ge(s$fM, 97) & lt(s$fY, 9997); fa <- ge(s$fM, 97) & ge(s$fY, 9997) & lt(s$f6, 97)
      l <- last(s); lm <- lt(l$lM, 97) & lt(l$lY, 9997); ly <- ge(l$lM, 97) & lt(l$lY, 9997)
      list(fm = p100(fm), fy = p100(fy), fa = p100(fa), fnd = p100(!(fm | fy | fa)), ftot = if (nrow(s)) 100 else NA_real_, fn = nrow(s),
           lm = p100(lm), ly = p100(ly), lnd = p100(!(lm | ly)), ltot = if (nrow(l)) 100 else NA_real_, lnum = nrow(l))
    })
    ff <- !is.na(d$fm) & d$fm < p$dob; lf <- !is.na(d$lm) & d$lm < p$dob
    target_col(d, "fm", ff, "ft"); target_col(d, "lm", lf, "lt")
    data.table::setcolorder(d, c(".row", ".key", "fm", "fy", "fa", "fnd", "ftot", "fn", "ft", "lm", "ly", "lnd", "ltot", "lnum", "lt"))
    fct_tab("F6B", d, rbind(cols_dt(c("fm", "fy", "fa", "fnd", "ftot", "fn", "ft"), c("Both month and year", "Year only", "Completed years since first birth only", "Other/DK/Missing", "Total", "Number of first births", "Target not met"),
                                    c("pct", "pct", "pct", "pct", "pct", "n", "pct"), "Date of first birth"),
                            cols_dt(c("lm", "ly", "lnd", "ltot", "lnum", "lt"), c("Both month and year", "Year only", "Other/DK/Missing", "Total", "Number of last births", "Target not met"), c("pct", "pct", "pct", "pct", "n", "pct"), "Date of last birth")),
            bad = list(fm = ff, ft = ff, lm = lf, lt = lf),
            plots = list(list(kind = "bars", cols = c("fm", "lm"), labels = c("First birth", "Last birth"), target = p$dob, title = "Month and year of birth given (%)")),
            note = sprintf("Women with a live birth. Target: at least %s%% with month and year.", p$dob))
  },
  F7W = function(D, p) {
    x <- D$wm; if (!has(x, "WM17")) return(fct_na("F7W", "needs WM17"))
    x <- x[is1(WM17)]
    d <- fct_rows(x, function(s) list(lbirth = if (has(s, "CM17")) p100(is1(s$CM17)), pregn = if (has(s, "CP1")) p100(is1(s$CP1)),
                                      fgm = if (has(s, "FG1")) p100(is1(s$FG1) | (if (has(s, "FG2")) is1(s$FG2) else FALSE)),
                                      esex = if (has(s, "SB1")) p100(!is.na(s$SB1) & s$SB1 > 0 & s$SB1 < 97), n = nrow(s)))
    cl <- cols_dt(c("lbirth", "pregn", "fgm", "esex"), c("With a live birth in the last 2 years", "Currently pregnant", "Who have ever heard of FGM", "Who have ever had sex"), "pct", "Percentage of women")
    fct_tab("F7W", d, rbind(cl, cols_dt("n", "Number of women", "n")),
            plots = list(list(kind = "bars", cols = intersect(cl$name, names(d)), labels = stats::setNames(cl$label, cl$name)[intersect(cl$name, names(d))], title = "Percentage of women")),
            note = "Columns are left out when the topic is not in the questionnaire. A team far from the others may be skipping questions to shorten the interview.")
  },
  F7M = function(D, p) {
    x <- D$mn; if (!has(x, c("MWM17", "MSB1"))) return(fct_na("F7M", "needs MSB1 (sexual behaviour, men)"))
    d <- fct_rows(x[is1(MWM17)], function(s) list(esex = p100(!is.na(s$MSB1) & s$MSB1 > 0 & s$MSB1 < 97), n = nrow(s)))
    fct_tab("F7M", d, cols_dt(c("esex", "n"), c("Ever had sex (%)", "Number of men"), c("pct", "n")), plots = list(list(kind = "bars", cols = "esex", title = "Men who ever had sex (%)")))
  },
  F7U = function(D, p) {
    x <- D$ch; if (!has(x, "UF17")) return(fct_na("F7U", "needs UF17"))
    x <- x[is1(UF17)]
    disc <- grep("^UCD2[A-K]$", names(x), value = TRUE); cf <- intersect(c("UCF7", "UCF9", "UCF14", "UCF15", "UCF16", "UCF17", "UCF18"), names(x))
    b7 <- grep("^BD7[A-Z]$", names(x), value = TRUE); b8 <- grep("^BD8[A-Z]$", names(x), value = TRUE)
    allnot1 <- function(s, v) Reduce(`&`, lapply(v, function(z) !is.na(s[[z]]) & s[[z]] != 1))
    d <- fct_rows(x, function(s) {
      o <- list(diarrhea = if (has(s, "CA1")) p100(is1(s$CA1)), cough = if (has(s, "CA16")) p100(is1(s$CA16)), fever = if (has(s, "CA14")) p100(is1(s$CA14)), n = nrow(s))
      if (length(disc) && has(s, "UB2")) { k <- s[!is.na(UB2) & UB2 > 0]; o$nodisc <- p100(allnot1(k, disc)); o$n14 <- nrow(k) }
      if (length(cf) == 7 && has(s, "UB2")) { k <- s[!is.na(UB2) & UB2 > 1]
        ok <- Reduce(`&`, lapply(cf, function(z) is1(k[[z]]))) & (if (has(k, "UCF12")) is.na(k$UCF12) | k$UCF12 == 1 else TRUE) & (if (has(k, "UCF13")) is.na(k$UCF13) | k$UCF13 == 1 else TRUE)
        o$nodiff <- p100(ok); o$n24 <- nrow(k) }
      if (length(b8) && has(s, "CAGE")) { k <- s[!is.na(CAGE) & CAGE > 5 & CAGE < 24]
        if (length(b7)) o$noliq <- p100(allnot1(k, b7))
        o$nofood <- p100(allnot1(k, b8))
        if (has(k, "BD3")) o$zero <- p100(Reduce(`&`, lapply(b8, function(z) is1(k[[z]], 2))) & is1(k$BD3, 2))
        o$n623 <- nrow(k) }
      o
    })
    cl <- rbind(cols_dt(c("diarrhea", "cough", "fever"), c("Diarrhoea in the last 2 weeks", "A symptom of ARI in the last 2 weeks", "Fever in the last 2 weeks"), "pct", "Percentage of children with"),
                cols_dt(c("n", "nodisc", "n14", "nodiff", "n24"), c("Number of children under 5", "All 'No' to child discipline (%)", "Number of children age 1-4", "All 'No difficulty' to child functioning (%)", "Number of children age 2-4"), c("n", "pct", "n", "pct", "n")),
                cols_dt(c("noliq", "nofood", "zero", "n623"), c("All 'No' or 'DK' to BD7 liquids", "All 'No' or 'DK' to BD8 foods", "No food and no breastmilk", "Number of children age 6-23 months"), c("pct", "pct", "pct", "n"), "Children age 6-23 months"))
    pc <- intersect(c("diarrhea", "cough", "fever", "nodisc", "nodiff", "nofood"), names(d))
    fct_tab("F7U", d, cl, plots = list(list(kind = "bars", cols = pc, labels = stats::setNames(cl$label, cl$name)[pc], title = "Key answers (%)")),
            note = "High percentages of all 'No' answers can mean the questions were skipped or not asked properly.")
  },
  F7C = function(D, p) {
    x <- D$fs; if (!has(x, "FS17")) return(fct_na("F7C", "needs FS17"))
    x <- x[is1(FS17)]; disc <- grep("^FCD2[A-K]$", names(x), value = TRUE); cf <- intersect(c("FCF6", "FCF8", paste0("FCF", 14:24)), names(x))
    if (!length(disc) && !length(cf)) return(fct_na("F7C", "needs FCD2A-FCD2K or FCF6-FCF24"))
    d <- fct_rows(x, function(s) {
      o <- list()
      if (length(disc) && has(s, "CB3")) { k <- s[!is.na(CB3) & CB3 < 15]; o$nodisc <- p100(Reduce(`&`, lapply(disc, function(z) !is.na(k[[z]]) & k[[z]] != 1))); o$n514 <- nrow(k) }
      if (length(cf)) { o$nodiff <- p100(Reduce(`&`, lapply(cf, function(z) is1(s[[z]])))); o$n <- nrow(s) }
      o
    })
    pc <- intersect(c("nodisc", "nodiff"), names(d))
    fct_tab("F7C", d, cols_dt(c("nodisc", "n514", "nodiff", "n"), c("All 'No' to child discipline (%)", "Number of children age 5-14", "All 'No difficulty' to child functioning (%)", "Number of children age 5-17"), c("pct", "n", "pct", "n")),
            plots = list(list(kind = "bars", cols = pc, labels = c(nodisc = "All 'No' to child discipline", nodiff = "All 'No difficulty' to child functioning")[pc], title = "Key answers (%)")))
  },
  F8A = function(D, p) {
    x <- D$ch; if (!has(x, c("UF17", "BR1"))) return(fct_na("F8A", "needs BR1 (birth registration)"))
    x <- x[is1(UF17)]
    d <- fct_seen("F8A", x, function(s) is1(s$BR1, 1:2), function(s) is1(s$BR1), "Number of children",
                  extra = function(s) { cd <- is1(s$BR1, 1:2); rn <- !cd & (if (has(s, "BR2")) is1(s$BR2) else FALSE); list(regn = p100(rn), rega = p100(cd | rn)) }, p = p)
    fail <- !is.na(d$tnm)
    fct_tab("F8A", d, cols_dt(c("card", "cseen", "regn", "rega", "n", "pseen", "tnm"), c("Reported to have a birth certificate (1)", "Birth certificate seen (2)", "Registered, no birth certificate (3)",
                                                                                      "Birth certificate or registered (4)", "Number of children (5)", "Proportion of certificates seen (6) = (2)/(1)", "Target not met"), c("pct", "pct", "pct", "pct", "n", "pct", "pct")),
            bad = list(pseen = fail, tnm = fail), plots = list(list(kind = "bars", cols = "pseen", target = p$seen, title = "Birth certificates seen (% of those reported)")),
            note = sprintf("Target: at least %s%% of the birth certificates seen.", p$seen))
  },
  F8B = function(D, p) {
    x <- D$ch; if (!has(x, c("UF17", "UB2", "IM2"))) return(fct_na("F8B", "needs IM2 and IM5 (immunization)"))
    x <- x[is1(UF17) & lt(UB2, 3)]
    d <- fct_seen("F8B", x, function(s) is1(s$IM2, 1:3), function(s) if (has(s, "IM5")) is1(s$IM5, 1:3) else rep(FALSE, nrow(s)), "Number of children", p = p)
    fail <- !is.na(d$tnm)
    fct_tab("F8B", d, cols_dt(c("card", "cseen", "n", "pseen", "tnm"), c("Reported to have a vaccination card (1)", "Vaccination card seen (2)", "Number of children under 3 (3)", "Proportion of cards seen (4) = (2)/(1)", "Target not met"), c("pct", "pct", "n", "pct", "pct")),
            bad = list(pseen = fail, tnm = fail), plots = list(list(kind = "bars", cols = "pseen", target = p$seen, title = "Vaccination cards seen (% of those reported)")),
            note = sprintf("Target: at least %s%% of the cards seen.", p$seen))
  },
  F8C = function(D, p) {
    x <- D$wm; if (!has(x, c("WM17", "CM17", "MN7"))) return(fct_na("F8C", "needs CM17 and MN7"))
    x <- x[is1(WM17) & is1(CM17)]
    d <- fct_seen("F8C", x, function(s) is1(s$MN7, 1:2), function(s) is1(s$MN7), "Number of women", p = p)
    fail <- !is.na(d$tnm)
    fct_tab("F8C", d, cols_dt(c("card", "cseen", "n", "pseen", "tnm"), c("Have a health card (1)", "Health card seen (2)", "Women with a live birth in the last 2 years (3)", "Proportion of cards seen (4) = (2)/(1)", "Target not met"), c("pct", "pct", "n", "pct", "pct")),
            bad = list(pseen = fail, tnm = fail), plots = list(list(kind = "bars", cols = "pseen", target = p$seen, title = "Health cards seen (% of those reported)")),
            note = sprintf("Target: at least %s%% of the cards seen.", p$seen))
  },
  F8D = function(D, p) {
    x <- D$tn; if (!has(x, "TN3")) return(fct_na("F8D", "needs the bednet file (TN3)"))
    d <- fct_rows(x, function(s) list(obs = p100(is1(s$TN3)), n = nrow(s)))
    fail <- !is.na(d$obs) & d$obs < p$seen; target_col(d, "obs", fail, "tnm")
    fct_tab("F8D", d, cols_dt(c("obs", "n", "tnm"), c("Percent of bednets observed by interviewer", "Total number of bednets", "Target not met"), c("pct", "n", "pct")),
            bad = list(obs = fail, tnm = fail), plots = list(list(kind = "bars", cols = "obs", target = p$seen, title = "Bednets observed (%)")), note = sprintf("Target: at least %s%% observed.", p$seen))
  },
  F8E = function(D, p) {
    x <- D$hh; if (!has(x, "HW1")) return(fct_na("F8E", "needs HW1 (handwashing)"))
    x <- x[is1(HH46)]
    d <- fct_rows(x, function(s) list(h1 = p100(is1(s$HW1, 1:3)), h2 = p100(is1(s$HW1, 4)), h3 = p100(is1(s$HW1, 5)), h4 = p100(is1(s$HW1, 6)), n = nrow(s)))
    fail <- !is.na(d$h1) & d$h1 < p$seen; target_col(d, "h1", fail, "tnm")
    fct_tab("F8E", d, cols_dt(c("h1", "h2", "h3", "h4", "n", "tnm"), c("Place for handwashing observed", "Not in the dwelling, plot or yard", "No permission to see", "Other", "Number of households", "Target not met"), c("pct", "pct", "pct", "pct", "n", "pct")),
            bad = list(h1 = fail, tnm = fail), plots = list(list(kind = "stack", cols = c("h1", "h2", "h3", "h4")), list(kind = "bars", cols = "h1", target = p$seen, title = "Place for handwashing observed (%)")),
            note = sprintf("Target: at least %s%% observed.", p$seen))
  },
  F8Q = function(D, p) {
    x <- D$hh; if (!has(x, c("WQ31", "WQ12"))) return(fct_na("F8Q", "needs WQ12 and WQ31 (water quality testing)"))
    x <- x[ge(WQ31, 1)]
    d <- fct_rows(x, function(s) {
      ss <- rep(2, nrow(s)); if (has(s, "WQ19")) ss[is1(s$WQ19)] <- 1; if (has(s, "WQ18")) ss[!is.na(s$WQ18) & s$WQ18 != 1] <- 3
      bl <- if (has(s, "WQ20")) is1(s$WQ20) else rep(FALSE, nrow(s)); ec <- if (has(s, "WQ29")) !is.na(s$WQ29) & s$WQ29 > 0 else rep(FALSE, nrow(s))
      list(obs = p100(lt(s$WQ12, 4)), nobs = p100(!lt(s$WQ12, 4)), col = p100(ss == 1), ncol = p100(ss == 2), nsh = p100(ss == 3), n = nrow(s),
           bdet = sum(bl & ec), bnot = sum(bl & !ec), bn = sum(bl), bpct = if (any(bl)) 100 * sum(bl & ec) / sum(bl) else NA_real_)
    })
    fct_tab("F8Q", d, rbind(cols_dt(c("obs", "nobs"), c("Source observed", "Unable to observe source"), "pct", "Household sample"),
                            cols_dt(c("col", "ncol", "nsh"), c("Shown: collected", "Shown: not collected", "Not shown"), "pct", "Source sample"),
                            cols_dt("n", "Households selected for water quality testing", "n"),
                            cols_dt(c("bdet", "bnot", "bn", "bpct"), c("E. coli detected", "E. coli not detected", "Households selected for blank testing", "% of blank tests with E. coli"), c("n", "n", "n", "pct"), "Blank test")),   # households not selected for the blank test are not counted (#85)
            plots = list(list(kind = "stack", cols = c("col", "ncol", "nsh")), list(kind = "bars", cols = "bpct", title = "Blank tests with E. coli detected (%)")),
            note = "Blank tests should never detect E. coli: a team with E. coli in blank tests has a contamination problem in its testing.")
  },
  F9A = function(D, p) fct_measure("F9A", if (has(D$ch, "UF17")) D$ch[is1(UF17)], "AN8", "AN11", "AN9", "AN13", "UB1M", "UB1Y", c("HAZFLAG", "WAZFLAG", "WHZFLAG"), "0-4", p),
  F9B = function(D, p) fct_heap("F9B", if (has(D$ch, "UF17")) D$ch[is1(UF17)], "AN8", "AN11", "0-4", p),
  F9C = function(D, p) fct_measure("F9C", if (has(D$fs, "FS17")) D$fs[is1(FS17)], "FA8", "FA11", "FA9", "FA13", "CB2M", "CB2Y", character(), "5-9", p),
  F9D = function(D, p) fct_heap("F9D", if (has(D$fs, "FS17")) D$fs[is1(FS17)], "FA8", "FA11", "5-9", p),
  F10 = function(D, p) {
    x <- D$wm; if (!has(x, c("WM17", "CEB"))) return(fct_na("F10", "needs CEB, CSURV and CDEAD"))
    x <- x[is1(WM17)]; s0 <- function(v) sum(v, na.rm = TRUE)
    d <- fct_rows(x, function(s) { n <- nrow(s); list(n = n, liv = s0(s$CSURV), mliv = ratio(s0(s$CSURV), n), dead = s0(s$CDEAD), mdead = ratio(s0(s$CDEAD), n), ceb = s0(s$CEB), mceb = ratio(s0(s$CEB), n)) })
    tg <- p$ceb_prev * p$ceb_share / 100; fail <- !is.na(d$mceb) & d$mceb < tg; target_col(d, "mceb", fail, "tnm")
    fct_tab("F10", d, cols_dt(c("n", "liv", "mliv", "dead", "mdead", "ceb", "mceb", "tnm"), c("Women with a completed interview (1)", "Living children (2)", "Mean living children (3)=(2)/(1)", "Deceased children (4)", "Mean deceased children (5)=(4)/(1)",
                                                                                         "Children ever born (6)", "Mean children ever born (7)=(6)/(1)", "Target not met"), c("n", "n", "r2", "n", "r2", "n", "r2", "r2")),
            bad = list(mceb = fail, tnm = fail), plots = list(list(kind = "bars", cols = "mceb", target = tg, title = "Mean number of children ever born")),
            note = sprintf("Target: mean children ever born at least %s (%s%% of %s in the previous survey). Fewer births than expected can mean births are left out to shorten the birth history.", format(round(tg, 2)), p$ceb_share, p$ceb_prev))
  },
  F11A = function(D, p) {
    x <- D$bh; if (!has(x, "BH4Y")) return(fct_na("F11A", "needs BH4Y"))
    fy <- p$bh_first_year; if (is.null(fy) || is.na(fy)) fy <- D$svy_year - 13
    x <- x[ge(BH4Y, fy)]; yrs <- sort(unique(x$BH4Y))
    d <- fct_rows(x, function(s) c(stats::setNames(as.list(vapply(yrs, function(k) sum(s$BH4Y == k), 0)), paste0("y", yrs)), list(tot = nrow(s))))
    lab <- ifelse(yrs >= 9997, fct_codelab(D$meta$bh, "BH4Y", yrs), as.character(yrs))
    fct_tab("F11A", d, rbind(cols_dt(paste0("y", yrs), lab, "n", "Year of birth"), cols_dt("tot", "Total", "n")),
            plots = list(list(kind = "profile", cols = paste0("y", yrs[yrs < 9997]), labels = as.character(yrs[yrs < 9997]), title = "Births by year of birth - all teams")),
            note = "A drop in the births of one year (often 5 or 15 years before the survey) and a rise in the year before can mean births are moved out of the period with more questions.")
  },
  F11B = function(D, p) {
    x <- D$bh; if (!has(x, c("BH4M", "BH4Y", "BH5", "BH6"))) return(fct_na("F11B", "needs BH4M, BH4Y, BH5 and BH6"))
    x <- x[is1(BH5)]
    d <- fct_rows(x, function(s) { a <- lt(s$BH4M, 98) & lt(s$BH4Y, 9998); b <- ge(s$BH4M, 98) & lt(s$BH4Y, 9998) & lt(s$BH6, 98)
      c_ <- ge(s$BH4M, 98) & lt(s$BH4Y, 9998) & ge(s$BH6, 98); e <- ge(s$BH4M, 98) & ge(s$BH4Y, 9998) & lt(s$BH6, 98)
      list(myb = p100(a), yab = p100(b), yb = p100(c_), ab = p100(e), nd = p100(!(a | b | c_ | e)), tot = if (nrow(s)) 100 else NA_real_, n = nrow(s)) })
    fail <- !is.na(d$myb) & d$myb < p$dob; target_col(d, "myb", fail, "tnm")
    fct_tab("F11B", d, rbind(cols_dt(c("myb", "yab", "yb", "ab", "nd"), c("Year and month of birth", "Year of birth and age", "Year of birth only", "Age only", "Other/No data"), "pct", "Completeness of reporting"),
                             cols_dt(c("tot", "n", "tnm"), c("Total", "Number of living children", "Target not met"), c("pct", "n", "pct"))),
            bad = list(myb = fail, tnm = fail), plots = list(list(kind = "stack", cols = c("myb", "yab", "yb", "ab", "nd"))), note = sprintf("Target: at least %s%% with year and month of birth.", p$dob))
  },
  F11C = function(D, p) {
    x <- D$bh; if (!has(x, c("BH4M", "BH4Y", "BH5", "BH6"))) return(fct_na("F11C", "needs BH4M, BH4Y, BH5 and BH6"))
    x <- x[is1(BH5, 2)]
    d <- fct_rows(x, function(s) { a <- lt(s$BH4M, 98) & lt(s$BH4Y, 9998); b <- ge(s$BH4M, 98) & lt(s$BH4Y, 9998); e <- lt(s$BH4M, 98) & ge(s$BH4Y, 9998) & ge(s$BH6, 98)
      list(myb = p100(a), yb = p100(b), mb = p100(e), nd = p100(!(a | b | e)), tot = if (nrow(s)) 100 else NA_real_, n = nrow(s)) })
    fail <- !is.na(d$myb) & d$myb < p$dob; target_col(d, "myb", fail, "tnm")
    fct_tab("F11C", d, rbind(cols_dt(c("myb", "yb", "mb", "nd"), c("Year and month of birth", "Year of birth only", "Month of birth only", "No data"), "pct", "Completeness of reporting"),
                             cols_dt(c("tot", "n", "tnm"), c("Total", "Number of deceased children", "Target not met"), c("pct", "n", "pct"))),
            bad = list(myb = fail, tnm = fail), plots = list(list(kind = "stack", cols = c("myb", "yb", "mb", "nd"))), note = sprintf("Target: at least %s%% with year and month of birth.", p$dob))
  },
  F11D = function(D, p) {
    x <- fct_bh15(D); if (is.null(x) || !has(x, c("BH9U", "BH9N"))) return(fct_na("F11D", "needs BH9U and BH9N (age at death) and WM6Y"))
    x <- x[is1(BH5, 2)]; x[, .am := fct_age_death(BH9U, BH9N)]
    x[, .c := data.table::fifelse(.am %in% 8:16, paste0("m", .am), NA_character_)]; x[is1(BH9U, 3) & is1(BH9N, 1), .c := "y1"]
    x <- x[!is.na(.c)]; cats <- c(paste0("m", 8:12), "y1", paste0("m", 13:16))
    d <- fct_rows(x, function(s) { cn <- vapply(cats, function(k) sum(s$.c == k), 0); tot <- sum(cn)
      c(stats::setNames(as.list(cn), cats), list(tot = tot, r12 = if (tot) (cn[["m12"]] + cn[["y1"]]) * 9 / tot else NA_real_)) })
    fail <- !is.na(d$r12) & d$r12 > p$death12; target_col(d, "r12", fail, "tnm")
    fct_tab("F11D", d, rbind(cols_dt(cats, c(paste(8:11, "m."), "12 months", "Reported as '1 year'", paste(13:16, "m.")), "n", "Age at death (in months)"),
                             cols_dt(c("tot", "r12", "tnm"), c("Total 8-16 months (including '1 year')", "12 months ratio (including '1 year')", "Target not met"), c("n", "r2", "r2"))),
            bad = list(r12 = fail, tnm = fail), plots = list(list(kind = "profile", cols = cats, labels = c(8:11, "12", "1 yr", 13:16), mark = c("m12", "y1"), title = "Deaths by age at death (months) - all teams"),
                                                             list(kind = "bars", cols = "r12", target = p$death12, ref = 1, title = "12 months ratio")),
            note = sprintf("12 months ratio = deaths at 12 months or '1 year' x 9 / deaths at 8-16 months; 1 = no heaping. Target: at most %s. Deaths in the last 15 years.", p$death12))
  },
  F11E = function(D, p) {
    x <- fct_bh15(D); if (is.null(x) || !has(x, c("BH5", "BH9U", "BH9N"))) return(fct_na("F11E", "needs BH5, BH9U and BH9N and WM6Y"))
    x[, .am := data.table::fifelse(is1(BH5, 2), fct_age_death(BH9U, BH9N), NA_real_)]
    d <- fct_rows(x, function(s) { dd <- is1(s$BH5, 2); a <- s$.am
      neo <- sum(dd & a %in% c(0, 98)); inf <- sum(dd & a %in% 1:11); old <- sum(dd & a %in% 12:97); mis <- sum(dd & a %in% 99)
      list(neo = neo, inf = inf, old = old, mis = mis, dead = sum(dd), alive = sum(is1(s$BH5)), tot = nrow(s), rni = ratio(neo, neo + inf), ipt = if (nrow(s)) 1000 * (neo + inf) / nrow(s) else NA_real_) })
    fct_tab("F11E", d, rbind(cols_dt(c("neo", "inf", "old", "mis", "dead"), c("< 1 month (1)", "1-11 months (2)", "12+ months (3)", "Missing (4)", "Total dead (5)"), "n", "Age at death of children who died"),
                             cols_dt(c("alive", "tot", "rni", "ipt"), c("Still alive (6)", "Total births (7)", "Ratio of neonatal to infant (1)/(1+2)", "Infant deaths per 1,000 births (1+2)/(7)"), c("n", "n", "r2", "r1"))),
            plots = list(list(kind = "bars", cols = c("rni", "ipt"), labels = c("Neonatal / infant deaths", "Infant deaths per 1,000 births"), title = "Child mortality ratios", free = TRUE)),
            note = "Births in the last 15 years. A low neonatal / infant ratio, or few infant deaths, can mean deaths are left out or ages at death are wrong.")
  },
  F12 = function(D, p) {
    x <- D$mm; if (!has(x, c("MM15", "MM16", "MM19"))) return(fct_na("F12", "needs the maternal mortality file (MM15, MM16, MM19)"))
    x <- x[is1(MM15, 2) & is1(MM16, 2)]
    d <- fct_rows(x, function(s) { k <- s[!is.na(MM19) & MM19 >= 12 & MM19 < 97]
      m2 <- Reduce(`|`, lapply(intersect(c("MM22", "MM23", "MM24"), names(k)), function(z) ge(k[[z]], 8)), rep(FALSE, nrow(k)))
      m3 <- if (has(k, "MM18")) ge(k$MM18, 97) else rep(FALSE, nrow(k))
      list(n1 = nrow(s), c1 = sum(ge(s$MM19, 97)), p1 = p100(ge(s$MM19, 97)), n2 = nrow(k), c2 = sum(m2), p2 = p100(m2), c3 = sum(m3), p3 = p100(m3)) })
    f1 <- !is.na(d$p1) & d$p1 >= p$mm_missing; f2 <- !is.na(d$p2) & d$p2 > 0; f3 <- !is.na(d$p3) & d$p3 > 0
    target_col(d, "p1", f1, "t1"); target_col(d, "p2", f2, "t2"); target_col(d, "p3", f3, "t3")
    data.table::setcolorder(d, c(".row", ".key", "n1", "c1", "p1", "t1", "n2", "c2", "p2", "t2", "c3", "p3", "t3"))
    fct_tab("F12", d, rbind(cols_dt(c("n1", "c1", "p1", "t1"), c("Number", "Missing age at death", "Missing age at death (%)", "Target not met"), c("n", "n", "pct", "pct"), "All deceased sisters"),
                            cols_dt(c("n2", "c2", "p2", "t2", "c3", "p3", "t3"), c("Number", "Missing: death during pregnancy, delivery or after", "%", "Target not met ", "Missing: years ago of death", "% ", "Target not met  "),
                                    c("n", "n", "pct", "pct", "n", "pct", "pct"), "Sisters who died at age 12+")),
            bad = list(p1 = f1, t1 = f1, p2 = f2, t2 = f2, p3 = f3, t3 = f3),
            plots = list(list(kind = "bars", cols = c("p1", "p2", "p3"), labels = c("Missing age at death", "Missing pregnancy-related", "Missing years ago"), target = c(p$mm_missing, 0, 0), title = "Missing information on deceased sisters (%)")),
            note = sprintf("Target: less than %s%% missing age at death, and no missing information on deaths during pregnancy or delivery and on their timing.", p$mm_missing))
  },
  F13AW = function(D, p) fct_priv("F13AW", D$wm, "WM17", "WB4", "MH1", "MH40", "women"),
  F13AM = function(D, p) fct_priv("F13AM", D$mn, "MWM17", "MWB4", "MMH1", "MMH40", "men"),
  F13B = function(D, p) {
    x <- D$wm; if (!has(x, c("WM17", "VW1"))) return(fct_na("F13B", "needs VW1 (violence against women)"))
    d <- fct_rows(x[is1(WM17) & !is.na(VW1)], function(s) list(priv = p100(is1(s$VW1, 2)), n = nrow(s)))
    fct_tab("F13B", d, cols_dt(c("priv", "n"), c("Module not administered: no privacy (%)", "Women selected for the module"), c("pct", "n")), plots = list(list(kind = "bars", cols = "priv", title = "No privacy for the violence module (%)")))
  },
  F14 = function(D, p) {
    x <- D$hh; fe <- paste0("FE", 1:8); if (!has(x, fe)) return(fct_na("F14", "needs FE1-FE8 (food insecurity experience)"))
    x <- x[is1(HH46)]
    d <- fct_rows(x, function(s) list(m9 = p100(Reduce(`&`, lapply(fe, function(z) is1(s[[z]], 9)))), m2 = p100(Reduce(`&`, lapply(fe, function(z) is1(s[[z]], 2)))), n = nrow(s)))
    fct_tab("F14", d, cols_dt(c("m9", "m2", "n"), c("'Missing' to all FIES questions", "'No' to all FIES questions", "Number of households"), c("pct", "pct", "n"), c("Percentage with", "Percentage with", "")),
            plots = list(list(kind = "bars", cols = c("m9", "m2"), labels = c("All missing", "All 'No'"), title = "Food insecurity questions (%)")))
  },
  F20 = function(D, p) {
    x <- D$hh; if (!has(x, c("HC7A", "HC12"))) return(fct_na("F20", "needs HC7A and HC12 (and the MICS Plus consent questions)"))
    x <- x[is1(HH46)]
    anyc <- function(r, res, v) { z <- D$all[[r]]; if (!has(z, c(res, v))) return(NULL); unique(z[is1(get(res)) & is1(get(v)), .(HH1, HH2)]) }
    hk <- paste(x$HH1, x$HH2)
    mark <- function(hhv, mv) { o <- if (has(x, hhv)) is1(x[[hhv]]) else rep(FALSE, nrow(x))
      for (m in mv) { k <- anyc(m[1], m[2], m[3]); if (!is.null(k)) o <- o | hk %in% paste(k$HH1, k$HH2) }; o }
    x[, .cons := mark("HH44BC", list(c("wm", "WM17", "WM15EF"), c("mn", "MWM17", "MWM15EF"), c("ch", "UF17", "UF15EF"), c("fs", "FS17", "FS15EF")))]
    x[, .phone := mark("HH44DE", list(c("wm", "WM17", "WM15GH"), c("mn", "MWM17", "MWM15GH"), c("ch", "UF17", "UF15GH"), c("fs", "FS17", "FS15GH")))]
    plus <- has(x, "HH44BC") || has(D$wm, "WM15EF")
    d <- fct_rows(x, function(s) c(list(fixed = p100(is1(s$HC7A)), mobile = p100(is1(s$HC12)), either = p100(is1(s$HC7A) | is1(s$HC12))),
                                   if (plus) list(cons = p100(s$.cons), phone = p100(s$.phone)), list(n = nrow(s))))
    fct_tab("F20", d, rbind(cols_dt(c("fixed", "mobile", "either"), c("A fixed telephone line", "Any member with a mobile telephone", "Fixed line or mobile telephone"), "pct", "Percentage with"),
                            cols_dt(c("cons", "phone"), c("At least one consent for MICS Plus", "At least one phone number for MICS Plus"), "pct", "Percentage with"), cols_dt("n", "Number of completed households", "n")),
            plots = list(list(kind = "bars", cols = intersect(c("either", "cons", "phone"), names(d)), labels = c(either = "Any telephone", cons = "MICS Plus consent", phone = "Phone number given")[intersect(c("either", "cons", "phone"), names(d))], title = "Telephones and MICS Plus (%)")))
  }
)

# eligible members per household by urban / rural (F2W, F2M, F2UC)
fct_elig <- function(id, D, items, p) {
  x <- D$hh; v <- vapply(items, `[[`, "", "v")
  if (!has(x, c("HH46", v))) return(fct_na(id, paste("needs", paste(v, collapse = ", "))))
  x <- x[is1(HH46)]
  if (all(is.na(x$.urb))) return(fct_na(id, "needs HH6 (area) with urban and rural codes (set them in the parameters)"))
  d <- fct_rows(x, function(s) {
    o <- list()
    for (a in c("u", "r")) { k <- s[.urb == a]
      o[[paste0(a, "_hh")]] <- nrow(k)
      for (j in seq_along(items)) { it <- items[[j]]; nsel <- if (!is.null(it$sel) && has(k, it$sel)) sum(is1(k[[it$sel]])) else nrow(k)
        if (!is.null(it$sel)) o[[paste0(a, "_sel")]] <- nsel
        tot <- sum(k[[it$v]], na.rm = TRUE)
        o[[paste0(a, j, "_n")]] <- tot; o[[paste0(a, j, "_m")]] <- ratio(tot, nsel)
        if (isTRUE(it$any)) o[[paste0(a, j, "_any")]] <- p100(!is.na(k[[it$v]]) & k[[it$v]] > 0) } }
    o })
  bad <- list(); cl <- list()
  for (a in c("u", "r")) {
    g <- c(u = "Urban", r = "Rural")[[a]]
    cl[[length(cl) + 1]] <- cols_dt(paste0(a, "_hh"), "Number of completed households", "n", g)
    if (!is.null(items[[1]]$sel)) cl[[length(cl) + 1]] <- cols_dt(paste0(a, "_sel"), "Completed households selected for men", "n", g)
    for (j in seq_along(items)) { it <- items[[j]]; tg <- (if (a == "u") it$tu else it$tr) * p$elig_share / 100
      m <- paste0(a, j, "_m"); fail <- !is.na(d[[m]]) & d[[m]] < tg; tn <- paste0(a, j, "_t"); target_col(d, m, fail, tn)
      bad[[m]] <- fail; bad[[tn]] <- fail
      cl[[length(cl) + 1]] <- cols_dt(c(paste0(a, j, "_n"), m, tn, if (isTRUE(it$any)) paste0(a, j, "_any")),
                                      c(paste("Number of", it$lab), paste("Mean number of", it$lab, "per household"), "Target not met", if (isTRUE(it$any)) paste("% of households with", it$lab)),
                                      c("n", "r2", "r2", if (isTRUE(it$any)) "pct"), g) } }
  cols <- data.table::rbindlist(cl); data.table::setcolorder(d, c(".row", ".key", intersect(cols$name, names(d))))
  ms <- unlist(lapply(seq_along(items), function(j) paste0(c("u", "r"), j, "_m")))
  labs <- unlist(lapply(items, function(it) paste(c("Urban:", "Rural:"), it$lab)))
  tgs <- unlist(lapply(items, function(it) c(it$tu, it$tr) * p$elig_share / 100))
  fct_tab(id, d, cols, bad = bad, plots = list(list(kind = "bars", cols = ms, labels = labs, target = tgs, title = "Mean per household (line: target)")),
          note = sprintf("Target not met when the mean is below %s%% of the expected number (%s). Too few eligible members can mean members are left out of the household list.", p$elig_share,
                         paste(vapply(items, function(it) sprintf("%s: %s urban, %s rural", it$lab, it$tu, it$tr), ""), collapse = "; ")))
}

# response rate of an individual questionnaire (F3W, F3M, F3U, F3C)
fct_resp <- function(id, D, r, res, n_label, group, age = c("own", "UF4", "all"), n15 = NULL) {
  age <- match.arg(age); x <- D[[r]]
  if (!has(x, res)) return(fct_na(id, paste("needs", res)))
  codes <- sort(unique(stats::na.omit(x[[res]])))
  d <- fct_dist(x, res, codes)
  if (age == "all") d[, cons := fct_rows(x, function(s) list(v = p100(is1(s[[res]], 6))))$v]
  else {
    ln <- if (age == "own") D$line[[r]] else if (has(x, "UF4")) "UF4"
    if (!is.null(ln) && !is.null(D$hla)) {
      y <- merge(x[, c("HH1", "HH2", ln, ".g", ".lab", res), with = FALSE], D$hla, by.x = c("HH1", "HH2", ln), by.y = c("HH1", "HH2", "HL1"), all.x = TRUE, sort = FALSE)
      y <- y[!is.na(HL6) & HL6 >= 15 & HL6 <= 17]
      k <- fct_rows(y, function(s) list(cons = p100(is1(s[[res]], 6)), n15 = nrow(s)))
      i <- match(d$.row, k$.row)
      d[, `:=`(cons = k$cons[i], n15 = data.table::fifelse(is.na(i), 0L, as.integer(k$n15[i])))]
    }
  }
  fct_tab(id, d, rbind(cols_dt(paste0("c", codes), fct_codelab(D$meta[[r]], res, codes), "pct", group),
                       cols_dt(c("tot", "n", "cons", "n15"), c("Total", n_label, "Percentage with no adult consent", n15 %||% ""), c("pct", "n", "pct", "n"))),
          plots = list(list(kind = "stack", cols = paste0("c", codes))),
          note = if (age != "all") "No adult consent: among the respondents age 15-17 (age from the household list), result code 6." else "No adult consent: result code 6.")
}

# age groups listed and interviewed (F4W, F4M)
fct_agedist <- function(id, D, sex, r, res, who, p) {
  h <- D$hl; x <- D$all[[r]]
  if (!has(h, c("HL4", "HL6")) || !has(x, res) || is.null(D$line[[r]])) return(fct_na(id, sprintf("needs the household list and the %s questionnaire", who)))
  h <- h[is1(HL4, sex) & !is.na(HL6) & HL6 >= 15 & HL6 <= 49]
  if (sex == 1 && has(h, "HL9")) h <- h[!is.na(HL9)]
  k <- unique(x[, c("HH1", "HH2", D$line[[r]], res), with = FALSE], by = c("HH1", "HH2", D$line[[r]]))
  data.table::setnames(k, c("HH1", "HH2", "HL1", ".res"))
  for (z in c("HH1", "HH2", "HL1")) { h[, (z) := as.numeric(get(z))]; k[, (z) := as.numeric(get(z))] }
  h <- merge(h, k, by = c("HH1", "HH2", "HL1"), all.x = TRUE, sort = FALSE)
  lo <- seq(15, 45, 5)
  d <- fct_rows(h, function(s) { o <- list()
    for (a in lo) { k <- s[HL6 >= a & HL6 <= a + 4]; o[[paste0("n", a)]] <- nrow(k); o[[paste0("r", a)]] <- p100(is1(k$.res)) }
    o$n <- nrow(s); o$r <- p100(is1(s$.res)); o$min <- suppressWarnings(min(unlist(o[paste0("r", lo)]), na.rm = TRUE)); if (!is.finite(o$min)) o$min <- NA_real_
    o })
  fail <- !is.na(d$min) & d$min <= p$age_int; target_col(d, "min", fail, "tnm")
  rc <- paste0("r", lo); bad <- list(min = fail, tnm = fail)
  for (z in rc) bad[[z]] <- !is.na(d[[z]]) & d[[z]] <= p$age_int
  ag <- sprintf("%d-%d", lo, lo + 4)
  cols <- data.table::rbindlist(lapply(seq_along(lo), function(i) cols_dt(c(paste0("n", lo[i]), rc[i]), c(paste("No of", who), "% interviewed"), c("n", "pct"), ag[i])))
  fct_tab(id, d, rbind(cols, cols_dt(c("n", "r"), c(paste("No of", who), "% interviewed"), c("n", "pct"), "Total"),
                       cols_dt(c("min", "tnm"), c("Minimum % interviewed in any age group", "Target not met"), "pct")),
          bad = bad, plots = list(list(kind = "heat", cols = rc, labels = ag, target = p$age_int, title = sprintf("%% of %s interviewed by age group", who))),
          note = sprintf("Target: more than %s%% interviewed in every age group. A low rate in the youngest or oldest group can mean eligible %s are given an age out of range.", p$age_int, who))
}

# privacy in the mental health module (F13AW, F13AM)
fct_priv <- function(id, x, res, age, v1, v2, who) {
  if (!has(x, c(res, age, v1))) return(fct_na(id, sprintf("needs %s (mental health module)", v1)))
  x <- x[is1(get(res)) & !is.na(get(age)) & get(age) >= 15 & get(age) <= 24]
  d <- fct_rows(x, function(s) list(p1 = p100(is1(s[[v1]], 2)), p2 = if (has(s, v2)) p100(is1(s[[v2]], 2)), n = nrow(s)))
  fct_tab(id, d, cols_dt(c("p1", "p2", "n"), c(sprintf("Mental health module not administered: no privacy (%s=2)", v1), sprintf("Suicide questions not administered: no privacy (%s=2)", v2), sprintf("Number of %s age 15-24", who)), c("pct", "pct", "n")),
          plots = list(list(kind = "bars", cols = intersect(c("p1", "p2"), names(d)), labels = c(p1 = "Module", p2 = "Suicide questions")[intersect(c("p1", "p2"), names(d))], title = "Not administered for lack of privacy (%)")))
}

# age at death in months from the unit and number (BH9U, BH9N)
fct_age_death <- function(u, n) {
  a <- rep(0, length(u))
  a[is1(u, 1)] <- trunc(n[is1(u, 1)] * 12 / 365); a[is1(u, 2)] <- n[is1(u, 2)]; a[is1(u, 3)] <- n[is1(u, 3)] * 12
  a[is1(u, 9) | is1(n, 99)] <- 99; a[is.na(a)] <- 0
  a
}

# births in the last 15 years before the interview of the mother
fct_bh15 <- function(D) {
  x <- D$bh; w <- D$wmx
  if (!has(x, c("BH4Y", "BH5")) || !has(w, "WM6Y") || is.null(D$bline) || is.null(D$wline)) return(NULL)
  k <- unique(w[, c("HH1", "HH2", D$wline, "WM6Y"), with = FALSE], by = c("HH1", "HH2", D$wline)); data.table::setnames(k, D$wline, D$bline)
  x <- data.table::copy(x); if ("WM6Y" %in% names(x)) x[, WM6Y := NULL]
  for (z in c("HH1", "HH2", D$bline)) { k[, (z) := as.numeric(get(z))]; x[, (z) := as.numeric(get(z))] }
  x <- merge(x, k, by = c("HH1", "HH2", D$bline), all.x = TRUE, sort = FALSE)
  x[ge(BH4Y, 9997) | (!is.na(BH4Y) & !is.na(WM6Y) & BH4Y >= WM6Y - 15)]
}

# urban / rural codes of HH6: from the parameters, or the value labels
fct_area <- function(hh, meta, p) {
  cd <- function(s) suppressWarnings(as.numeric(unlist(strsplit(trimws(s %||% ""), "[ ,;]+"))))
  u <- cd(p$urban); r <- cd(p$rural); u <- u[!is.na(u)]; r <- r[!is.na(r)]
  if (!length(u) || !length(r)) {
    vl <- meta$vallab$HH6
    if (!is.null(vl)) { l <- tolower(names(vl)); u <- unname(vl[grepl("urba", l)]); r <- unname(vl[grepl("rural", l)]) }
    if (!length(u) || !length(r)) { u <- 1; r <- 2 }
  }
  list(urban = u, rural = r)
}

#' Build the Field Check Tables
#'
#' @param fd data from [ipm_fct_data()]
#' @param params targets, see [ipm_fct_defaults()] (missing ones take the default)
#' @param rows "team" or "interv" (one row per interviewer)
#' @param from,to first and last day of interview (NULL = whole fieldwork)
#' @param teams teams to keep (NULL = all)
#' @param tables ids of the tables (NULL = all, see [ipm_fct_catalog()])
#' @return list(tables, summary, overview, params, rows, filters, area)
#' @export
ipm_fct <- function(fd, params = list(), rows = c("team", "interv"), from = NULL, to = NULL, teams = NULL, tables = NULL) {
  rows <- match.arg(rows)
  p <- utils::modifyList(ipm_fct_defaults(), params %||% list())
  for (k in names(p)) if (is.list(p[[k]])) p[[k]] <- unlist(p[[k]])
  ti <- fd$team_info
  prep <- function(x) {
    if (is.null(x)) return(NULL)
    x <- data.table::copy(x)
    if (!is.null(from)) x <- x[!is.na(doi) & doi >= data.table::as.IDate(from)]
    if (!is.null(to)) x <- x[!is.na(doi) & doi <= data.table::as.IDate(to)]
    if (length(teams)) x <- x[team %in% as.integer(teams)]
    if (rows == "interv") { x[, .g := interv]; x[, .lab := interv_label] }
    else { x[, .g := team]; x[, .lab := ti$team_label[match(team, ti$team)]]; x[is.na(.lab), .lab := ifelse(is.na(team), "(no team)", paste0("T", team))] }
    x
  }
  D <- lapply(fd$ds, prep); D$meta <- fd$meta; D$all <- fd$ds
  line <- function(x, v) { v <- intersect(v, names(x)); if (length(v)) v[1] }
  D$line <- list(wm = line(fd$ds$wm, c("LN", "WM3")), mn = line(fd$ds$mn, c("LN", "MWM3")), ch = line(fd$ds$ch, c("LN", "UF3")), fs = line(fd$ds$fs, c("LN", "FS3")))
  D$wline <- D$line$wm; D$bline <- line(fd$ds$bh, c("LN", "WM3")); D$bhx <- fd$ds$bh; D$wmx <- fd$ds$wm
  if (has(fd$ds$hl, c("HL1", "HL6"))) { D$hla <- unique(fd$ds$hl[, .(HH1 = as.numeric(HH1), HH2 = as.numeric(HH2), HL1 = as.numeric(HL1), HL6)], by = c("HH1", "HH2", "HL1")) }
  for (r in c("wm", "mn", "ch", "fs")) for (nm in c("x", "all")) {
    z <- if (nm == "x") D[[r]] else D$all[[r]]; if (is.null(z)) next
    ln <- c(D$line[[r]], if (r == "ch" && has(z, "UF4")) "UF4"); for (v in c("HH1", "HH2", ln)) if (!is.numeric(z[[v]])) z[, (v) := as.numeric(get(v))]
  }
  ar <- if (!is.null(D$hh)) fct_area(D$hh, fd$meta$hh, p)
  if (has(D$hh, "HH6")) D$hh[, .urb := data.table::fifelse(HH6 %in% ar$urban, "u", data.table::fifelse(HH6 %in% ar$rural, "r", NA_character_))]
  else if (!is.null(D$hh)) D$hh[, .urb := NA_character_]
  D$svy_year <- { y <- if (has(fd$ds$hh, "doi")) as.integer(format(max(fd$ds$hh$doi, na.rm = TRUE), "%Y")) else NA_integer_; if (is.na(y)) as.integer(format(Sys.Date(), "%Y")) else y }
  ids <- tables %||% names(fct_def); ids <- intersect(names(fct_def), ids)
  out <- lapply(ids, function(i) tryCatch(fct_build[[i]](D, p), error = function(e) fct_na(i, paste("could not be computed:", conditionMessage(e)))))
  names(out) <- ids
  list(tables = out, summary = fct_summary(out), overview = tryCatch(fct_overview(D, p), error = function(e) NULL), params = p, rows = rows,
       filters = list(from = from, to = to, teams = teams), area = ar,
       survey_label = if (nzchar(p$survey_label %||% "")) p$survey_label else fd$cfg$survey_name %||% "")
}

# per table and row (team): is a target not met?
fct_summary <- function(tabs) {
  x <- data.table::rbindlist(lapply(tabs, function(t) {
    if (!isTRUE(t$available) || !length(t$bad)) return(NULL)
    b <- Reduce(`|`, t$bad)
    data.table(id = t$id, title = t$title, row = t$data$.row, rkey = t$data$.key, not_met = b)
  }))
  x
}

# performance overview: first and second half of the interviews of each team (by date), as in the MICS7 Performance syntax
fct_overview <- function(D, p) {
  half <- function(x) { if (is.null(x) || !nrow(x)) return(x); x <- data.table::copy(x); data.table::setorderv(x, "doi", na.last = TRUE)
    x[, .h := { n <- .N; if (n) ifelse(seq_len(n) <= ceiling(n / 2), 1L, 2L) else integer() }, by = .g]; x[, .ht := ifelse(seq_len(.N) <= ceiling(.N / 2), 1L, 2L)]; x }
  res <- list()
  add <- function(ind, x, f) { if (is.null(x) || !nrow(x)) return()
    tot <- vapply(1:2, function(h) f(x[.ht == h]), 0)
    k <- sort(unique(x$.g), na.last = TRUE); lab <- x$.lab[match(k, x$.g)]
    v <- t(vapply(k, function(g) { s <- x[if (is.na(g)) is.na(.g) else (!is.na(.g) & .g == g)]; vapply(1:2, function(h) f(s[.h == h]), 0) }, numeric(2)))
    res[[length(res) + 1]] <<- data.table(indicator = ind, row = c("Total", lab), rkey = c(NA, k), first = c(tot[1], v[, 1]), second = c(tot[2], v[, 2])) }
  hh <- half(if (has(D$hh, "HH46")) D$hh[is1(HH46)])
  if (!is.null(hh) && !is.null(D$hla)) {
    sz <- D$hla[, .(.size = .N), by = .(HH1, HH2)]
    hh[, `:=`(HH1n = as.numeric(HH1), HH2n = as.numeric(HH2))]; hh <- merge(hh, sz, by.x = c("HH1n", "HH2n"), by.y = c("HH1", "HH2"), all.x = TRUE, sort = FALSE)
    for (a in c("u", "r")) add(sprintf("Mean number of household members (%s)", c(u = "urban", r = "rural")[[a]]), hh[.urb %in% a], function(s) mean(s$.size, na.rm = TRUE))
  }
  if (has(D$wm, c("WM17", "CEB"))) add("Children ever born - mean", half(D$wm[is1(WM17)]), function(s) mean(s$CEB, na.rm = TRUE))
  if (has(D$ch, c("UF17", "HAZFLAG", "WAZFLAG"))) add("Anthropometry - % height/weight out of range", half(D$ch[is1(UF17) & lt(AN8, 99) & lt(AN11, 999)]),
                                                      function(s) p100((!is.na(s$HAZFLAG) & s$HAZFLAG != 0) | (!is.na(s$WAZFLAG) & s$WAZFLAG != 0)))
  if (has(D$hh, c("WQ20", "WQ29"))) add("Blank testing - % with E. coli", half(D$hh[is1(WQ20)]), function(s) p100(!is.na(s$WQ29) & s$WQ29 > 0))
  if (has(D$hl, c("HL4", "HL6"))) for (sx in 2:1) for (r in list(c(2, 1), c(5, 4), c(6, 7), c(14, 15), c(25, 24), c(50, 49)))
    add(sprintf("%s age ratio %d/%d", c("Male", "Female")[sx], r[1], r[2]), half(D$hl[is1(HL4, sx)]), function(s) ratio(sum(s$HL6 == r[1], na.rm = TRUE), sum(s$HL6 == r[2], na.rm = TRUE)))
  if (!length(res)) return(NULL)
  o <- data.table::rbindlist(res); o[is.nan(first), first := NA]; o[is.nan(second), second := NA]; o
}

# ---------- display ----------
fct_fmt <- function(v, f) {
  d <- switch(f, n = 0, pct = 1, r1 = 1, r2 = 2, 1)
  ifelse(is.na(v), "", formatC(v, format = "f", digits = d, big.mark = if (f == "n") "," else ""))
}

#' Graph of a Field Check Table
#' @param tab one table of [ipm_fct()]`$tables`
#' @param i which graph of the table (most have one or two)
#' @return ggplot or NULL
#' @export
ipm_fct_plot <- function(tab, i = 1) {
  if (!isTRUE(tab$available) || length(tab$plots) < i) return(NULL)
  sp <- tab$plots[[i]]; d <- tab$data; cols <- intersect(sp$cols, names(d)); if (!length(cols)) return(NULL)
  lab_of <- function(n) { l <- sp$labels; if (!is.null(l) && !is.null(names(l)) && all(n %in% names(l))) unname(l[n]) else if (!is.null(l) && length(l) == length(sp$cols)) l[match(n, sp$cols)] else tab$cols$label[match(n, tab$cols$name)] }
  rows <- factor(d$.row, levels = rev(unique(d$.row)))
  th <- ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 11, face = "bold"),
                                                                legend.position = "bottom", axis.title = ggplot2::element_blank())
  pal <- c("#1CABE2", "#80BD41", "#FFC20E", "#F26A21", "#E2231A", "#961A49", "#6A1E74", "#374EA2", "#00833D", "#777779", "#B6B7B8", "#D8D1C9", "#5C3D2E", "#0058AB")
  if (sp$kind == "stack") {
    l <- data.table::rbindlist(lapply(cols, function(cn) data.table(row = rows, cat = lab_of(cn), v = d[[cn]])))
    l[, cat := factor(cat, levels = lab_of(cols))]
    return(ggplot2::ggplot(l[!is.na(v)], ggplot2::aes(x = v, y = row, fill = cat)) + ggplot2::geom_col(width = 0.75, position = ggplot2::position_stack(reverse = TRUE)) +
             ggplot2::scale_fill_manual(values = rep(pal, 3)[seq_along(cols)], name = NULL, labels = function(z) vapply(z, function(s) paste(strwrap(s, 38), collapse = "\n"), "")) +
             ggplot2::scale_x_continuous(labels = function(z) paste0(z, "%"), expand = c(0, 0.5)) +
             ggplot2::labs(title = sp$title %||% tab$cols$group[match(cols[1], tab$cols$name)]) + th + ggplot2::guides(fill = ggplot2::guide_legend(ncol = 3)))
  }
  if (sp$kind == "bars") {
    tg <- sp$target; if (!is.null(tg)) tg <- rep(tg, length.out = length(sp$cols))[match(cols, sp$cols)]
    l <- data.table::rbindlist(lapply(seq_along(cols), function(j) { cn <- cols[j]
      data.table(row = rows, ind = lab_of(cn), v = d[[cn]], bad = if (!is.null(tab$bad[[cn]])) tab$bad[[cn]] else FALSE, tot = d$.row == "Total", tg = if (!is.null(tg)) tg[j] else NA_real_) }))
    l[, ind := factor(ind, levels = unique(lab_of(cols)))]
    l[, st := ifelse(bad, "Target not met", ifelse(tot, "Total", "Team"))]
    g <- ggplot2::ggplot(l[!is.na(v)], ggplot2::aes(x = v, y = row, fill = st)) + ggplot2::geom_col(width = 0.7) +
      ggplot2::geom_text(ggplot2::aes(label = formatC(v, format = "f", digits = if (max(abs(l$v), na.rm = TRUE) < 10) 2 else 1)), hjust = -0.15, size = 3) +
      ggplot2::scale_fill_manual(values = c(Team = "#1CABE2", Total = "#374EA2", `Target not met` = "#E2231A"), name = NULL, drop = TRUE) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) + ggplot2::labs(title = sp$title) + th
    if (!is.null(tg)) g <- g + ggplot2::geom_vline(data = unique(l[!is.na(tg), .(ind, tg)]), ggplot2::aes(xintercept = tg), linetype = "dashed", colour = "#E2231A")
    if (!is.null(sp$ref)) g <- g + ggplot2::geom_vline(xintercept = sp$ref, colour = "grey40", linewidth = 0.3)
    if (length(cols) > 1) g <- g + ggplot2::facet_wrap(~ind, nrow = 1, scales = if (isTRUE(sp$free) || is.null(sp$target) && !all(tab$cols$fmt[match(cols, tab$cols$name)] == "pct")) "free_x" else "fixed")
    return(g)
  }
  if (sp$kind == "heat") {
    l <- data.table::rbindlist(lapply(seq_along(cols), function(j) data.table(row = rows, col = lab_of(cols[j]), v = d[[cols[j]]])))
    l[, col := factor(col, levels = lab_of(cols))]
    return(ggplot2::ggplot(l, ggplot2::aes(x = col, y = row, fill = v)) + ggplot2::geom_tile(colour = "white") +
             ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(v), "", formatC(v, format = "f", digits = 0)), colour = !is.na(v) & v <= (sp$target %||% -Inf)), size = 3) +
             ggplot2::scale_colour_manual(values = c(`FALSE` = "grey15", `TRUE` = "#B00020"), guide = "none") +
             ggplot2::scale_fill_gradient(low = "#FDE0DD", high = "#C6E9F7", na.value = "grey95", name = "%", limits = c(min(c(l$v, sp$target %||% 100), na.rm = TRUE), 100)) +
             ggplot2::labs(title = sp$title, subtitle = if (!is.null(sp$target)) sprintf("Red numbers: %s%% or less", sp$target)) + th + ggplot2::theme(panel.grid = ggplot2::element_blank()))
  }
  if (sp$kind == "profile") {
    v <- unlist(d[1, cols, with = FALSE]); l <- data.table(col = factor(lab_of(cols), levels = lab_of(cols)), v = v, mk = cols %in% (sp$mark %||% character()))
    return(ggplot2::ggplot(l, ggplot2::aes(x = col, y = v, fill = mk)) + ggplot2::geom_col(width = 0.75) +
             ggplot2::scale_fill_manual(values = c(`FALSE` = "#1CABE2", `TRUE` = "#F26A21"), guide = "none") + ggplot2::labs(title = sp$title) + th)
  }
  NULL
}

# performance overview: first and second half of the interviews, one indicator, per team
fct_ov_plot <- function(fc, indicator) {
  o <- fc$overview; if (is.null(o)) return(NULL)
  ind <- indicator; o <- o[o[["indicator"]] == ind]; if (!nrow(o)) return(NULL)
  o[, row := factor(row, levels = rev(unique(row)))]
  l <- data.table::rbindlist(list(o[, .(row, half = "First half", v = first)], o[, .(row, half = "Second half", v = second)]))
  ggplot2::ggplot() + ggplot2::geom_segment(data = o[!is.na(first) & !is.na(second)], ggplot2::aes(x = first, xend = second, y = row, yend = row), colour = "grey70", linewidth = 1) +
    ggplot2::geom_point(data = l[!is.na(v)], ggplot2::aes(x = v, y = row, colour = half), size = 2.8) +
    ggplot2::scale_colour_manual(values = c(`First half` = "#B6B7B8", `Second half` = "#1CABE2"), name = NULL) +
    ggplot2::scale_y_discrete(limits = levels(o$row), drop = FALSE) +
    { if (grepl("ratio", indicator)) ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") } +
    ggplot2::labs(title = indicator, subtitle = "First and second half of each team's interviews to date") +
    ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(legend.position = "bottom", axis.title = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                                                            plot.title = ggplot2::element_text(size = 11, face = "bold"))
}

# table for display: formatted text, with the bad cells marked
fct_display <- function(tab) {
  d <- tab$data; cl <- tab$cols[name %in% names(d)]
  out <- data.table(` ` = d$.row)
  for (j in seq_len(nrow(cl))) out[, (paste0("v", j)) := fct_fmt(d[[cl$name[j]]], cl$fmt[j])]
  bad <- vapply(cl$name, function(n) if (!is.null(tab$bad[[n]])) tab$bad[[n]] else rep(FALSE, nrow(d)), logical(nrow(d)))
  if (!is.matrix(bad)) bad <- matrix(bad, nrow = nrow(d))
  list(data = out, cols = cl, bad = bad)
}

#' Save the Field Check Tables in an Excel workbook
#'
#' One sheet per table (as in 'MICS7 Field Check Tables.xlsx'), with the cells where the target is not met in red, a
#' contents sheet, the summary of targets and the performance overview.
#' @param fc result of [ipm_fct()]
#' @param file .xlsx file
#' @export
ipm_fct_xlsx <- function(fc, file) {
  wb <- openxlsx::createWorkbook()
  st_t <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold"); st_s <- openxlsx::createStyle(fontSize = 10, textDecoration = "italic", wrapText = FALSE)
  st_h <- openxlsx::createStyle(fontSize = 9, textDecoration = "bold", wrapText = TRUE, halign = "center", valign = "center", fgFill = "#DCEFF8", border = "TopBottomLeftRight", borderColour = "#B6B7B8")
  st_g <- openxlsx::createStyle(fontSize = 9, textDecoration = "bold", halign = "center", fgFill = "#1CABE2", fontColour = "white", border = "TopBottomLeftRight", borderColour = "#B6B7B8")
  st_c <- openxlsx::createStyle(fontSize = 9, halign = "right", border = "TopBottomLeftRight", borderColour = "#DDDDDD")
  st_r <- openxlsx::createStyle(fontSize = 9, halign = "left", border = "TopBottomLeftRight", borderColour = "#DDDDDD")
  st_tot <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#F2F2F2")
  st_bad <- openxlsx::createStyle(fgFill = "#F8CBAD", fontColour = "#9C0006", textDecoration = "bold")
  lbl <- fc$survey_label; byw <- if (fc$rows == "interv") "interviewer" else "interviewer team"
  # contents
  openxlsx::addWorksheet(wb, "Contents")
  openxlsx::writeData(wb, "Contents", "Field Check Tables", startRow = 1); openxlsx::addStyle(wb, "Contents", st_t, 1, 1)
  openxlsx::writeData(wb, "Contents", sprintf("%s - produced with ipmR %s on %s, rows by %s%s", lbl, IPMR_VERSION, format(Sys.Date(), "%d %B %Y"), byw,
                                              if (!is.null(fc$filters$from)) sprintf(", interviews from %s to %s", format(as.Date(fc$filters$from)), format(as.Date(fc$filters$to %||% Sys.Date()))) else ""), startRow = 2)
  cont <- data.table::rbindlist(lapply(fc$tables, function(t) data.table(Table = t$title, Status = if (isTRUE(t$available)) { nb <- if (length(t$bad)) sum(Reduce(`|`, t$bad)) else 0L
    if (length(t$bad)) sprintf("%d row(s) with a target not met", nb) else "" } else paste("Not produced:", t$why))))
  openxlsx::writeData(wb, "Contents", cont, startRow = 4, headerStyle = st_h); openxlsx::setColWidths(wb, "Contents", 1:2, c(70, 60))
  # summary of targets
  s <- fc$summary
  if (!is.null(s) && nrow(s)) {
    w <- data.table::dcast(s, row + rkey ~ id, value.var = "not_met", fun.aggregate = function(z) if (length(z)) as.integer(any(z)) else NA_integer_)
    w <- w[order(row != "Total", rkey)][, rkey := NULL]
    ids <- intersect(names(fc$tables), names(w)); data.table::setcolorder(w, c("row", ids)); data.table::setnames(w, "row", if (fc$rows == "interv") "Interviewer" else "Team")
    m <- as.matrix(w[, ids, with = FALSE]); w2 <- data.table::copy(w)
    for (j in ids) w2[, (j) := ifelse(is.na(get(j)), "", ifelse(get(j) == 1, "not met", "ok"))]
    openxlsx::addWorksheet(wb, "Targets")
    openxlsx::writeData(wb, "Targets", "Targets not met, by table", startRow = 1); openxlsx::addStyle(wb, "Targets", st_t, 1, 1)
    openxlsx::writeData(wb, "Targets", w2, startRow = 3, headerStyle = st_h)
    ij <- which(m == 1, arr.ind = TRUE); if (nrow(ij)) openxlsx::addStyle(wb, "Targets", st_bad, rows = ij[, 1] + 3, cols = ij[, 2] + 1, stack = TRUE)
    openxlsx::setColWidths(wb, "Targets", 1, 26)
  }
  # performance overview
  if (!is.null(fc$overview)) {
    o <- data.table::dcast(fc$overview, row + rkey ~ indicator, value.var = c("first", "second"))
    o <- o[order(row != "Total", rkey)][, rkey := NULL]
    ind <- unique(fc$overview$indicator); nm <- c("row", unlist(lapply(ind, function(i) paste0(c("first_", "second_"), i))))
    o <- o[, intersect(nm, names(o)), with = FALSE]
    for (j in names(o)[-1]) o[, (j) := round(get(j), 2)]
    openxlsx::addWorksheet(wb, "Overview")
    openxlsx::writeData(wb, "Overview", "Performance overview", startRow = 1); openxlsx::addStyle(wb, "Overview", st_t, 1, 1)
    openxlsx::writeData(wb, "Overview", sprintf("Performance indicators by first and second half of the interviews to date, by %s, %s", byw, lbl), startRow = 2)
    gi <- rep(ind, each = 2); openxlsx::writeData(wb, "Overview", t(c("", gi)), startRow = 4, colNames = FALSE)
    for (k in seq_along(ind)) openxlsx::mergeCells(wb, "Overview", cols = 2 * k + 0:1, rows = 4)
    openxlsx::addStyle(wb, "Overview", st_g, rows = 4, cols = seq_len(ncol(o)), gridExpand = TRUE)
    data.table::setnames(o, c(if (fc$rows == "interv") "Interviewer" else "Team", rep(c("First half", "Second half"), length(ind))))
    openxlsx::writeData(wb, "Overview", o, startRow = 5, headerStyle = st_h)
    openxlsx::setColWidths(wb, "Overview", 1, 26); openxlsx::setColWidths(wb, "Overview", 1 + seq_len(2 * length(ind)), 11)
    openxlsx::writeData(wb, "Overview", "The first and second half are computed for each team (and the total) by splitting its interviews in two by date, not the days of fieldwork.", startRow = 7 + nrow(o))
  }
  # one sheet per table
  for (t in fc$tables) {
    if (!isTRUE(t$available)) next
    sh <- t$id; openxlsx::addWorksheet(wb, sh)
    x <- fct_display(t); cl <- x$cols; nc <- nrow(cl)
    openxlsx::writeData(wb, sh, t$title, startRow = 1); openxlsx::addStyle(wb, sh, st_t, 1, 1)
    openxlsx::writeData(wb, sh, sprintf("%s, according to %s, %s", t$subtitle, byw, lbl), startRow = 2); openxlsx::addStyle(wb, sh, st_s, 2, 1)
    grp <- cl$group; openxlsx::writeData(wb, sh, t(c("", grp)), startRow = 4, colNames = FALSE)
    r <- rle(grp); e <- cumsum(r$lengths); b <- e - r$lengths + 1
    for (k in seq_along(r$values)) if (nzchar(r$values[k])) { if (r$lengths[k] > 1) openxlsx::mergeCells(wb, sh, cols = 1 + b[k]:e[k], rows = 4); openxlsx::addStyle(wb, sh, st_g, rows = 4, cols = 1 + b[k]:e[k], gridExpand = TRUE) }
    openxlsx::writeData(wb, sh, t(c(if (fc$rows == "interv") "Interviewer" else "Team", trimws(cl$label))), startRow = 5, colNames = FALSE)
    openxlsx::addStyle(wb, sh, st_h, rows = 5, cols = 1:(nc + 1), gridExpand = TRUE); openxlsx::setRowHeights(wb, sh, 5, 48)
    num <- as.data.frame(lapply(seq_len(nc), function(j) { v <- t$data[[cl$name[j]]]; d <- switch(cl$fmt[j], n = 0, r2 = 2, 1); round(v, d) }))
    names(num) <- paste0("c", seq_len(nc))
    openxlsx::writeData(wb, sh, data.frame(row = t$data$.row, num, check.names = FALSE), startRow = 6, colNames = FALSE)
    nr <- nrow(t$data)
    openxlsx::addStyle(wb, sh, st_r, rows = 5 + seq_len(nr), cols = 1, gridExpand = TRUE)
    openxlsx::addStyle(wb, sh, st_c, rows = 5 + seq_len(nr), cols = 1 + seq_len(nc), gridExpand = TRUE)
    for (j in seq_len(nc)) openxlsx::addStyle(wb, sh, openxlsx::createStyle(numFmt = switch(cl$fmt[j], n = "#,##0", r2 = "0.00", "0.0")), rows = 5 + seq_len(nr), cols = 1 + j, stack = TRUE)
    openxlsx::addStyle(wb, sh, st_tot, rows = 6, cols = 1:(nc + 1), stack = TRUE)
    ij <- which(x$bad, arr.ind = TRUE); if (nrow(ij)) openxlsx::addStyle(wb, sh, st_bad, rows = ij[, 1] + 5, cols = ij[, 2] + 1, stack = TRUE)
    openxlsx::setColWidths(wb, sh, 1, 24); openxlsx::setColWidths(wb, sh, 1 + seq_len(nc), 11)
    if (!is.null(t$note)) openxlsx::writeData(wb, sh, t$note, startRow = 7 + nr)
    openxlsx::freezePane(wb, sh, firstActiveRow = 6, firstActiveCol = 2)
  }
  # parameters
  openxlsx::addWorksheet(wb, "Parameters")
  sp <- fct_param_spec(); pv <- fc$params[names(sp)]
  openxlsx::writeData(wb, "Parameters", data.frame(Parameter = vapply(sp, `[[`, "", "label"), Value = vapply(pv, function(z) if (is.null(z) || all(is.na(z))) "" else paste(z, collapse = " "), "")), headerStyle = st_h)
  openxlsx::writeData(wb, "Parameters", data.frame(Parameter = c("Urban codes of HH6", "Rural codes of HH6"), Value = c(paste(fc$area$urban, collapse = " "), paste(fc$area$rural, collapse = " "))),
                      startRow = length(sp) + 3, colNames = FALSE)
  openxlsx::setColWidths(wb, "Parameters", 1:2, c(60, 14))
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}
