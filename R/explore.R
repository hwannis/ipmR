# Data explorer: distributions and frequencies of any variable, by team, interviewer, week or another variable.
# Reads only the variables asked for (plus interviewer, team, date and result) from the data files; aggregates only.

#' Read variables of one dataset for the data explorer
#'
#' Reads the chosen variables of one dataset, with the interviewer, team, date, week and result of the interview
#' (taken from the parent questionnaire for linked files such as hl or bh), as in the analysis. Long text variables
#' (names, addresses) are never read.
#' @param cfg configuration (with `data_dir`)
#' @param dataset name of the dataset (e.g. "hh")
#' @param vars variables to read (upper case)
#' @return list(x = data.table, meta = list(varlab, vallab), team_info, dataset, completed_codes)
#' @export
ipm_explore_data <- function(cfg, dataset, vars) {
  cfg <- normalise_config(cfg)
  if (is.null(cfg$datasets[[dataset]])) stop("Unknown dataset: ", dataset)
  chain <- dataset
  while (!is.null(p <- cfg$datasets[[utils::tail(chain, 1)]]$parent) && !p %in% chain) chain <- c(chain, p)
  c2 <- cfg; c2$datasets <- cfg$datasets[rev(chain)]; c2$extras <- list()
  c2$options$drop_long_strings <- TRUE                                  # never read names or other free text
  for (nm in names(c2$datasets)) {
    c2$datasets[[nm]]$flag_modules <- if (nm == dataset) paste(unique(toupper(vars)), collapse = " ") else character()
    c2$datasets[[nm]]$flag_units <- character()
  }
  dat <- ipm_load(c2, quiet = TRUE, slim = TRUE)
  if (is.null(dat$raw[[dataset]])) stop("The file of dataset '", dataset, "' was not found")
  pr <- ipm_prepare(dat, c2, quiet = TRUE, inplace = TRUE)
  x <- pr$ds[[dataset]]
  if (is.null(x)) stop("Dataset '", dataset, "' has no interviewer or date (or its parent questionnaire is missing)")
  if (!"result" %in% names(x) && "presult" %in% names(x)) x[, result := presult]
  list(x = x, meta = dat$meta[[dataset]], team_info = pr$team_info, dataset = dataset, completed_codes = cfg$options$interco %||% 1)
}

# labels that mark "don't know", missing, refused, other... (MICS conventions, several languages)
special_rx <- "don.?t know|\\bdk\\b|missing|no response|not stated|refus|inconsistent|\\bother\\b|ne sait pas|manquant|autre|no sabe|otro|sin respuesta|não sabe|outro"

#' Distribution of one variable (data explorer)
#'
#' Frequencies (categorical) or summary statistics, histogram and last digit (numeric) of one variable, overall and
#' broken down by team, interviewer, week of fieldwork or another variable, with a test of the differences
#' (chi-square, or Kruskal-Wallis for numbers).
#' @param ed object from [ipm_explore_data()]
#' @param var variable
#' @param by "none", "team", "interv", "week" or "var"
#' @param by_var the variable to break down by when `by = "var"`
#' @param completed keep only completed interviews (result code of the questionnaire, or of its parent)
#' @param from,to first and last day of interview to keep (Date or NULL)
#' @param teams,intervs keep only these teams / interviewers (NULL = all)
#' @param type "auto", "categorical" or "numeric"
#' @param skip_blank leave out the records where the variable is blank (question not asked, skipped): they are counted in `blank`
#' @param by_groups when the break-down variable is a number with many values (a score, an age), it is cut into this many
#'   groups of about equal size (5 = quintiles); `binned` in the result says so
#' @param weight sample weight variable (e.g. "HHWEIGHT"), or NULL: percentages, means, SD and percentiles are weighted,
#'   counts (`n`) stay the numbers of records, and the tests and the last digit use the records unweighted
#' @return list(type, var, label, n, blank, missing, freq, stats, digits, cross, num_by, test, by, by_label, groups, binned,
#'   weight, no_weight)
#' @export
ipm_tabulate <- function(ed, var, by = c("none", "team", "interv", "week", "var"), by_var = NULL, completed = TRUE,
                         from = NULL, to = NULL, teams = NULL, intervs = NULL, type = c("auto", "categorical", "numeric"),
                         skip_blank = TRUE, by_groups = 5, weight = NULL) {
  by <- match.arg(by); type <- match.arg(type)
  x <- ed$x; var <- toupper(var)
  if (!var %in% names(x)) stop("Variable ", var, " was not read")
  keep <- rep(TRUE, nrow(x))
  if (completed && "result" %in% names(x)) keep <- keep & x$result %in% ed$completed_codes
  if (!is.null(from)) keep <- keep & !is.na(x$doi) & x$doi >= as.IDate(from)
  if (!is.null(to)) keep <- keep & !is.na(x$doi) & x$doi <= as.IDate(to)
  if (length(teams)) keep <- keep & x$team %in% as.integer(teams)
  if (length(intervs)) keep <- keep & x$interv %in% as.numeric(intervs)
  blank <- sum(keep & is.na(x[[var]]))
  if (skip_blank) keep <- keep & !is.na(x[[var]])
  no_weight <- 0L
  if (!is.null(weight) && nzchar(weight)) {
    weight <- toupper(weight); if (!weight %in% names(x)) stop("Weight variable ", weight, " was not read")
    wt <- suppressWarnings(as.numeric(x[[weight]])); bad <- is.na(wt) | wt < 0
    no_weight <- sum(keep & bad); keep <- keep & !bad
  } else weight <- NULL
  v <- x[[var]][keep]
  w <- if (is.null(weight)) rep(1, length(v)) else wt[keep]
  vl <- ed$meta$vallab[[var]]; lab <- ed$meta$varlab[[var]] %||% ""
  vlab <- function(z) { if (is.null(vl) || !length(z)) return(rep(NA_character_, length(z))); unname(names(vl)[match(z, unname(vl))]) }
  # group
  g <- switch(by, none = NULL, team = x$team[keep], interv = x$interv[keep], week = x$woi[keep],
              var = { bv <- toupper(by_var %||% ""); if (!bv %in% names(x)) stop("Variable ", bv, " was not read"); x[[bv]][keep] })
  # a break-down variable with many numeric values (wealth score, age...) is cut into groups of about equal size
  binned <- NULL
  if (by == "var" && is.numeric(g)) {
    bvl <- ed$meta$vallab[[toupper(by_var)]]
    gsp <- if (!is.null(bvl)) { l <- names(bvl)[match(g, unname(bvl))]; !is.na(l) & grepl(special_rx, l, ignore.case = TRUE) } else rep(FALSE, length(g))
    gu <- unique(g[!is.na(g) & !gsp])
    if (length(gu) > 15 && !(!is.null(bvl) && all(gu %in% unname(bvl)))) {
      k <- max(2L, as.integer(by_groups)); gv <- g[!is.na(g) & !gsp]
      ok <- !is.na(g) & !gsp
      br <- unique(c(min(gv), wquant(gv, seq(0, 1, length.out = k + 1)[-c(1, k + 1)], w[ok]), max(gv)))
      bin <- rep(NA_integer_, length(g))
      bin[ok] <- if (length(br) > 1) cut(g[ok], br, include.lowest = TRUE, labels = FALSE) else 1L
      nm <- unname(c(`3` = "Tercile", `4` = "Quartile", `5` = "Quintile", `10` = "Decile")[as.character(k)]); if (is.na(nm)) nm <- "Group"
      rg <- data.table(b = bin[ok], v = g[ok])[, .(lo = min(v), hi = max(v)), by = b][order(b)]
      f3 <- function(z) vapply(z, function(a) format(signif(a, 3), scientific = FALSE), "")
      binned <- list(var = toupper(by_var), groups = k, name = nm, labels = stats::setNames(sprintf("%s %d (%s to %s)", nm, rg$b, f3(rg$lo), f3(rg$hi)), rg$b))
      g <- bin
    }
  }
  glab <- function(z) {
    if (by == "team") { ti <- ed$team_info; out <- ti$team_label[match(z, ti$team)]; ifelse(is.na(out), ifelse(is.na(z), "(no team)", paste0("T", z)), out) }
    else if (by == "interv") { il <- unique(x[, .(interv, interv_label)]); out <- il$interv_label[match(z, il$interv)]; ifelse(is.na(out), as.character(z), out) }
    else if (by == "week") ifelse(is.na(z), "(before week 1)", paste0("Week ", z))
    else if (by == "var" && !is.null(binned)) ifelse(is.na(z), "(blank or don't know)", unname(binned$labels[as.character(z)]))
    else if (by == "var") { bl <- ed$meta$vallab[[toupper(by_var)]]; out <- if (!is.null(bl)) names(bl)[match(z, unname(bl))] else NA_character_
                            ifelse(is.na(z), "(missing)", ifelse(is.na(out), as.character(z), paste(z, out, sep = " ")))}
    else as.character(z)
  }
  # type
  nonmiss <- v[!is.na(v)]
  is_special <- function(z) { l <- vlab(z); !is.na(l) & grepl(special_rx, l, ignore.case = TRUE) }
  if (type == "auto") {
    if (!is.numeric(v)) type <- "categorical"
    else {
      u <- unique(nonmiss); u <- u[!is_special(u)]
      labelled_all <- !is.null(vl) && length(u) && all(u %in% unname(vl))
      type <- if (length(u) > 15 && !labelled_all) "numeric" else "categorical"
    }
  }
  out <- list(type = type, var = var, label = lab, n = length(v), blank = blank, missing = sum(is.na(v)), by = by, weight = weight, no_weight = no_weight,
              by_label = c(none = "", team = "Team", interv = "Interviewer", week = "Week of fieldwork", var = toupper(by_var %||% ""))[[by]], binned = binned)
  if (!length(v)) return(out)
  if (type == "categorical") {
    f <- data.table(value = v, w = w)[, .(N = .N, W = sum(w)), by = value][order(value)]
    f[, label := vlab(value)]; f[is.na(value), label := "(blank)"]
    f[, pct := 100 * W / sum(W)]; f[, cum_pct := cumsum(pct)]
    f[, special := is.na(value) | is_special(value)]
    out$freq <- f[, .(value, label, n = N, pct, cum_pct, special)]
  } else {
    sp <- !is.na(v) & is_special(v); num <- as.numeric(v[!is.na(v) & !sp]); wn <- w[!is.na(v) & !sp]
    q <- if (length(num)) wquant(num, c(.05, .5, .95), wn) else rep(NA_real_, 3)
    out$stats <- data.table(n = length(num), missing = sum(is.na(v)), special_codes = sum(sp), mean = if (length(num)) wmean(num, wn) else NA_real_,
                            sd = if (length(num) > 1) wsd(num, wn) else NA_real_, min = if (length(num)) min(num) else NA_real_,
                            p5 = q[1], median = q[2], p95 = q[3], max = if (length(num)) max(num) else NA_real_)
    out$values <- num; out$weights <- wn
    out$whole <- length(num) && mean(num == round(num)) >= 0.9          # last digit only means something for whole numbers
    if (length(num) && out$whole) {
      dd <- tabulate(last_digit(num) + 1L, 10)
      out$digits <- data.table(digit = 0:9, n = dd, pct = 100 * dd / sum(dd))
    }
    if (any(sp)) out$special <- data.table(value = v[sp])[, .N, by = value][, .(value, label = vlab(value), n = N)][order(value)]
  }
  if (!is.null(g)) {
    d <- data.table(g = g, v = v, w = w)
    out$groups <- d[, .N, by = g][order(g)][, .(group = glab(g), n = N, key = g)]
    if (type == "categorical") {
      cr <- d[, .(N = .N, W = sum(w)), by = .(g, v)][, pct := 100 * W / sum(W), by = g]
      cr[, value_label := ifelse(is.na(v), "(blank)", ifelse(is.na(vlab(v)), as.character(v), paste(v, vlab(v))))]
      cr[, group := glab(g)]
      out$cross <- cr[order(g, v), .(group, key = g, value = v, value_label, n = N, pct)]
      tb <- table(d$g, d$v, useNA = "no")
      out$test <- if (nrow(tb) > 1 && ncol(tb) > 1) {
        ct <- suppressWarnings(stats::chisq.test(tb)); if (!is.finite(ct$p.value)) NULL else list(name = "Chi-square", statistic = unname(ct$statistic), df = unname(ct$parameter), p = ct$p.value)
      } else NULL
    } else {
      sp <- !is.na(d$v) & is_special(d$v); dn <- d[!is.na(v) & !sp][, v := as.numeric(v)]
      cs <- dn[, .(n = .N, mean = wmean(v, w), sd = if (.N > 1) wsd(v, w) else NA_real_, min = min(v), median = wquant(v, .5, w),
                   p5 = wquant(v, .05, w), p95 = wquant(v, .95, w), max = max(v),
                   pct_0_5 = if (isTRUE(out$whole)) 100 * mean(last_digit(v) %in% c(0, 5)) else NA_real_), by = g][order(g)]
      cs[, group := glab(g)]
      out$num_by <- cs[, c("group", "n", "mean", "sd", "min", "p5", "median", "p95", "max", "pct_0_5", "g"), with = FALSE]
      setnames(out$num_by, "g", "key")
      out$by_values <- dn[, .(group = glab(g), v, w)]
      out$test <- if (data.table::uniqueN(dn$g) > 1 && nrow(dn) > 2) {
        kt <- stats::kruskal.test(dn$v, factor(dn$g)); if (!is.finite(kt$p.value)) NULL else list(name = "Kruskal-Wallis", statistic = unname(kt$statistic), df = unname(kt$parameter), p = kt$p.value)
      } else NULL
    }
  }
  out
}

# weighted mean, SD and quantiles (all weights 1 = the usual unweighted figures)
wmean <- function(x, w) if (sum(w) > 0) sum(w * x) / sum(w) else NA_real_
wsd <- function(x, w) { m <- wmean(x, w); n <- length(x); if (n < 2 || sum(w) <= 0) NA_real_ else sqrt(sum(w * (x - m)^2) / sum(w) * n / (n - 1)) }
wquant <- function(x, probs, w) {
  if (!length(x)) return(rep(NA_real_, length(probs)))
  if (length(unique(w)) == 1) return(stats::quantile(x, probs, names = FALSE))
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(probs, function(p) x[min(length(x), which(cw >= p - 1e-12)[1])], 0)
}

# last digit of the integer part (heaping on 0 and 5 in ages, weights, heights, times)
last_digit <- function(v) as.integer(floor(abs(v) + 1e-9) %% 10)

#' Graph of a distribution from ipm_tabulate()
#' @param t result of [ipm_tabulate()]
#' @param what "dist" (bars or histogram), "digits" (last digit) or "by" (break-down)
#' @return ggplot or NULL
#' @export
ipm_tab_plot <- function(t, what = c("dist", "digits", "by")) {
  what <- match.arg(what)
  tl <- trimws(paste(t$var, if (nzchar(t$label)) paste("-", t$label) else ""))
  th <- ggplot2::theme_minimal(base_size = 11) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank(), plot.title = ggplot2::element_text(size = 11, face = "bold"))
  if (what == "dist") {
    if (t$type == "categorical" && !is.null(t$freq)) {
      f <- utils::head(t$freq, 40); f$lbl <- ifelse(is.na(f$label), as.character(f$value), ifelse(is.na(f$value), f$label, paste(f$value, f$label)))
      f$lbl <- factor(f$lbl, levels = rev(unique(f$lbl)))
      return(ggplot2::ggplot(f, ggplot2::aes(x = pct, y = lbl, fill = special)) + ggplot2::geom_col(width = 0.7) +
               ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.2) +
               ggplot2::scale_fill_manual(values = c(`FALSE` = "#1CABE2", `TRUE` = "#F26A21"), labels = c(`FALSE` = "answer", `TRUE` = "don't know / missing / other"), name = NULL) +
               ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) + ggplot2::labs(title = tl, x = if (is.null(t$weight)) "% of records" else paste0("% (weighted, ", t$weight, ")"), y = NULL) + th +
               ggplot2::theme(legend.position = "bottom"))
    }
    if (t$type == "numeric" && length(t$values)) {
      nb <- min(60, max(10, length(unique(t$values))))
      return(ggplot2::ggplot(data.frame(v = t$values, w = t$weights %||% 1), ggplot2::aes(x = v, weight = w)) + ggplot2::geom_histogram(bins = nb, fill = "#1CABE2", colour = "white") +
               ggplot2::labs(title = tl, x = NULL, y = if (is.null(t$weight)) "Records" else paste("Weighted records (", t$weight, ")", sep = "")) + th)
    }
    return(NULL)
  }
  if (what == "digits") {
    if (is.null(t$digits)) return(NULL)
    return(ggplot2::ggplot(t$digits, ggplot2::aes(x = factor(digit), y = pct, fill = digit %in% c(0, 5))) + ggplot2::geom_col(width = 0.7) +
             ggplot2::geom_hline(yintercept = 10, linetype = "dashed", colour = "grey40") +
             ggplot2::scale_fill_manual(values = c(`FALSE` = "#1CABE2", `TRUE` = "#F26A21"), guide = "none") +
             ggplot2::labs(title = paste("Last digit:", t$var), subtitle = "About 10% each expected; much more on 0 and 5 = heaping (rounding)", x = "Last digit", y = "% of values") + th)
  }
  if (t$type == "categorical" && !is.null(t$cross)) {
    cr <- t$cross; top <- t$freq[order(-n)][!is.na(value)]$value[1:min(12, nrow(t$freq))]
    cr <- cr[is.na(value) | value %in% top]
    cr$group <- factor(cr$group, levels = rev(unique(t$groups$group)))
    cr$value_label <- factor(cr$value_label, levels = unique(cr[order(is.na(value), value)]$value_label))
    return(ggplot2::ggplot(cr, ggplot2::aes(x = value_label, y = group, fill = pct)) + ggplot2::geom_tile(colour = "white") +
             ggplot2::geom_text(ggplot2::aes(label = sprintf("%.0f", pct)), size = 3) +
             ggplot2::scale_fill_gradient(low = "#F2F8FC", high = "#1CABE2", name = "% of row") +
             ggplot2::labs(title = paste(tl, "by", (if (t$by == "var") t$by_label else tolower(t$by_label))), x = NULL, y = NULL) + th +
             ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)))
  }
  if (t$type == "numeric" && !is.null(t$by_values)) {
    vb <- t$by_values; vb$group <- factor(vb$group, levels = rev(unique(t$groups$group)))
    return(ggplot2::ggplot(vb, ggplot2::aes(x = v, y = group, weight = w)) + ggplot2::geom_boxplot(fill = "#E3F3FB", colour = "#1C7FB5", outlier.size = 0.8) +
             ggplot2::labs(title = paste(tl, "by", (if (t$by == "var") t$by_label else tolower(t$by_label))), x = NULL, y = NULL) + th)
  }
  NULL
}
