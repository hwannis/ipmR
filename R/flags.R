# Flag analysis of interviewer bias (port of flaganal.do)
#
# For every question in the selected modules, the answer pattern of each
# interviewer is compared with that of the rest of his/her team over the
# analysis period: Pearson chi-square (categorical / 1-character strings) or
# pooled two-sample t-test (continuous, > max_uniq distinct values). A flag
# is raised when p < levelsig (default 0.001).

fmt_int <- function(x) format(x, scientific = FALSE, trim = TRUE)

# Stata ustrword(levels, -k) on sorted unique non-missing values
last_level <- function(v, k = 1) {
  u <- sort(unique(v[!is.na(v)]))
  if (length(u) >= k) u[length(u) - k + 1] else NA_real_
}

# port of the unit-value pairing block
make_unit_value_pairs <- function(x, units, varlab) {
  for (u in units) {
    vars <- names(x)
    i <- match(u, vars)
    if (is.na(i)) stop("Unit variable ", u, " not found")
    if (i == length(vars)) stop("Unit variable ", u, " is last and cannot be paired")
    v <- vars[i + 1]
    if (!is.numeric(x[[u]]) || !is.numeric(x[[v]])) stop("Unit/value variables must be numeric: ", u, ", ", v)
    uu <- as.numeric(x[[u]]); vv <- as.numeric(x[[v]])
    l1 <- last_level(uu, 1); l2 <- last_level(uu, 2); l3 <- last_level(uu, 3)
    digits <- nchar(fmt_int(l1)); base <- 10^digits
    mult <- 10^nchar(fmt_int(last_level(vv, 1)))
    sq <- if (is.na(l3)) 0 else l3
    if (!is.na(l2) && l2 == base - 2) { sq <- sq + 1; uu[!is.na(uu) & uu == base - 2] <- sq } else sq <- if (is.na(l2)) sq else l2
    if (!is.na(l1) && l1 == base - 1) { sq <- sq + 1; uu[!is.na(uu) & uu == base - 1] <- sq }
    pn <- paste0(u, "_", v, "_pair")
    newc <- data.table(uu * mult + vv); setnames(newc, pn)
    x <- cbind(x[, seq_len(i - 1), with = FALSE], newc, x[, i:length(vars), with = FALSE])
    x <- x[, setdiff(names(x), c(u, v)), with = FALSE]
    varlab[pn] <- paste0(varlab[u] %||% u, " (unit*", mult, " + value)")
  }
  list(x = x, varlab = varlab)
}

# classify and recode one variable; returns NULL (skip) or list(cat, val)
prep_variable <- function(z, fmt, o) {
  if (is.character(z)) {
    nm <- z[!is.na(z) & z != ""]
    if (!length(nm)) return(NULL)                                           # empty
    if (length(nm) == length(z) && length(unique(nm)) == 1) return(NULL)    # uniform
    w <- suppressWarnings(as.integer(sub("^A", "", fmt)))
    is_str1 <- if (!is.na(w)) w <= 1 else max(nchar(z), na.rm = TRUE) <= 1
    if (!is_str1) return(NULL)
    z[is.na(z)] <- ""
    return(list(cat = "SS", val = z))
  }
  z <- as.numeric(z)
  nn <- z[!is.na(z)]
  if (!length(nn)) return(NULL)
  if (length(nn) == length(z) && length(unique(nn)) == 1) return(NULL)
  v <- trunc(z)
  if (length(unique(v[!is.na(v)])) > o$max_uniq) {
    base <- 10^nchar(fmt_int(last_level(v, 1)))
    for (j in 1:4) v[!is.na(v) & v == base - j] <- -j - 1   # 99->-2 (NR), 98->-3 (DK), 97->-4 (NA), 96->-5 (other)
    v[is.na(v)] <- -1
    return(list(cat = "CO", val = v))
  }
  list(cat = "CA", val = v)
}

# chi-square of each interviewer vs rest of team; A = interviewers x values counts
chi2_rows <- function(A) {
  cs <- colSums(A); A <- A[, cs > 0, drop = FALSE]; cs <- cs[cs > 0]
  K <- length(cs); N <- sum(cs)
  n1 <- rowSums(A); n2 <- N - n1
  p <- rep(1, nrow(A))
  ok <- K >= 2 & n1 > 0 & n2 > 0
  if (!any(ok)) return(p)
  B <- matrix(cs, nrow(A), K, byrow = TRUE) - A
  E1 <- outer(n1, cs) / N; E2 <- outer(n2, cs) / N
  chi <- rowSums((A - E1)^2 / E1 + (B - E2)^2 / E2)
  p[ok] <- stats::pchisq(chi[ok], K - 1, lower.tail = FALSE)
  p
}

# pooled t-test of each interviewer vs rest of team; returns p (NA when undefined)
ttest_rows <- function(n1, s1, ss1, N, S, SS) {
  n2 <- N - n1; s2 <- S - s1; ss2 <- SS - ss1
  m1 <- s1 / n1; m2 <- s2 / n2
  dev <- (ss1 - s1^2 / n1) + (ss2 - s2^2 / n2)
  df <- n1 + n2 - 2
  sp2 <- dev / df
  se <- sqrt(sp2 * (1 / n1 + 1 / n2))
  t <- (m1 - m2) / se
  p <- 2 * stats::pt(-abs(t), df)
  p[!is.finite(t) | se <= 1e-9 * pmax(1, abs(m1), abs(m2)) | df <= 0] <- NA
  p
}

value_label_vec <- function(vals, lab) {
  out <- ifelse(is.na(vals), "Missing", as.character(vals))
  if (!is.null(lab) && length(lab)) {
    m <- match(vals, unname(lab))
    out[!is.na(m)] <- paste0(vals[!is.na(m)], ". ", names(lab)[m[!is.na(m)]])
  }
  out
}

# One-sentence reading of a flagged test: where does the interviewer differ most from team-mates?
flag_pattern <- function(z, is_int, cat, lab, interv, team_label) {
  is_int <- !is.na(is_int) & is_int
  pct <- function(a) sprintf("%.0f%%", 100 * a)
  if (cat %in% c("CA", "SS")) {
    if (is.character(z)) z[is.na(z) | z == ""] <- NA
    keep <- if (cat == "CA") !is.na(z) else rep(TRUE, length(z))
    vals <- value_label_vec(z[keep], lab)
    g <- is_int[keep]
    if (!any(g) || all(g)) return(NA_character_)
    ti <- table(vals[g]) / sum(g); tr <- table(vals[!g]) / sum(!g)
    lv <- union(names(ti), names(tr))
    pi <- as.numeric(ti[lv]); pr <- as.numeric(tr[lv]); pi[is.na(pi)] <- 0; pr[is.na(pr)] <- 0
    j <- which.max(abs(pi - pr))
    sprintf("Interviewer %g recorded '%s' for %s of cases (%d/%d) vs %s for the rest of team %s (%d/%d)%s.",
            interv, lv[j], pct(pi[j]), round(pi[j] * sum(g)), sum(g), pct(pr[j]), team_label, round(pr[j] * sum(!g)), sum(!g),
            if (pi[j] > pr[j]) "" else "")
  } else {
    zz <- as.numeric(z); ok <- !is.na(zz)
    hi <- if (any(ok)) 10^nchar(fmt_int(max(trunc(zz[ok])))) else 10
    real <- ok & zz < hi - 4                                         # exclude 96-99 style special codes
    mi <- mean(zz[real & is_int]); mr <- mean(zz[real & !is_int])
    si <- mean(!real[is_int]); sr <- mean(!real[!is_int])
    txt <- sprintf("Interviewer %g: mean %.1f (n=%d) vs %.1f for the rest of team %s (n=%d)", interv, mi, sum(real & is_int), mr, team_label, sum(real & !is_int))
    if (abs(si - sr) >= 0.1) txt <- paste0(txt, sprintf("; missing/DK/special codes %s vs %s", pct(si), pct(sr)))
    paste0(txt, ".")
  }
}

flag_crosstab <- function(team_rows, var, lab) {
  z <- team_rows[[var]]
  if (is.character(z)) z[z == ""] <- NA
  vals <- value_label_vec(z, lab)
  ord <- if (is.numeric(z)) order(is.na(z), z) else order(is.na(z), z)
  lev <- unique(vals[ord])
  il <- unique(team_rows[order(interv), interv_label])
  tb <- table(factor(vals, levels = lev), factor(team_rows$interv_label, levels = il))
  df <- as.data.frame.matrix(tb, stringsAsFactors = FALSE)
  df$Total <- rowSums(tb)
  df <- rbind(df, Total = colSums(df))
  cbind(Value = rownames(df), df, stringsAsFactors = FALSE, row.names = NULL)
}

#' Flag analysis (vectorised engine)
#' @param p prepared object from ipm_prepare()
#' @return list with tests (one row per test), scores, flagged tables and plots
#' @param progress function(message) for progress reporting
#' @param make_tables build crosstabs for flagged tests
#' @param make_plots build flag graphs
#' @export
flag_analysis <- function(p, progress = function(...) NULL, make_tables = TRUE, make_plots = TRUE) {
  o <- p$cfg$options; pb <- period_bounds(p$cfg)
  tests <- list(); tables <- list(); retained <- list()
  dsl <- names(p$ds)[vapply(names(p$ds), function(n) length(p$cfg$datasets[[n]]$flag_modules) > 0, logical(1))]
  for (dsn in dsl) {
    progress(sprintf("Flag analysis: %s", dsn))
    d <- p$cfg$datasets[[dsn]]
    x <- copy(p$ds[[dsn]])
    if ("result" %in% names(x)) x <- x[result %in% o$interco]
    x <- x[!is.na(doi) & doi >= pb$start & doi <= pb$end]
    varlab <- p$meta[[dsn]]$varlab; fmt <- p$meta[[dsn]]$fmt; vallab <- p$meta[[dsn]]$vallab
    if (length(d$flag_units)) { r <- make_unit_value_pairs(x, d$flag_units, varlab); x <- r$x; varlab <- r$varlab }
    data_vars <- setdiff(names(x), c(utility_vars, "interv_label"))
    ign <- expand_varlist(d$flag_ignore, data_vars)
    if (length(ign)) { x[, (ign) := NULL]; data_vars <- setdiff(data_vars, ign) }
    tx <- x[!is.na(team) & !is.na(interv)]
    if (!nrow(tx)) next
    # all-row counts per interviewer / team (Stata: tab int_team, summ int_team)
    cnt_all <- tx[, .(n_i = .N), by = .(team, interv)]
    cnt_all[, `:=`(N_t = sum(n_i), n_int = .N), by = team]
    for (mod in d$flag_modules) {
      vars <- if (identical(trimws(mod), "*")) data_vars else expand_varlist(mod, data_vars)
      varorder <- 0L
      for (v in vars) {
        pv <- prep_variable(x[[v]], fmt[v] %||% "", o)
        if (is.null(pv)) next
        varorder <- varorder + 1L
        vname <- sprintf("v%05d_%s", varorder, v)
        retained[[length(retained) + 1]] <- data.table(dataset = dsn, module = mod, variable = v)
        val <- pv$val[!is.na(x$team) & !is.na(x$interv)]
        dt <- data.table(team = tx$team, interv = tx$interv, val = val)
        if (pv$cat == "CA") dt <- dt[!is.na(val)]
        cc <- copy(cnt_all)
        if (isTRUE(o$test_min_nonmissing)) {
          cc <- dt[, .(n_i = .N), by = .(team, interv)]; cc[, `:=`(N_t = sum(n_i), n_int = .N), by = team]
        }
        elig <- cc[n_int >= 2 & n_i >= o$test_min & N_t >= 2 * o$test_min & N_t > n_i]
        if (!nrow(elig)) next
        res <- list()
        for (tm in unique(elig$team)) {
          ints <- elig[team == tm, interv]
          dtt <- dt[team == tm]
          if (pv$cat %in% c("CA", "SS")) {
            if (!nrow(dtt)) { pvals <- rep(1, length(ints)) } else {
              tab <- dtt[, .N, by = .(interv, val)]
              allint <- sort(unique(c(ints, tab$interv)))
              A <- matrix(0, length(allint), length(unique(tab$val)), dimnames = list(allint, NULL))
              A[cbind(match(tab$interv, allint), match(tab$val, unique(tab$val)))] <- tab$N
              pr <- chi2_rows(A)
              pvals <- pr[match(ints, allint)]
              pvals[is.na(pvals)] <- 1
            }
            ttype <- "chi2-test"
          } else {
            mu <- mean(dtt$val)
            st <- dtt[, .(n1 = .N, s1 = sum(val - mu), ss1 = sum((val - mu)^2)), by = interv]
            N <- sum(st$n1); S <- sum(st$s1); SS <- sum(st$ss1)
            st <- st[match(ints, interv)]
            pvals <- ttest_rows(st$n1, st$s1, st$ss1, N, S, SS)
            ttype <- "t-test"
          }
          res[[length(res) + 1]] <- data.table(dataset = dsn, module = mod, interv = ints, team = tm, variable = vname,
                                               varname = v, vardesc = varlab[v] %||% "", test = ttype, var_cat = pv$cat, p = pvals)
        }
        res <- rbindlist(res)
        res[, flag := as.integer(!is.na(p) & p < o$levelsig)]
        res[, pattern := NA_character_]
        if (any(res$flag == 1)) {
          for (k in which(res$flag == 1)) {
            tr <- x[!is.na(team) & team == res$team[k]]
            pat <- flag_pattern(tr[[v]], tr$interv == res$interv[k], pv$cat, vallab[[v]], res$interv[k],
                                p$team_info$team_label[match(res$team[k], p$team_info$team)] %||% paste0("T", res$team[k]))
            set(res, i = k, j = "pattern", value = pat)
            if (make_tables) tables[[length(tables) + 1]] <- list(
              dataset = dsn, team = res$team[k], interv = res$interv[k], variable = v, module = mod, test = res$test[k], p = res$p[k],
              title = sprintf("Var=%s  Interv=%g  Period=%s  %s p<%s", v, res$interv[k], pb$label, res$test[k], fmt_int(o$levelsig)),
              desc = varlab[v] %||% "", pattern = pat,
              table = flag_crosstab(tr, v, vallab[[v]]))
          }
        }
        tests[[length(tests) + 1]] <- res
      }
    }
  }
  tests <- rbindlist(tests)
  out <- list(tests = tests, tables = tables, period = pb$label, retained = rbindlist(retained))
  if (!nrow(tests)) return(out)
  tests[, flag100 := flag * 100]
  sc <- list(
    data.table(level = "all", dataset = "ALL", module = NA_character_, variable = NA_character_, vardesc = NA_character_,
               flag_score = mean(tests$flag100), tests = nrow(tests), flags = sum(tests$flag), n_var = uniqueN(tests[, .(dataset, module, variable)])),
    tests[, .(level = "dataset", module = NA_character_, variable = NA_character_, vardesc = NA_character_, flag_score = mean(flag100), tests = .N, flags = sum(flag),
              n_var = uniqueN(paste(module, variable))), by = dataset],
    tests[, .(level = "module", variable = NA_character_, vardesc = NA_character_, flag_score = mean(flag100), tests = .N, flags = sum(flag), n_var = uniqueN(variable)), by = .(dataset, module)],
    tests[, .(level = "variable", vardesc = vardesc[1], flag_score = mean(flag100), tests = .N, flags = sum(flag), n_var = 1L), by = .(dataset, module, variable = varname)]
  )
  out$scores <- rbindlist(sc, use.names = TRUE)
  out$highly <- out$scores[level == "variable" & flag_score >= o$highly_flag][order(dataset, module, variable)]
  if (make_plots) out$plots <- flag_plots(tests, out$scores, p, pb, o)
  out
}

flag_plots <- function(tests, scores, p, pb, o) {
  plots <- list()
  ilab <- unique(rbindlist(lapply(p$ds, function(x) x[!is.na(interv), .(interv, interv_label)])), by = "interv")
  for (dsn in unique(tests$dataset)) {
    td <- tests[dataset == dsn]
    fl <- mean(td$flag100)
    s <- td[, .(pct = mean(flag100)), by = .(team, interv)]
    s <- merge(s, ilab, by = "interv", all.x = TRUE); s <- merge(s, p$team_info, by = "team", all.x = TRUE)
    s[, team_label := factor(team_label, levels = unique(p$team_info$team_label))]
    s[, interv_label := factor(interv_label, levels = rev(unique(s[order(team, interv), interv_label])))]
    g <- ggplot2::ggplot(s, ggplot2::aes(x = pct, y = interv_label)) +
      ggplot2::geom_col(fill = unicef_cyan, width = 0.7) +
      ggplot2::geom_vline(xintercept = fl, colour = "#E2231A", linewidth = 0.6) +
      ggplot2::facet_wrap(~team_label, scales = "free_y", ncol = 7) +
      ggplot2::labs(title = sprintf('PERCENTAGE OF FLAGGED QUESTIONS BY INTERVIEWER: "%s" dataset', dsn),
                    subtitle = sprintf("Red line = mean %.1f%% (%s flags / %s tests on %d variables; sig. level %s); period %s",
                                       fl, format(sum(td$flag), big.mark = ","), format(nrow(td), big.mark = ","), uniqueN(td[, .(module, variable)]), fmt_int(o$levelsig), pb$label),
                    x = "% of flagged questions (variables)", y = NULL) +
      theme_ipm()
    plots[[length(plots) + 1]] <- list(type = "dataset", dataset = dsn, plot = g)
    for (mod in unique(td$module)) {
      tm <- td[module == mod]
      vs <- sort(unique(tm$variable)); nv <- length(vs)
      vsc <- tm[, .(pct = mean(flag100)), by = .(variable, varname, vardesc)][order(variable)]
      flm <- mean(tm$flag100)
      ipos <- unique(tm[, .(team, interv)])[order(team, interv)][, k := seq_len(.N) - 1L, by = team][, x := team + k * 0.1]
      tm <- merge(tm, ipos[, .(team, interv, x)], by = c("team", "interv"))
      chunks <- split(vs, ceiling(seq_along(vs) / o$max_flag_var))
      for (ci in seq_along(chunks)) {
        cv <- chunks[[ci]]
        lab <- vsc[variable %in% cv]
        lab[, ylab := sprintf("%s: %s (%.0f%%)", varname, substr(vardesc, 1, 55), pct)]
        dd <- tm[variable %in% cv]
        dd[, yf := factor(variable, levels = rev(cv), labels = rev(lab$ylab[match(cv, lab$variable)]))]
        rng <- range(c(dd$team, dd$x))
        g <- ggplot2::ggplot(dd, ggplot2::aes(x = x, y = yf)) +
          ggplot2::geom_point(data = dd[flag == 0], size = 0.15, colour = "grey80") +
          ggplot2::geom_point(data = dd[flag == 1], shape = 21, size = 1.8, stroke = 0.6, colour = unicef_dark, fill = NA) +
          ggplot2::scale_x_continuous(breaks = sort(unique(dd$team)), labels = p$team_info$team_label[match(sort(unique(dd$team)), p$team_info$team)],
                                      limits = c(rng[1] - 0.2, rng[2] + 0.2)) +
          ggplot2::labs(title = sprintf('DISTRIBUTION OF FLAGS: "%s" dataset, module %s (var. %d-%d of %d)', dsn, mod,
                                        (ci - 1) * o$max_flag_var + 1, (ci - 1) * o$max_flag_var + length(cv), nv),
                        subtitle = sprintf("%.1f%% of tests flagged (%d flags / %d tests on %d variables), period %s. Circles: flagged interviewer; grey dots: tests with no flag.",
                                           flm, sum(tm$flag), nrow(tm), nv, pb$label),
                        x = "Teams (interviewers side by side within each team)", y = NULL) +
          theme_ipm(6) + ggplot2::theme(axis.text.y = ggplot2::element_text(size = 4.6), axis.text.x = ggplot2::element_text(size = 5.5),
                                        panel.grid.major.x = ggplot2::element_line(colour = "grey92"))
        plots[[length(plots) + 1]] <- list(type = "module", dataset = dsn, module = mod, plot = g)
      }
    }
  }
  # summary grid of flag scores by dataset and module
  ms <- scores[level == "module"][, mod := sub("\\*$", "", module)]
  dsc <- scores[level %in% c("all", "dataset")]
  rows <- c("ALL", unique(ms$dataset))
  ms[, row := match(dataset, rows)][, col := seq_len(.N) + 1L, by = dataset]
  dsc[, row := match(dataset, rows)][, col := 0L]
  ms[, cat := cut(flag_score, c(-Inf, 1e-9, 3, 6, 10, 20, Inf), right = FALSE, labels = c("0%", ">0 to <3%", "3 to <6%", "6 to <10%", "10 to <20%", "20%+"))]
  pal <- c("0%" = "white", ">0 to <3%" = "#B5E61D", "3 to <6%" = "#FFF200", "6 to <10%" = "#FF9F1C", "10 to <20%" = "#E2231A", "20%+" = "#C2178E")
  g <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = ms, ggplot2::aes(x = col, y = row, fill = cat), colour = "black", width = 0.9, height = 0.8) +
    ggplot2::geom_text(data = ms, ggplot2::aes(x = col, y = row, label = sprintf("%s\n%s", mod, ifelse(flag_score > 0 & flag_score < 0.5, "<.5", sprintf("%.0f", flag_score)))), size = 2.2, lineheight = 0.85, fontface = "bold") +
    ggplot2::geom_text(data = dsc, ggplot2::aes(x = col, y = row, label = sprintf("%s  %.1f", dataset, flag_score)), size = 2.3, fontface = "bold", hjust = 1) +
    ggplot2::scale_fill_manual(values = pal, drop = FALSE, name = "% of flags") +
    ggplot2::scale_y_reverse() + ggplot2::scale_x_continuous(limits = c(-2.2, max(ms$col, 2) + 0.6)) +
    ggplot2::labs(title = "PERCENTAGE OF FLAGS PER DATASET AND MODULE", subtitle = sprintf("Period %s. Left: dataset name and overall %% of flagged tests.", pb$label), x = NULL, y = NULL) +
    theme_ipm(7) + ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), panel.grid = ggplot2::element_blank(),
                                  panel.border = ggplot2::element_blank(), legend.position = "bottom")
  plots[[length(plots) + 1]] <- list(type = "summary", plot = g)
  plots
}

#' Naive reference implementation of the flag tests (one Stata-like test at a time)
#'
#' Slow; used to cross-check the vectorised engine. Uses stats::chisq.test
#' (no continuity correction) and stats::t.test (pooled variance).
#' @param p prepared object from ipm_prepare()
#' @param datasets datasets to test (default all)
#' @param max_vars maximum variables per module (for quick checks)
#' @export
flag_tests_naive <- function(p, datasets = NULL, max_vars = Inf) {
  o <- p$cfg$options; pb <- period_bounds(p$cfg)
  out <- list()
  for (dsn in datasets %||% names(p$ds)) {
    d <- p$cfg$datasets[[dsn]]; if (!length(d$flag_modules)) next
    x <- copy(p$ds[[dsn]]); if ("result" %in% names(x)) x <- x[result %in% o$interco]
    x <- x[!is.na(doi) & doi >= pb$start & doi <= pb$end]
    varlab <- p$meta[[dsn]]$varlab
    if (length(d$flag_units)) x <- make_unit_value_pairs(x, d$flag_units, varlab)$x
    dv <- setdiff(names(x), c(utility_vars, "interv_label"))
    ign <- expand_varlist(d$flag_ignore, dv); if (length(ign)) { x[, (ign) := NULL]; dv <- setdiff(dv, ign) }
    for (mod in d$flag_modules) {
      vars <- head(expand_varlist(mod, dv), max_vars)
      for (v in vars) {
        pv <- prep_variable(x[[v]], p$meta[[dsn]]$fmt[v] %||% "", o); if (is.null(pv)) next
        val <- pv$val
        for (tm in sort(unique(x$team[!is.na(x$team)]))) for (iv in sort(unique(x$interv[which(x$team == tm)]))) {
          inteam <- which(x$team == tm)
          g <- as.integer(x$interv[inteam] == iv); z <- val[inteam]
          if (isTRUE(o$test_min_nonmissing) && pv$cat == "CA") { keep <- !is.na(z); g2 <- g[keep] } else g2 <- g
          if (length(unique(g2)) != 2 || sum(g2) < o$test_min || length(g2) < 2 * o$test_min) next
          if (pv$cat == "CO") {
            pval <- tryCatch(stats::t.test(z[g == 1], z[g == 0], var.equal = TRUE)$p.value, error = function(e) NA_real_)
          } else {
            ok <- !is.na(z)
            tb <- table(z[ok], g[ok])
            pval <- if (nrow(tb) < 2 || ncol(tb) < 2) 1 else suppressWarnings(stats::chisq.test(tb, correct = FALSE)$p.value)
            if (is.na(pval)) pval <- 1
          }
          out[[length(out) + 1]] <- data.table(dataset = dsn, module = mod, varname = v, team = tm, interv = iv, p_naive = pval)
        }
      }
    }
  }
  rbindlist(out)
}
