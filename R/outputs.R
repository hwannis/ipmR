# Descriptive outputs 1-8 (ports of quesperday.do ... ant_oor.do)

unicef_cyan <- "#1CABE2"
unicef_dark <- "#374EA2"

theme_ipm <- function(base = 7) {
  ggplot2::theme_bw(base_size = base) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "#E3F4FB", colour = "grey70"),
      strip.text = ggplot2::element_text(size = base - 0.5, face = "bold", colour = "grey15", margin = ggplot2::margin(1, 1, 1, 1)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "grey88", linetype = "dotted"),
      plot.title = ggplot2::element_text(colour = unicef_cyan, face = "bold", size = base + 3),
      plot.subtitle = ggplot2::element_text(size = base + 1),
      plot.caption = ggplot2::element_text(hjust = 0, size = base, colour = "grey30"),
      panel.spacing = ggplot2::unit(2, "pt")
    )
}

# port of team_groups.do: split teams into pages of <= rows*cols panels
team_groups <- function(sdt, o, facet = "interv") {
  t <- unique(sdt[!is.na(team), c(facet, "team"), with = FALSE])[, .(cnt = .N), by = team][order(team)]
  if (!nrow(t)) return(list())
  maxp <- o$scatmaxrow * o$scatmaxcol
  grp <- integer(nrow(t)); g <- 1L; cum <- 0
  for (i in seq_len(nrow(t))) {
    if (cum + t$cnt[i] > maxp && cum > 0) { g <- g + 1L; cum <- 0 }
    cum <- cum + t$cnt[i]; grp[i] <- g
  }
  split(t$team, grp)
}

# generic interviewer/team x week panel graph (Stata: scatter ... by(interv, cols(12)))
panel_pages <- function(sdt, y, n, average, o, title, subtitle_fn, caption = NULL, ylab = NULL,
                        facet = "interv", ylim = NULL, ybreaks = ggplot2::waiver(), ylabels = ggplot2::waiver()) {
  sdt <- sdt[!is.na(team) & !is.na(woi)]
  groups <- team_groups(sdt, o, facet)
  wmax <- max(sdt$woi, na.rm = TRUE)
  lapply(groups, function(tg) {
    d <- sdt[team %in% tg]
    if (facet == "interv") {
      lv <- unique(d[order(team, interv), interv_label]); d[, panel := factor(interv_label, levels = lv)]
    } else {
      lv <- unique(d[order(team), team_label]); d[, panel := factor(team_label, levels = lv)]
    }
    trange <- paste(range(tg), collapse = "-")
    yr <- ylim %||% range(c(0, d[[y]], average), na.rm = TRUE)
    off <- diff(yr) * 0.09
    g <- ggplot2::ggplot(d, ggplot2::aes(x = woi, y = .data[[y]])) +
      ggplot2::geom_hline(yintercept = average, colour = unicef_cyan, linewidth = 0.45) +
      ggplot2::geom_point(size = 0.9, colour = "black") +
      ggplot2::geom_text(ggplot2::aes(y = .data[[y]] - off, label = .data[[n]]), size = 1.5, colour = unicef_dark, angle = 90, hjust = 1) +
      ggplot2::facet_wrap(~panel, ncol = o$scatmaxcol, drop = TRUE) +
      ggplot2::scale_x_continuous(breaks = function(l) unique(pmax(1, round(pretty(l, 4)))), limits = c(0.5, wmax + 0.5)) +
      ggplot2::scale_y_continuous(breaks = ybreaks, labels = ylabels) +
      ggplot2::coord_cartesian(ylim = c(min(yr[1], yr[1] - off * 1.6), yr[2]), clip = "off") +
      ggplot2::labs(title = title, subtitle = subtitle_fn(trange), x = "Week of fieldwork", y = ylab,
                    caption = paste(c(sprintf("Cyan line: overall average (%s). Numbers below dots: sample size in the week.", format(round(average, 1), nsmall = 1)), caption), collapse = "\n")) +
      theme_ipm()
    g
  })
}

ques_datasets <- function(p) names(p$ds)[vapply(names(p$ds), function(n) !is.null(p$cfg$datasets[[n]]$result) && "result" %in% names(p$ds[[n]]), logical(1))]

add_labels <- function(sdt, p) {
  sdt <- merge(sdt, p$team_info, by = "team", all.x = TRUE, sort = FALSE)
  sdt[is.na(team_label) & !is.na(team), team_label := paste0("T", team)]
  sdt
}

interv_lab_map <- function(x) unique(x[!is.na(interv), .(interv, interv_label)], by = "interv")

#' Output 1: number of completed questionnaires per team and per day
#' @param p prepared object from ipm_prepare()
#' @export
out_quesperday <- function(p) {
  o <- p$cfg$options; q <- ques_datasets(p)
  all <- rbindlist(lapply(q, function(n) p$ds[[n]][result %in% o$interco & !is.na(team) & !is.na(doi), .(team, doi)]))
  res <- list(id = "quesperday", title = sprintf("NUMBER OF QUESTIONNAIRES ADMINISTERED (%s) PER TEAM AND PER DAY OF FIELDWORK", paste(q, collapse = " + ")),
              note = "Only datasets with a Result variable and a Day of interview variable are included in this count.")
  if (!nrow(all)) { res$tables <- list(); res$data <- data.table(); return(res) }
  long <- all[, .N, by = .(doi, team)]
  long <- add_labels(long, p)
  wide <- dcast(long, doi ~ team, value.var = "N")
  tl <- p$team_info$team_label[match(as.integer(names(wide)[-1]), p$team_info$team)]
  setnames(wide, c("Date", ifelse(is.na(tl), paste0("T", names(wide)[-1]), tl)))
  wide[, Date := format(as.Date(Date), "%d %b %Y")]
  wide[, Total := rowSums(.SD, na.rm = TRUE), .SDcols = -1]
  nteam <- ncol(wide) - 2
  per <- if (nteam <= 28) nteam else ceiling(nteam / ceiling(nteam / 25))
  blocks <- split(seq_len(nteam) + 1, ceiling(seq_len(nteam) / per))
  res$tables <- lapply(seq_along(blocks), function(i) {
    cols <- c(1, blocks[[i]], if (i == length(blocks)) ncol(wide))
    as.data.frame(wide[, cols, with = FALSE], check.names = FALSE)
  })
  res$data <- long[, .(date = doi, team, team_label, questionnaires = N)]
  res
}

#' Output 2: completion rates per interviewer and week
#' @param p prepared object from ipm_prepare()
#' @export
out_quescrates <- function(p) {
  o <- p$cfg$options
  secs <- lapply(ques_datasets(p), function(n) {
    x <- copy(p$ds[[n]]); x[, completed := as.integer(result %in% o$interco)]
    avg <- mean(x$completed) * 100
    s <- x[!is.na(woi), .(value = mean(completed) * 100, cnt = .N), by = .(team, interv, woi)]
    s <- merge(s, interv_lab_map(x), by = "interv")
    s <- add_labels(s, p)
    plots <- panel_pages(s, "value", "cnt", avg, o, "COMPLETION RATES",
      function(tr) sprintf('"%s" questionnaire; teams %s', n, tr),
      caption = sprintf("Completed interviews: %s = %s", p$cfg$datasets[[n]]$result, paste(o$interco, collapse = ",")),
      ylab = "Completion rate (%)", ylim = c(0, 100), ybreaks = seq(0, 100, 20))
    list(dataset = n, plots = plots, data = s[, .(dataset = n, team, interv, woi, completion_rate = value, n = cnt)], average = avg)
  })
  list(id = "quescrates", title = "COMPLETION RATES OF EACH QUESTIONNAIRE PER INTERVIEWER AND PER WEEK OF FIELDWORK", sections = secs,
       data = rbindlist(lapply(secs, `[[`, "data")))
}

hm_minutes <- function(x, hvar, mvar = NULL) {
  m <- mvar %||% next_var(names(x), hvar, 1)
  h <- suppressWarnings(as.numeric(x[[hvar]])); mi <- suppressWarnings(as.numeric(x[[m]]))
  ok <- !is.na(h) & !is.na(mi) & h >= 0 & h < 24 & mi >= 0 & mi < 60
  out <- rep(NA_real_, nrow(x)); out[ok] <- h[ok] * 60 + mi[ok]
  list(min = out, var = m)
}

#' Output 3: median interview duration per interviewer and week
#' @param p prepared object from ipm_prepare()
#' @export
out_quesmtimes <- function(p) {
  o <- p$cfg$options
  q <- Filter(function(n) !is.null(p$cfg$datasets[[n]]$start_hour) && !is.null(p$cfg$datasets[[n]]$end_hour), ques_datasets(p))
  secs <- lapply(q, function(n) {
    d <- p$cfg$datasets[[n]]
    x <- p$ds[[n]][result %in% o$interco]
    b <- hm_minutes(x, d$start_hour, d$start_min); e <- hm_minutes(x, d$end_hour, d$end_min)
    x <- copy(x); x[, duration := e$min - b$min]
    x[duration <= 0, duration := NA]               # negative: may be completed next day
    x[duration > o$maxquestime, duration := o$maxquestime]
    avg <- stats::median(x$duration, na.rm = TRUE)
    s <- x[!is.na(woi), .(value = stats::median(duration, na.rm = TRUE), cnt = sum(!is.na(duration))), by = .(team, interv, woi)]
    s <- s[cnt > 0]
    s <- merge(s, interv_lab_map(x), by = "interv"); s <- add_labels(s, p)
    mq <- o$maxquestime
    brk <- sort(unique(c(0, 10, 20, 40, mq)[c(0, 10, 20, 40, mq) <= mq]))
    plots <- panel_pages(s, "value", "cnt", avg, o, "MEDIAN TIME TO COMPLETE",
      function(tr) sprintf('"%s" questionnaire; teams %s', n, tr),
      caption = sprintf("Time begins: %s-%s; time ends: %s-%s. Durations capped at %d minutes.", d$start_hour, b$var, d$end_hour, e$var, mq),
      ylab = "Median duration (minutes)", ylim = c(0, mq), ybreaks = brk,
      ylabels = ifelse(brk == mq, paste0(mq, "+"), brk))
    list(dataset = n, plots = plots, data = s[, .(dataset = n, team, interv, woi, median_minutes = value, n = cnt)], average = avg)
  })
  list(id = "quesmtimes", title = "MEDIAN TIME TO COMPLETE EACH QUESTIONNAIRE PER INTERVIEWER AND PER WEEK OF FIELDWORK", sections = secs,
       data = rbindlist(lapply(secs, `[[`, "data")))
}

# port of meannoindiv_graph.do
mean_indiv <- function(x, unit, ingrp, p, title, subtitle, ylab) {
  o <- p$cfg$options
  x <- copy(x); x[, inagegr := as.integer(ingrp)]
  h <- x[!is.na(woi), .(size = sum(inagegr)), by = c(unit, "team", "interv", "woi")]
  avg <- mean(h$size)
  s <- h[, .(value = mean(size), cnt = .N), by = .(team, interv, woi)]
  s[value > 10, value := 10]
  s <- merge(s, interv_lab_map(x), by = "interv"); s <- add_labels(s, p)
  plots <- panel_pages(s, "value", "cnt", avg, o, title, function(tr) sprintf("%s; teams %s", subtitle, tr),
                       caption = "Y axis limited to a maximum of 10 (10 stands for 10+).", ylab = ylab)
  list(plots = plots, data = s, average = avg)
}

#' Output 4: mean number of household members by age/sex group
#' @param p prepared object from ipm_prepare()
#' @export
out_meannohhm <- function(p) {
  o <- p$cfg$options; e <- p$cfg$extras$hhm; n <- e$dataset
  x <- p$ds[[n]]
  if ("result" %in% names(x)) x <- x[result %in% o$interco]
  sex <- as.numeric(x[[e$sex]]); age <- as.numeric(x[[e$age]])
  secs <- lapply(intersect(p$cfg$hhm_groups, names(hhm_group_defs)), function(g) {
    gd <- hhm_group_defs[[g]]
    ing <- rep(TRUE, nrow(x))
    if (!is.na(gd$lo)) ing <- !is.na(age) & age >= gd$lo & age <= gd$hi
    if (!is.na(gd$sex)) ing <- ing & !is.na(sex) & sex == if (gd$sex == "f") o$hhmsexf else o$hhmsexm
    r <- mean_indiv(x, intersect(o$hhunit, names(x)), ing, p, sprintf('MEAN NO. OF HOUSEHOLD MEMBERS ("%s" dataset): %s', n, gd$label),
                    sprintf('Members per household, %s', gd$label), sprintf("Mean (age: %s; sex: %s)", e$age, e$sex))
    r$dataset <- n; r$group <- g; r$data <- r$data[, .(dataset = n, group = g, team, interv, woi, mean_members = value, households = cnt)]
    r
  })
  list(id = "meannohhm", title = "MEAN NUMBER OF HOUSEHOLD MEMBERS PER INTERVIEWER AND PER WEEK OF FIELDWORK", sections = secs,
       data = rbindlist(lapply(secs, `[[`, "data")))
}

#' Output 5: mean number of children ever born (birth history records per mother)
#' @param p prepared object from ipm_prepare()
#' @export
out_everborn <- function(p) {
  o <- p$cfg$options; n <- p$cfg$extras$bh$dataset; x <- p$ds[[n]]
  if ("result" %in% names(x)) x <- x[result %in% o$interco]
  r <- mean_indiv(x, intersect(o$momunit, names(x)), rep(TRUE, nrow(x)), p, sprintf('MEAN NO. OF CHILDREN EVER BORN: "%s" dataset', n),
                  "Births recorded per woman with a birth history", "Mean children ever born")
  r$dataset <- n; r$data <- r$data[, .(dataset = n, team, interv, woi, mean_ceb = value, women = cnt)]
  list(id = "everborn", title = "MEAN NUMBER OF CHILDREN EVER BORN IN BIRTH HISTORIES PER INTERVIEWER AND PER WEEK OF FIELDWORK",
       sections = list(r), data = r$data)
}

#' Output 6: percentage of under-5 deaths among births in birth histories
#' @param p prepared object from ipm_prepare()
#' @export
out_u5deaths <- function(p) {
  o <- p$cfg$options; e <- p$cfg$extras$bh; n <- e$dataset; x <- copy(p$ds[[n]])
  if ("result" %in% names(x)) x <- x[result %in% o$interco]
  dv <- as.numeric(x[[e$death]]); av <- as.numeric(x[[e$age_death]])
  if (!is.null(e$age_death_unit)) {                    # raw birth history: unit (1 days, 2 months, 3 years) + number
    un <- as.numeric(x[[e$age_death_unit]])
    av <- ifelse(un == 1 & av < 98, 0, ifelse(un == 2 & av < 98, av, ifelse(un == 3 & av < 98, av * 12, NA_real_)))
  }
  x[, u5 := as.integer(!is.na(dv) & dv == o$bhdeath & !is.na(av) & av < 60)]
  avg <- mean(x$u5) * 100
  s <- x[!is.na(woi), .(value = mean(u5) * 100, cnt = .N), by = .(team, interv, woi)]
  s <- merge(s, interv_lab_map(x), by = "interv"); s <- add_labels(s, p)
  plots <- panel_pages(s, "value", "cnt", avg, o, "PERCENTAGE OF UNDER-5 DEATHS", function(tr) sprintf('"%s" dataset; teams %s', n, tr),
                       caption = sprintf("Under-5 death: %s=%s & %s<60 months%s", e$death, o$bhdeath, e$age_death, if (!is.null(e$age_death_unit)) paste0(" (converted using unit ", e$age_death_unit, ")") else ""), ylab = "Percent")
  sec <- list(dataset = n, plots = plots, average = avg, data = s[, .(dataset = n, team, interv, woi, pct_u5_deaths = value, births = cnt)])
  list(id = "u5deaths", title = "PERCENTAGE OF UNDER-5 DEATHS IN BIRTH HISTORIES PER INTERVIEWER AND PER WEEK OF FIELDWORK", sections = list(sec), data = sec$data)
}

#' Output 7: heaping on height (.0 / .5) per team and week
#' @param p prepared object from ipm_prepare()
#' @export
out_ant_heap <- function(p) {
  o <- p$cfg$options; e <- p$cfg$extras$ant; n <- e$dataset; x <- copy(p$ds[[n]])
  if ("result" %in% names(x)) x <- x[result %in% o$interco]
  h <- as.numeric(x[[e$height]])                       # Stata bug fixed: was hard-coded AN11
  if (isTRUE(o$exclude_height_codes)) h[!is.na(h) & h >= 900] <- NA
  dec <- round((h %% 1) * 10)
  x[, heaping := ifelse(is.na(dec), NA_real_, as.numeric(dec %in% c(0, 5)))]
  avg <- mean(x$heaping, na.rm = TRUE) * 100
  s <- x[!is.na(woi), .(value = mean(heaping, na.rm = TRUE) * 100, cnt = sum(!is.na(heaping))), by = .(team, woi)][cnt > 0]
  s <- add_labels(s, p); s[, interv := team]
  plots <- panel_pages(s, "value", "cnt", avg, o, "PERCENTAGE OF HEAPING ON HEIGHT", function(tr) sprintf('"%s" dataset; teams %s', n, tr),
                       caption = paste0(sprintf("Heaping: %s ends in .0 or .5 (about 20%% expected).", e$height),
                                        if (isTRUE(o$exclude_height_codes)) " Heights >= 900 (missing codes) excluded." else ""),
                       ylab = "Percent", facet = "team")
  sec <- list(dataset = n, plots = plots, average = avg, data = s[, .(dataset = n, team, team_label, woi, pct_heaping = value, measured = cnt)])
  list(id = "ant_heap", title = "ANTHROPOMETRY - HEAPING ON HEIGHT PER TEAM AND PER WEEK OF FIELDWORK", sections = list(sec), data = sec$data)
}

period_bounds <- function(cfg) {
  st <- as.IDate(cfg$period$start); en <- st + as.integer(cfg$period$days %||% 21)
  list(start = st, end = en, label = sprintf("%s-%s", format(st, "%d%b%Y"), format(en, "%d%b%Y")))
}

#' Output 8: out-of-range / flagged anthropometry per team and week (+ case listing)
#' @param p prepared object from ipm_prepare()
#' @export
out_ant_oor <- function(p) {
  o <- p$cfg$options; e <- p$cfg$extras$ant; n <- e$dataset; x <- copy(p$ds[[n]])
  if ("result" %in% names(x)) x <- x[result %in% o$interco]
  fl <- unlist(e$flags)
  oor <- Reduce(`|`, lapply(fl, function(v) { z <- as.numeric(x[[v]]); !is.na(z) & z == o$outrange }))
  x[, oor := as.integer(oor)]
  pb <- period_bounds(p$cfg)
  mod <- if (grepl("^..[0-9]", e$height)) substr(e$height, 1, 2) else substr(e$height, 1, 3)
  modvars <- expand_varlist(paste0(mod, "*"), names(x))
  cols <- unique(c("doi", "interv", "team", intersect(o$antunit, names(x)), fl, modvars))
  cases <- x[oor == 1 & !is.na(doi) & doi >= pb$start & doi <= pb$end, cols, with = FALSE]
  avg <- mean(x$oor) * 100
  s <- x[!is.na(woi), .(value = mean(oor) * 100, cnt = .N), by = .(team, woi)]
  s <- add_labels(s, p); s[, interv := team]
  plots <- panel_pages(s, "value", "cnt", avg, o, "PERC. OF OUT-OF-RANGE/FLAGGED ANTHRO. RESULTS", function(tr) sprintf('"%s" dataset; teams %s', n, tr),
                       caption = paste("Out-of-range/flagged:", paste(sprintf("%s=%s", fl, o$outrange), collapse = " OR ")), ylab = "Percent", facet = "team")
  sec <- list(dataset = n, plots = plots, average = avg, data = s[, .(dataset = n, team, team_label, woi, pct_oor = value, measured = cnt)])
  list(id = "ant_oor", title = "ANTHROPOMETRY - OUT-OF-RANGE/FLAGGED RESULTS PER TEAM AND PER WEEK OF FIELDWORK", sections = list(sec),
       data = sec$data, cases = cases, period = pb$label, case_labels = p$meta[[n]]$varlab)
}
