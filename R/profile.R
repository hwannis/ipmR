# Team and interviewer pages: everything about one team or one interviewer, built from the run and its scorecard.

# the questionnaire used for interview length and completion: the household questionnaire when there is one,
# otherwise the questionnaire with the most interviews
main_questionnaire <- function(res) {
  d <- res$outputs$quesmtimes$data %||% res$outputs$quescrates$data
  if (is.null(d) || !nrow(d)) return(NA_character_)
  if ("hh" %in% d$dataset) return("hh")
  d[, .(n = sum(n, na.rm = TRUE)), by = dataset][order(-n)]$dataset[1]
}

# weekly series of a weighted mean for named groups of rows (each group is a logical vector over d)
weekly_series <- function(d, vcol, ncol, groups) {
  if (is.null(d) || !nrow(d)) return(NULL)
  out <- rbindlist(lapply(names(groups), function(g) {
    x <- d[groups[[g]] & !is.na(d[[vcol]]) & !is.na(d[[ncol]]) & d[[ncol]] > 0]
    if (!nrow(x)) return(NULL)
    x[, .(value = sum(get(vcol) * get(ncol)) / sum(get(ncol)), n = sum(get(ncol))), by = woi][, group := g]
  }))
  if (!nrow(out)) return(NULL)
  out[, group := factor(group, levels = names(groups))]
  setorder(out, group, woi)[]
}

recent_rows <- function(d, weeks) {
  if (is.null(d) || !nrow(d)) return(d)
  d <- copy(d); d[, .wmax := max(woi, na.rm = TRUE), by = dataset]
  d[!is.na(woi) & woi > .wmax - weeks][, .wmax := NULL][]
}

# issues of a team or interviewer, with their status at this run when a comparison with the last run is given
profile_issues <- function(sc, cmp, rows) {
  s <- if (!is.null(cmp$issues) && nrow(cmp$issues)) cmp$issues else copy(sc$signals)[, `:=`(status = NA_character_, runs = NA_integer_, since = as.Date(NA), was = NA_character_)]
  s <- s[rows(s)]
  s[, `:=`(sev_rank = match(severity, c("high", "medium")), lev_rank = match(level, c("team", "interviewer")))]
  setorder(s, lev_rank, sev_rank, interv)[, c("sev_rank", "lev_rank") := NULL][]
}

#' Team page: everything about one team
#'
#' @param res result of `run_ipm()`
#' @param sc result of `ipm_scorecard()`
#' @param team team number
#' @param cmp result of `ipm_compare()` or `ipm_history_update()`, or NULL
#' @return a list: `team` (one row of the team scorecard), `issues` (team and interviewer issues), `interviewers`
#'   (one row per interviewer: flags, interview length, completion, issues), `modules` (modules where the team stands out),
#'   `trends` (weekly series, team vs the other teams), `questionnaire` (the questionnaire used for length and completion)
#' @export
ipm_team_profile <- function(res, sc, team, cmp = NULL) {
  t <- as.integer(team); wk <- sc$survey$recent_weeks %||% 3
  tr <- sc$teams[team == t]
  if (!nrow(tr)) stop("team ", team, " is not in the scorecard")
  q <- main_questionnaire(res); r <- res$outputs
  issues <- profile_issues(sc, cmp, function(s) !is.na(s$team) & s$team == t & s$level != "survey")

  # interviewers of the team
  ids <- unique(c(if (!is.null(sc$interviewers)) sc$interviewers[team == t, interv],
                  if (!is.null(r$quesmtimes$data)) r$quesmtimes$data[team == t, interv],
                  if (!is.null(r$quescrates$data)) r$quescrates$data[team == t, interv]))
  ids <- sort(ids[!is.na(ids)])
  iv <- data.table(interv = ids)
  if (!is.null(sc$interviewers) && nrow(sc$interviewers)) {
    f <- sc$interviewers[team == t, .(interv, tests, flag_rate = fifelse(is.na(rate_excl), rate, rate_excl))]
    iv <- merge(iv, f, by = "interv", all.x = TRUE)
  } else iv[, `:=`(tests = NA_real_, flag_rate = NA_real_)]
  agg_iv <- function(d, vcol, ncol, name) {
    d <- recent_rows(d, wk); if (is.null(d) || !nrow(d) || is.na(q)) { iv[, (name) := NA_real_]; return(invisible()) }
    a <- d[dataset == q & team == t & !is.na(get(vcol)) & get(ncol) > 0, .(v = sum(get(vcol) * get(ncol)) / sum(get(ncol))), by = interv]
    iv[, (name) := a$v[match(interv, a$interv)]]
  }
  agg_iv(r$quesmtimes$data, "median_minutes", "n", "minutes")
  agg_iv(r$quescrates$data, "completion_rate", "n", "completion")
  ni <- issues[level == "interviewer", .(issues = .N, high = sum(severity == "high"), top = headline[1]), by = interv]
  iv <- merge(iv, ni, by = "interv", all.x = TRUE)
  iv[is.na(issues), `:=`(issues = 0L, high = 0L)]
  iv[, sev_rank := fifelse(high > 0, 1L, fifelse(issues > 0, 2L, 3L))]
  setorder(iv, sev_rank, -issues, interv)[, sev_rank := NULL]

  # modules where the team stands out (questions flagged in most teams left out)
  mods <- if (!is.null(sc$team_modules)) sc$team_modules[team == t & flags >= 2 & rate > module_rate][order(-excess)][seq_len(min(.N, 8))] else NULL
  if (!is.null(mods)) mods <- mods[, .(dataset, module, flags, tests, rate, module_rate, interviewers_flagged)]

  # weekly trends: the team vs the other teams
  trends <- list()
  if (!is.na(q)) {
    vs <- function(d) if (!is.null(d)) list(`This team` = d$dataset == q & !is.na(d$team) & d$team == t, `Other teams` = d$dataset == q & !is.na(d$team) & d$team != t)
    trends$minutes <- weekly_series(r$quesmtimes$data, "median_minutes", "n", vs(r$quesmtimes$data))
    trends$completion <- weekly_series(r$quescrates$data, "completion_rate", "n", vs(r$quescrates$data))
  }
  h <- r$meannohhm$data
  if (!is.null(h) && "a00_04" %in% h$group) { h <- h[group == "a00_04"]
    trends$under5 <- weekly_series(h, "mean_members", "households", list(`This team` = !is.na(h$team) & h$team == t, `Other teams` = !is.na(h$team) & h$team != t)) }
  trends <- Filter(Negate(is.null), trends)

  list(team = tr, issues = issues, interviewers = iv, modules = mods, trends = trends, questionnaire = q,
       survey = sc$survey, recent_weeks = wk)
}

#' Interviewer page: everything about one interviewer
#'
#' @inheritParams ipm_team_profile
#' @param interv interviewer code
#' @return a list: `interviewer` (code, team, flag rate vs team and survey), `issues`, `figures` (interview length and
#'   completion per questionnaire, vs the team and the survey), `modules` (modules with the most flags), `trends`
#'   (weekly interview length: interviewer, team, survey) and `questionnaire`
#' @export
ipm_interviewer_profile <- function(res, sc, interv, cmp = NULL) {
  iid <- as.numeric(interv); r <- res$outputs; wk <- sc$survey$recent_weeks %||% 3; q <- main_questionnaire(res)
  team <- NA_integer_
  for (d in list(sc$interviewers, r$quesmtimes$data, r$quescrates$data)) if (!is.null(d) && is.na(team) && any(d$interv == iid, na.rm = TRUE)) team <- d[interv == iid & !is.na(team), team][1]
  if (is.na(team)) stop("interviewer ", interv, " is not in the results")
  tl <- sc$teams$team_label[match(team, sc$teams$team)]
  hd <- data.table(interv = iid, team = team, team_label = tl, tests = NA_real_, flag_rate = NA_real_, team_rate = NA_real_, survey_rate = sc$survey$flag_rate_excl %||% NA_real_)
  if (!is.null(sc$interviewers) && nrow(x <- sc$interviewers[interv == iid]))
    hd[, `:=`(tests = x$tests, flag_rate = fifelse(is.na(x$rate_excl), x$rate, x$rate_excl), team_rate = sc$teams$rate_excl[match(team, sc$teams$team)])]
  issues <- profile_issues(sc, cmp, function(s) s$level == "interviewer" & !is.na(s$interv) & s$interv == iid)

  # interview length and completion per questionnaire, last weeks: interviewer vs team vs survey
  fig <- function(d, vcol, ncol, what) {
    d <- recent_rows(d, wk); if (is.null(d) || !nrow(d)) return(NULL)
    d <- d[!is.na(get(vcol)) & get(ncol) > 0]
    w <- function(x) if (nrow(x)) sum(x[[vcol]] * x[[ncol]]) / sum(x[[ncol]]) else NA_real_
    rbindlist(lapply(unique(d[interv == iid, dataset]), function(ds) { x <- d[dataset == ds]
      data.table(what = what, dataset = ds, interviewer = w(x[interv == iid]), n = sum(x[interv == iid][[ncol]]),
                 team = w(x[!is.na(team) & team == hd$team]), survey = w(x)) }))
  }
  figures <- rbind(fig(r$quesmtimes$data, "median_minutes", "n", "minutes"), fig(r$quescrates$data, "completion_rate", "n", "completion"))

  mods <- NULL
  if (!is.null(ft <- r$flaganal$tests) && nrow(ft) && any(ft$interv == iid)) {
    x <- copy(ft)[, mod := sub("\\*$", "", module)]
    if (!is.null(sc$questions)) x <- x[!paste(dataset, varname) %in% sc$questions[survey_wide == TRUE, paste(dataset, question)]]   # questionnaire problems left out
    mr <- x[, .(module_rate = mean(flag) * 100), by = .(dataset, mod)]
    mods <- x[interv == iid, .(flags = sum(flag), tests = .N, questions = paste(head(unique(varname[flag == 1]), 4), collapse = ", ")), by = .(dataset, mod)]
    mods <- merge(mods, mr, by = c("dataset", "mod"))[flags > 0][, rate := flags / tests * 100][order(-flags, -rate)][seq_len(min(.N, 6))]
    setnames(mods, "mod", "module")
  }

  trends <- list()
  if (!is.na(q) && !is.null(d <- r$quesmtimes$data))
    trends$minutes <- weekly_series(d, "median_minutes", "n", list(`This interviewer` = d$dataset == q & d$interv %in% iid,
                                                                   `Team` = d$dataset == q & !is.na(d$team) & d$team == hd$team, `Survey` = d$dataset == q))
  list(interviewer = hd, issues = issues, figures = figures, modules = mods, trends = Filter(Negate(is.null), trends),
       questionnaire = q, survey = sc$survey, recent_weeks = wk)
}

trend_titles <- c(minutes = "Median interview length (minutes)", completion = "Completion rate (%)", under5 = "Children under 5 listed per household")

#' Small chart of a weekly trend (team or interviewer page, supervisor brief)
#' @param tr one item of `trends` from `ipm_team_profile()` or `ipm_interviewer_profile()`
#' @param title chart title
#' @return a ggplot
#' @export
ipm_trend_plot <- function(tr, title = NULL) {
  cols <- c(`This team` = "#1CABE2", `This interviewer` = "#1CABE2", `Team` = "#F26A21", `Other teams` = "#9AA0A6", `Survey` = "#9AA0A6")
  ggplot2::ggplot(tr, ggplot2::aes(woi, value, colour = group)) +
    ggplot2::geom_line(linewidth = 0.9) + ggplot2::geom_point(size = 1.6) +
    ggplot2::scale_colour_manual(values = cols, name = NULL) +
    ggplot2::scale_x_continuous(breaks = function(l) unique(round(pretty(l)))) +
    ggplot2::labs(title = title, x = "Week of fieldwork", y = NULL) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "top", legend.justification = "left", panel.grid.minor = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size = 10, face = "bold", colour = "#1f2a44"), plot.title.position = "plot")
}

#' One-page brief per team for the supervisor (Word)
#'
#' What to do this week, the issues with their figures, the interviewers to observe and a trend chart, on one page per team.
#' @inheritParams ipm_team_profile
#' @param teams team numbers, one page each (default: High and Medium priority teams)
#' @param file the .docx to write
#' @return the file name, invisibly
#' @export
ipm_supervisor_brief <- function(res, sc, file, teams = sc$teams[priority %in% c("High", "Medium"), team], cmp = NULL) {
  doc <- officer::body_set_default_section(officer::read_docx(), officer::prop_section(
    page_size = officer::page_size(orient = "portrait", width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 0.6, bottom = 0.6, left = 0.7, right = 0.7, header = 0.3, footer = 0.3)))
  first <- TRUE
  survey <- res$cfg$survey_name %||% sc$survey$name %||% ""
  if (!nzchar(survey)) survey <- "MICS survey"
  if (!length(teams)) { doc <- add_heading(doc, "Supervisor brief"); doc <- add_text(doc, "No team to act on in this run.") }
  for (tm in teams) {
    p <- ipm_team_profile(res, sc, tm, cmp); tr <- p$team
    if (!first) doc <- officer::body_add_break(doc); first <- FALSE
    doc <- add_heading(doc, sprintf("%s - supervisor brief", tr$team_label), 14)
    doc <- add_text(doc, sprintf("%s - period of analysis %s - %s priority - generated %s", survey, sc$survey$period %||% "-", tr$priority, format(Sys.Date(), "%d %b %Y")), 8.5, TRUE)
    s <- p$issues
    doc <- add_heading(doc, "This week, do", 11, "grey15")
    if (nrow(s)) { dd <- unique(s[, .(do, owner, timing)])[seq_len(min(.N, 4))]
      for (k in seq_len(nrow(dd))) doc <- add_bullet(doc, sprintf("%s (%s, %s)", dd$do[k], dd$owner[k], tolower(dd$timing[k])), 9.5)
    } else doc <- add_text(doc, "No issue found for this team: keep the usual spot-checks.")
    tt <- s[level == "team"]
    if (nrow(tt)) { doc <- add_heading(doc, "Team issues", 11, "grey15")
      for (k in seq_len(min(nrow(tt), 6))) doc <- add_bullet(doc, sprintf("%s%s. %s", tt$headline[k], if (tt$severity[k] == "high") " [high]" else "", tt$evidence[k]), 8.5) }
    ii <- s[level == "interviewer"]
    if (nrow(ii)) { doc <- add_heading(doc, "Interviewers to observe", 11, "grey15")
      for (id in head(unique(ii$interv), 6)) { h <- ii[interv == id]
        doc <- add_bullet(doc, sprintf("Interviewer %s: %s.", fmt_int(id), paste0(h$headline, ifelse(h$severity == "high", " [high]", ""), collapse = "; ")), 8.5) } }
    if (length(p$trends)) {
      k <- if (!is.null(p$trends$minutes)) "minutes" else names(p$trends)[1]
      f <- tempfile(fileext = ".png")
      ggplot2::ggsave(f, ipm_trend_plot(p$trends[[k]], sprintf("%s (%s)", trend_titles[[k]], if (k == "under5") "hl" else p$questionnaire)),
                      width = 6.5, height = 2.2, dpi = 150, bg = "white")
      doc <- officer::body_add_img(doc, f, width = 6.5, height = 2.2)
    }
  }
  print(doc, target = file)
  invisible(file)
}
