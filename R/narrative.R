# Narrative summary of results: what is problematic, where, and who may need support

#' How to read each output (used in the report and in the app)
#' @export
ipm_howto <- function() list(
  quesperday = "Each cell is the number of completed questionnaires (all types) returned by a team on a given day. Look for days with unusually high counts (possible rushing), gaps (team not working, or not synchronising data), and teams falling behind their workplan.",
  quescrates = "One panel per interviewer; each dot is one week of work and the small number below it is the number of questionnaires in that week. The cyan line is the survey average. Look for interviewers clearly below their team-mates (more non-response, refusals or 'not at home' than colleagues working in the same areas). Rates of exactly 100% with large numbers week after week can also deserve a check.",
  quesmtimes = "Median minutes to complete a questionnaire, per interviewer and week. Much shorter times than team-mates, or times falling sharply over the weeks, suggest questions are being skipped or rushed. Much longer times may signal difficulties with the questionnaire or the tablet.",
  meannohhm = "Mean number of household members per household, by age/sex group, per interviewer and week. Interviewers listing consistently fewer members in a group than their team-mates (e.g. children 0-4, women 15-49, or a shift of women from 15-49 to 46-49/50+) may be omitting members or displacing ages to avoid eligible individual interviews.",
  everborn = "Mean number of births recorded per woman with a birth history. Interviewers with clearly lower values than team-mates may be omitting births (often dead children or children living elsewhere).",
  u5deaths = "Percentage of births in the birth history that are deaths under age 5. Interviewers recording no or very few deaths while team-mates do may be omitting dead children; unusually high values may indicate stillbirths recorded as live births.",
  ant_heap = "Percentage of heights ending in .0 or .5 cm, per team and week. With accurate measurement about 20% is expected. Much higher values mean measurers round or estimate heights; target them for retraining on reading the measuring board.",
  ant_oor = "Percentage of children with out-of-range / flagged anthropometric results (implausible z-scores), per team and week. Higher values point to measurement or recording errors (wrong units, decimal point, age). The Excel listing gives the cases for call-backs.",
  flaganal = paste(
    "For every question and every interviewer, the answers recorded by the interviewer are compared with those of the rest of his/her team over the period of analysis (chi-square test for categorical answers, t-test for numeric answers).",
    "Team members work in the same clusters at the same time, so their answer patterns should be similar. A flag (p < 0.001) means the difference is very unlikely to be due to chance - by chance alone only about 0.1% of tests would be flagged.",
    "A flag does not prove misconduct; it shows where to look: question misunderstood or misread, answer categories misused, questions skipped or answered 'don't know', respondents led, or genuinely different respondents (e.g. the only interviewer speaking a given language).",
    "Read the flag table to see the pattern, then discuss with the supervisor, observe the interviewer or re-interview a few households."),
  flag_module = "Rows are questions, columns are teams (interviewers of a team side by side). A circle is a flagged interviewer. A horizontal line of circles (many teams flagged on the same question) points to a problem with the question or its training across the survey. A vertical line (one interviewer flagged on many questions) points to a problem with that interviewer.",
  flag_dataset = "Percentage of questions flagged for each interviewer, by team; red line = average for the dataset. Interviewers far to the right of the red line deserve a supervision visit or observation.",
  flag_summary = "Each square is a module; its colour and number give the % of tests flagged. White/green modules are fine; orange, red and magenta modules deserve attention - see the highly flagged questions list.",
  team_module = "Percentage of tests flagged for each team (rows) in each module (columns). Dark cells show which team has a problem with which module; use them to target refresher training and supervision visits."
)

pct1 <- function(x) sprintf("%.1f%%", x)

#' Narrative summary and follow-up priorities
#'
#' @param res result of run_ipm()
#' @return list(sections = list of list(title, bullets)), tables, plots
#' @export
ipm_summary <- function(res) {
  o <- res$cfg$options; ti <- res$team_info %||% res$prepared$team_info
  tl <- function(t) { l <- ti$team_label[match(t, ti$team)]; ifelse(is.na(l), paste0("T", t), l) }
  secs <- list(); tabs <- list(); plots <- list()
  fa <- res$outputs$flaganal
  if (!is.null(fa) && !is.null(fa$tests) && nrow(fa$tests)) {
    t <- copy(fa$tests); t[, mod := sub("\\*$", "", module)]
    R <- mean(t$flag) * 100
    b <- sprintf("%s tests run on %s questions; %s flagged (%s), against about %s expected by chance alone at p < %s.",
                 format(nrow(t), big.mark = ","), format(uniqueN(t[, .(dataset, varname)]), big.mark = ","), format(sum(t$flag), big.mark = ","),
                 pct1(R), pct1(o$levelsig * 100), fmt_int(o$levelsig))
    ratio <- R / (o$levelsig * 100)
    b <- c(b, if (ratio >= 10) sprintf("The flag rate is about %.0f times what chance would produce: interviewer effects are substantial and need follow-up.", ratio)
              else if (ratio >= 3) sprintf("The flag rate is about %.0f times the chance level: there are real interviewer effects to follow up.", ratio)
              else "The flag rate is close to the chance level: little evidence of systematic interviewer effects.")
    dsr <- t[, .(rate = mean(flag) * 100, tests = .N), by = dataset][order(-rate)]
    b <- c(b, paste0("By dataset: ", paste(sprintf("%s %s", dsr$dataset, pct1(dsr$rate)), collapse = "; "), "."))
    cfgd <- names(res$cfg$datasets)[vapply(res$cfg$datasets, function(d) length(d$flag_modules) > 0, logical(1))]
    none <- setdiff(cfgd, dsr$dataset)
    if (length(none)) b <- c(b, sprintf("No flag test was possible for %s: a test needs at least two interviewers in the same team with %d or more records each in the period (e.g. only one male interviewer per team for the men's questionnaire).",
                                        paste(none, collapse = ", "), o$test_min))
    secs$overall <- list(title = "Overall", bullets = b)

    # modules
    ms <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag), teams_flagged = uniqueN(team[flag == 1]), teams = uniqueN(team)), by = .(dataset, mod)]
    ms <- ms[tests >= 30][order(-rate)]
    qs <- t[, .(rate = mean(flag) * 100, tests = .N, desc = vardesc[1]), by = .(dataset, mod, varname)]
    top <- head(ms[rate >= max(2 * R, 3)], 6); if (!nrow(top)) top <- head(ms, 3)
    bm <- vapply(seq_len(nrow(top)), function(i) {
      m <- top[i]; q <- qs[dataset == m$dataset & mod == m$mod][order(-rate)][rate >= o$highly_flag | seq_len(.N) <= 2][seq_len(min(.N, 3))]
      sprintf("%s module %s (%s): %s of tests flagged, %d of %d teams affected. Most flagged questions: %s.",
              m$dataset, m$mod, if (m$teams_flagged / m$teams >= 0.5) "survey-wide problem - review the question wording/training with all teams" else "problem concentrated in some teams",
              pct1(m$rate), m$teams_flagged, m$teams,
              paste(sprintf("%s '%s' (%.0f%%)", q$varname, substr(q$desc, 1, 60), q$rate), collapse = "; "))
    }, "")
    secs$modules <- list(title = "Most problematic modules", bullets = bm)
    tabs$modules <- ms

    # teams
    ts <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag)), by = team][order(-rate)]
    tm <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag)), by = .(team, dataset, mod)]
    mrate <- t[, .(mrate = mean(flag) * 100), by = .(dataset, mod)]
    tm <- merge(tm, mrate, by = c("dataset", "mod"))
    iv <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag)), by = .(team, interv)]
    hot <- ts[rate >= 1.5 * R & flags >= 5]
    bt <- vapply(seq_len(nrow(hot)), function(i) {
      h <- hot[i]
      mods <- tm[team == h$team & flags >= 3 & rate >= 2 * mrate][order(-rate)][seq_len(min(.N, 4))]
      ints <- iv[team == h$team][order(-rate)]
      mates <- ints[rate >= 1.5 * h$rate & flags >= 3]
      sprintf("Team %s: %s of tests flagged (survey %s). Modules to target: %s.%s",
              tl(h$team), pct1(h$rate), pct1(R),
              if (nrow(mods)) paste(sprintf("%s %s (%.0f%% vs %.0f%% overall)", mods$dataset, mods$mod, mods$rate, mods$mrate), collapse = ", ") else "no single module stands out",
              if (nrow(mates)) paste0(" Interviewer(s) driving it: ", paste(sprintf("%g (%.0f%%)", mates$interv, mates$rate), collapse = ", "), ".")
              else " Flags are spread across the team (team-level issue: supervision, team practices).")
    }, "")
    if (!length(bt)) bt <- "No team stands out: flags are spread evenly across teams."
    secs$teams <- list(title = "Teams that may need refresher training or supervision", bullets = bt)
    tabs$teams <- ts[, .(team = tl(team), rate, tests, flags)]

    # interviewers
    iv2 <- iv[flags >= 5 & rate >= 2 * R][order(-rate)][seq_len(min(.N, 15))]
    bi <- vapply(seq_len(nrow(iv2)), function(i) {
      h <- iv2[i]
      mods <- t[interv == h$interv, .(f = sum(flag), n = .N), by = .(dataset, mod)][f > 0][order(-f)][seq_len(min(.N, 3))]
      qq <- t[interv == h$interv & flag == 1][seq_len(min(.N, 3))]
      sprintf("Interviewer %g (team %s): %s of questions flagged (%d/%d). Main modules: %s. Examples: %s",
              h$interv, tl(h$team), pct1(h$rate), h$flags, h$tests,
              paste(sprintf("%s %s (%d flags)", mods$dataset, mods$mod, mods$f), collapse = ", "),
              paste(sprintf("%s - %s", qq$varname, ifelse(is.na(qq$pattern), "", qq$pattern)), collapse = " | "))
    }, "")
    if (!length(bi)) bi <- "No individual interviewer has a flag rate at least twice the survey average."
    secs$interviewers <- list(title = "Interviewers to observe / re-interview (flag rate at least twice the survey average)", bullets = bi)
    tabs$interviewers <- iv[order(-rate)][, .(team = tl(team), interv, rate, tests, flags)]

    # team x module heatmap
    keep <- ms[seq_len(min(.N, 30)), paste(dataset, mod)]
    hm <- tm[paste(dataset, mod) %in% keep]
    hm[, col := factor(paste(dataset, mod), levels = keep)]
    hm[, row := factor(tl(team), levels = rev(tl(sort(unique(team)))))]
    lim <- max(10, stats::quantile(hm$rate, .95))
    plots$team_module <- ggplot2::ggplot(hm, ggplot2::aes(x = col, y = row, fill = rate)) +
      ggplot2::geom_tile(colour = "white") +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(rate > 0, sprintf("%.0f", rate), ""), colour = rate > 0.55 * lim), size = 1.9, show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = c(`FALSE` = "grey15", `TRUE` = "white")) +
      ggplot2::scale_fill_gradient(low = "#F2FAFD", high = "#0058AB", name = "% flagged", limits = c(0, lim), oob = scales::squish) +
      ggplot2::labs(title = "WHICH TEAM HAS PROBLEMS WITH WHICH MODULE", subtitle = sprintf("%% of tests flagged per team and module (30 modules with the highest flag rates); period %s", fa$period),
                    x = NULL, y = NULL) +
      theme_ipm(7) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1), panel.grid = ggplot2::element_blank())
  }

  # descriptive warning signs (most recent 3 weeks)
  recent <- function(d) { if (is.null(d) || !nrow(d)) return(d); d[, wmax := max(woi, na.rm = TRUE), by = dataset][woi >= wmax - 2] }
  bd <- character()
  agg <- function(d, vcol, ncol) { d <- copy(d); d[, `:=`(.v = d[[vcol]], .n = d[[ncol]])]; d[, .(v = sum(.v * .n) / sum(.n), n = sum(.n)), by = .(dataset, team, interv)] }
  r <- res$outputs
  if (!is.null(r$quescrates)) {
    d <- recent(copy(r$quescrates$data)); a <- agg(d, "completion_rate", "n")
    a[, tm := sum(v * n) / sum(n), by = .(dataset, team)]
    x <- a[n >= 10 & v < tm - 15][order(v)]
    if (nrow(x)) bd <- c(bd, sprintf("Low completion (last 3 weeks) vs team-mates: %s.", paste(sprintf("%g/%s %s %.0f%% (team %.0f%%)", x$interv, tl(x$team), x$dataset, x$v, x$tm)[seq_len(min(nrow(x), 10))], collapse = "; ")))
  }
  if (!is.null(r$quesmtimes)) {
    d <- recent(copy(r$quesmtimes$data)); a <- agg(d, "median_minutes", "n")
    a[, tm := stats::median(v), by = dataset]
    x <- a[n >= 5 & v < 0.6 * tm][order(v)]
    if (nrow(x)) bd <- c(bd, sprintf("Very short interviews (median < 60%% of the survey median, last 3 weeks) - possible skipping: %s.", paste(sprintf("%g/%s %s %.0f min (survey %.0f)", x$interv, tl(x$team), x$dataset, x$v, x$tm)[seq_len(min(nrow(x), 10))], collapse = "; ")))
  }
  if (!is.null(r$meannohhm)) {
    for (g in intersect(c("a00_04", "f15_49"), unique(r$meannohhm$data$group))) {
      d <- recent(copy(r$meannohhm$data[group == g])); a <- agg(d, "mean_members", "households")
      a[, tm := sum(v * n) / sum(n), by = team]
      x <- a[n >= 10 & v < 0.7 * tm][order(v / tm)]
      if (nrow(x)) bd <- c(bd, sprintf("Few household members %s listed (< 70%% of team-mates, last 3 weeks) - possible omission/age displacement: %s.",
                                       hhm_group_defs[[g]]$label, paste(sprintf("%g/%s %.2f vs %.2f", x$interv, tl(x$team), x$v, x$tm)[seq_len(min(nrow(x), 10))], collapse = "; ")))
    }
  }
  if (!is.null(r$u5deaths)) {
    d <- recent(copy(r$u5deaths$data)); a <- agg(d, "pct_u5_deaths", "births")
    av <- r$u5deaths$sections[[1]]$average
    x <- a[n >= 30 & v == 0 & av >= 3]
    if (nrow(x)) bd <- c(bd, sprintf("No under-5 deaths recorded despite >= 30 births (survey %s): %s - check for omitted dead children.", pct1(av), paste(sprintf("%g/%s (%d births)", x$interv, tl(x$team), x$n), collapse = "; ")))
  }
  if (!is.null(r$ant_heap)) {
    d <- recent(copy(r$ant_heap$data)); a <- d[, .(v = sum(pct_heaping * measured) / sum(measured), n = sum(measured)), by = team]
    x <- a[n >= 20 & v >= 30][order(-v)]
    if (nrow(x)) bd <- c(bd, sprintf("Height heaping on .0/.5 above 30%% (about 20%% expected), last 3 weeks: %s - retrain measurers.", paste(sprintf("%s %.0f%%", tl(x$team), x$v), collapse = "; ")))
  }
  if (!is.null(r$ant_oor)) {
    d <- recent(copy(r$ant_oor$data)); a <- d[, .(v = sum(pct_oor * measured) / sum(measured), n = sum(measured)), by = team]
    av <- r$ant_oor$sections[[1]]$average
    x <- a[n >= 20 & v >= max(2 * av, 5)][order(-v)]
    if (nrow(x)) bd <- c(bd, sprintf("High share of out-of-range anthropometry (survey %s), last 3 weeks: %s - use the Excel listing for call-backs.", pct1(av), paste(sprintf("%s %.0f%%", tl(x$team), x$v), collapse = "; ")))
  }
  if (length(bd)) secs$descriptive <- list(title = "Other warning signs from the descriptive outputs", bullets = bd)
  secs$caveats <- list(title = "How to use these findings", bullets = c(
    "Flags and warning signs are statistical signals, not proof of error or misconduct. Confirm with the flag tables, supervisor feedback, observation or re-interviews before acting.",
    "Small numbers (few interviews per interviewer in the period) make results unstable; re-run the IPM every 1-2 weeks and act on patterns that persist.",
    "When most teams are flagged on the same question, the issue is usually the question, its translation or the training - not the interviewers."))
  list(sections = secs, tables = tabs, plots = plots)
}
