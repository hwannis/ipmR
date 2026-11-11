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
  fl = paste("Checks of the foundational learning (FL) assessment of children in the 5-17 questionnaire, one chart per check, per interviewer and week: FL completed, median duration (start with the child to end of interview), children who do not want to read, assessments stopped at the reading practice, reading story reached (children 10-14 attending school go straight to the story, the others through the practice), numeracy stopped after number reading.",
    "An interviewer far from team-mates working in the same clusters - very short assessments, many children stopped at the practice or after number reading, few reaching the story - may be ending the assessment early instead of giving each child the chance to try. Observe him/her administering the assessment."),
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
    b <- sprintf("%s tests run on %s questions; %s flagged (%s). If interviewers differed only by chance, about %s of tests would be flagged at p < %s (nominal level).",
                 format(nrow(t), big.mark = ","), format(uniqueN(t[, .(dataset, varname)]), big.mark = ","), format(sum(t$flag), big.mark = ","),
                 pct1(R), pct1(o$levelsig * 100), fmt_int(o$levelsig))
    ratio <- R / (o$levelsig * 100)
    b <- c(b, if (ratio >= 10) "The flag rate is far above the nominal level: there are substantial differences between interviewers of the same team to follow up."
              else if (ratio >= 3) "The flag rate is clearly above the nominal level: there are real differences between interviewers to follow up."
              else "The flag rate is close to the nominal level: little evidence of systematic interviewer effects.",
           "Do not read the flag rate as 'x times chance': each test treats records as independent, but records of the same household (members, births, nets) and of the same cluster are alike, so real differences between the households an interviewer visited also produce flags; and flags are not independent of each other (one interviewer, or one misunderstood question, produces many). Use the % flagged to compare teams, modules, interviewers and successive rounds, not as a measure of the size of the problem.")
    dsr <- t[, .(rate = mean(flag) * 100, tests = .N), by = dataset][order(-rate)]
    b <- c(b, paste0("By dataset: ", paste(sprintf("%s %s", dsr$dataset, pct1(dsr$rate)), collapse = "; "), "."))
    cfgd <- names(res$cfg$datasets)[vapply(res$cfg$datasets, function(d) length(d$flag_modules) > 0, logical(1))]
    none <- setdiff(cfgd, dsr$dataset)
    if (length(none)) b <- c(b, sprintf("No flag test was possible for %s: a test needs at least two interviewers in the same team with %d or more records each in the period (e.g. only one male interviewer per team for the men's questionnaire).",
                                        paste(none, collapse = ", "), o$test_min))
    cl <- fa$clustering
    if (!is.null(cl) && nrow(cl <- cl[per_household >= 1.5 & dataset %in% dsr$dataset]))
      b <- c(b, sprintf("Several records per household in %s: tests count records, not households, and records of the same household (members, nets, births...) tend to have the same answers, so these files flag more easily. Check that a flag in them is not driven by a few large households (flag table), and compare their flag rates with each other rather than with the questionnaire files.",
                        paste(sprintf("%s (%.1f records per household)", cl$dataset, cl$per_household), collapse = ", ")))
    secs$overall <- list(title = "Overall", bullets = b)

    # highly flagged questions (as in the Stata report): the most direct pointer to what needs attention
    qq <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag), desc = vardesc[1], teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1]),
                interviewers_flagged = uniqueN(interv[flag == 1])), by = .(dataset, mod, varname)]
    hq <- qq[rate >= o$highly_flag & flags >= 2][order(-rate, -flags)]
    tabs$highly <- hq
    bq <- if (nrow(hq)) c(
      sprintf("%d question(s) have %s%% or more of their tests flagged. These are the first things to check, in the flag tables (tab 'Flag explorer' or 'Flag tables by team'):", nrow(hq), fmt_int(o$highly_flag)),
      sprintf("%s %s '%s': %s of tests flagged (%d of %d); flagged in %d of %d teams, %d interviewer(s)%s.",
              head(hq$dataset, 25), head(hq$varname, 25), substr(head(hq$desc, 25), 1, 60), pct1(head(hq$rate, 25)), head(hq$flags, 25), head(hq$tests, 25),
              head(hq$teams_flagged, 25), head(hq$teams, 25), head(hq$interviewers_flagged, 25),
              ifelse(head(hq$teams_flagged / hq$teams, 25) >= 0.5, " - many teams: question, translation or training problem", "")),
      if (nrow(hq) > 25) sprintf("... and %d more (full list in the report).", nrow(hq) - 25))
      else sprintf("No question has %s%% or more of its tests flagged.", fmt_int(o$highly_flag))
    secs$highly <- list(title = sprintf("Highly flagged questions (%s%% or more of tests flagged)", fmt_int(o$highly_flag)), bullets = bq)

    # modules
    ms <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag), teams_flagged = uniqueN(team[flag == 1]), teams = uniqueN(team)), by = .(dataset, mod)]
    mq <- qq[, .(q_tested = .N, q_flagged = sum(flags > 0), q_highly = sum(rate >= o$highly_flag & flags >= 2)), by = .(dataset, mod)]
    ms <- merge(ms, mq, by = c("dataset", "mod"))
    ms <- ms[tests >= 30][order(-q_highly, -rate)]
    qs <- t[, .(rate = mean(flag) * 100, tests = .N, desc = vardesc[1]), by = .(dataset, mod, varname)]
    # a long module can have several highly flagged questions "diluted" in a low overall %: keep it too
    top <- head(ms[rate >= max(2 * R, 3) | q_highly >= 2], 8); if (!nrow(top)) top <- head(ms, 3)
    bm <- vapply(seq_len(nrow(top)), function(i) {
      m <- top[i]; q <- qs[dataset == m$dataset & mod == m$mod][order(-rate)][rate >= o$highly_flag | seq_len(.N) <= 2][seq_len(min(.N, 3))]
      sprintf("%s module %s (%s): %d of %d questions flagged at least once, %d highly flagged; %s of tests flagged, %d of %d teams affected. Most flagged questions: %s.",
              m$dataset, m$mod, if (m$teams_flagged / m$teams >= 0.5) "survey-wide problem - review the question wording/training with all teams" else "problem concentrated in some teams",
              m$q_flagged, m$q_tested, m$q_highly, pct1(m$rate), m$teams_flagged, m$teams,
              paste(sprintf("%s '%s' (%.0f%%)", q$varname, substr(q$desc, 1, 60), q$rate), collapse = "; "))
    }, "")
    secs$modules <- list(title = "Modules with the most flagged questions", bullets = c(bm,
      "The % of tests flagged in a module depends on its length: a short module can be pushed up by one question, a long one can hide several highly flagged questions. Read it together with the number of flagged questions."))
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
      qt <- t[team == h$team & flag == 1, .(n_int = uniqueN(interv), desc = vardesc[1]), by = .(dataset, varname)][order(-n_int)][seq_len(min(.N, 5))]
      sprintf("Team %s: %s of tests flagged (survey %s). Questions to target: %s. Modules: %s.%s",
              tl(h$team), pct1(h$rate), pct1(R),
              if (nrow(qt)) paste(sprintf("%s %s '%s'%s", qt$dataset, qt$varname, substr(qt$desc, 1, 40), ifelse(qt$n_int > 1, sprintf(" (%d interviewers)", qt$n_int), "")), collapse = "; ") else "none",
              if (nrow(mods)) paste(sprintf("%s %s (%.0f%% vs %.0f%% overall)", mods$dataset, mods$mod, mods$rate, mods$mrate), collapse = ", ") else "no single module stands out",
              if (nrow(mates)) paste0(" Interviewer(s) driving it: ", paste(sprintf("%g (%.0f%%)", mates$interv, mates$rate), collapse = ", "), ".")
              else " Flags are spread across the team (team-level issue: supervision, team practices).")
    }, "")
    if (!length(bt)) bt <- "No team stands out: flags are spread evenly across teams."
    secs$teams <- list(title = "Teams to observe / re-interview, and questions to target", bullets = bt)
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
    else bi <- c(bi, "Caution: interviewers are compared with their team-mates. An interviewer doing good work can be flagged because the others in the team record the same wrong pattern (e.g. 2 of 3 interviewers answering 'no' to skip a module). Look at the flag table: if the flagged interviewer's answers are the plausible ones, the problem is the team-mates.")
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
    if (nrow(x)) {
      mt <- r$ant_heap$measurers
      mtxt <- if (!is.null(mt)) vapply(x$team, function(tm) { m <- head(mt[team == tm & measured_period >= 10 & pct_heaping_period >= 30][order(-pct_heaping_period)], 3)
        if (nrow(m)) sprintf(" (measurer %s)", paste(sprintf("%s: %.0f%%", fmt_int(m$measurer), m$pct_heaping_period), collapse = ", ")) else "" }, "") else rep("", nrow(x))
      bd <- c(bd, sprintf("Height heaping on .0/.5 above 30%% (about 20%% expected), last 3 weeks: %s - retrain these measurers.", paste(sprintf("%s %.0f%%%s", tl(x$team), x$v, mtxt), collapse = "; ")))
    }
  }
  if (!is.null(r$ant_oor)) {
    d <- recent(copy(r$ant_oor$data)); a <- d[, .(v = sum(pct_oor * measured) / sum(measured), n = sum(measured)), by = team]
    av <- r$ant_oor$sections[[1]]$average
    x <- a[n >= 20 & v >= max(2 * av, 5)][order(-v)]
    if (nrow(x)) bd <- c(bd, sprintf("High share of out-of-range anthropometry (survey %s), last 3 weeks: %s - use the Excel listing for call-backs.", pct1(av), paste(sprintf("%s %.0f%%", tl(x$team), x$v), collapse = "; ")))
  }
  if (!is.null(r$fl) && !is.null(cc <- fl_concerns(recent(copy(r$fl$data))))) {
    for (k in unique(cc$check)) { x <- cc[check == k][order(-abs(v - bm))]; rl <- fl_rules[[k]]
      bd <- c(bd, sprintf("Foundational learning, %s (last 3 weeks) far from %s: %s - observe these interviewers administering the assessment.", rl$what,
                          if (k == "minutes") "the survey median" else "team-mates",
                          paste(sprintf(if (k == "minutes") "%g/%s %.0f min (survey %.0f)" else "%g/%s %.0f%% (team %.0f%%)", x$interv, tl(x$team), x$v, x$bm)[seq_len(min(nrow(x), 10))], collapse = "; "))) }
  }
  if (length(bd)) secs$descriptive <- list(title = "Other warning signs from the descriptive outputs", bullets = bd)
  secs$caveats <- list(title = "How to use these findings", bullets = c(
    "Flags and warning signs are statistical signals, not proof of error or misconduct. Confirm with the flag tables, supervisor feedback, observation or re-interviews before acting.",
    "Small numbers (few interviews per interviewer in the period) make results unstable; re-run the IPM every 1-2 weeks and act on patterns that persist.",
    "When most teams are flagged on the same question, the issue is usually the question, its translation or the training - not the interviewers."))
  list(sections = secs, tables = tabs, plots = plots)
}
