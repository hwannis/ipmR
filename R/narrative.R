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
lcfirst <- function(x) ifelse(grepl("^[A-Z]{2}", x), x, paste0(tolower(substr(x, 1, 1)), substring(x, 2)))

#' Narrative summary and follow-up priorities
#'
#' Written from the scorecard (ipm_scorecard(), the single rule engine): the teams to act on, the interviewers
#' to observe and the questionnaire problems are exactly those of the scorecard, with their evidence and action.
#' @param res result of run_ipm()
#' @param sc scorecard (computed if not given)
#' @return list(sections = list of list(title, bullets)), tables, plots
#' @export
ipm_summary <- function(res, sc = ipm_scorecard(res)) {
  o <- res$cfg$options; ti <- res$team_info %||% res$prepared$team_info
  tl <- function(t) { l <- ti$team_label[match(t, ti$team)]; ifelse(is.na(l), paste0("T", t), l) }
  secs <- list(); tabs <- list(); plots <- list()
  sig <- sc$signals; s <- sc$survey
  act <- function(x) sprintf("%s (%s, %s)", x$action, x$owner, tolower(x$timing))
  area_lab <- c(flags = "flags", completion = "completion", duration = "interview length", dk = "'don't know' answers", hhm = "household listing",
                births = "birth history", fl = "FL assessment", anthropometry = "anthropometry")
  alab <- function(a) ifelse(a %in% names(area_lab), area_lab[a], sub("^module ", "module ", a))

  fa <- res$outputs$flaganal
  has_flags <- !is.null(fa) && !is.null(fa$tests) && nrow(fa$tests)
  if (has_flags) {
    t <- copy(fa$tests); t[, mod := sub("\\*$", "", module)]
    R <- s$flag_rate
    b <- sprintf("%s tests run on %s questions; %s flagged (%s). If interviewers differed only by chance, about %s of tests would be flagged at p < %s (nominal level).",
                 format(s$tests, big.mark = ","), format(uniqueN(t[, .(dataset, varname)]), big.mark = ","), format(s$flags, big.mark = ","),
                 pct1(R), pct1(o$levelsig * 100), fmt_int(o$levelsig))
    ratio <- R / (o$levelsig * 100)
    b <- c(b, if (ratio >= 10) "The flag rate is far above the nominal level: there are substantial differences between interviewers of the same team to follow up."
              else if (ratio >= 3) "The flag rate is clearly above the nominal level: there are real differences between interviewers to follow up."
              else "The flag rate is close to the nominal level: little evidence of systematic interviewer effects.")
    if (isTRUE(s$questionnaire_questions > 0))
      b <- c(b, sprintf("%d question(s) are flagged in most teams: they are questionnaire problems (below) and are left out when teams and interviewers are compared (flag rate without them: %s).",
                        s$questionnaire_questions, pct1(s$flag_rate_excl)))
    b <- c(b, "Do not read the flag rate as 'x times chance': records of the same household and cluster are alike, and one interviewer or one misunderstood question produces many flags. Use the % flagged to compare teams, modules, interviewers and successive rounds.")
    dsr <- t[, .(rate = mean(flag) * 100, tests = .N), by = dataset][order(-rate)]
    b <- c(b, paste0("By dataset: ", paste(sprintf("%s %s", dsr$dataset, pct1(dsr$rate)), collapse = "; "), "."))
    cfgd <- names(res$cfg$datasets)[vapply(res$cfg$datasets, function(d) length(d$flag_modules) > 0, logical(1))]
    none <- setdiff(cfgd, dsr$dataset)
    if (length(none)) b <- c(b, sprintf("No flag test was possible for %s: a test needs at least two interviewers in the same team with %d or more records each in the period (e.g. only one male interviewer per team for the men's questionnaire).",
                                        paste(none, collapse = ", "), o$test_min))
    cl <- fa$clustering
    if (!is.null(cl) && nrow(cl <- cl[per_household >= 1.5 & dataset %in% dsr$dataset]))
      b <- c(b, sprintf("Several records per household in %s: records of the same household tend to have the same answers, so these files flag more easily; compare their flag rates with each other rather than with the questionnaire files.",
                        paste(sprintf("%s (%.1f records per household)", cl$dataset, cl$per_household), collapse = ", ")))
  } else b <- "No flag analysis in this run: the findings below come from the descriptive outputs only."
  nt <- table(factor(sc$teams$priority, c("High", "Medium", "Low")))
  b <- c(sprintf("Priorities: %d team(s) High, %d Medium, %d Low; %d interviewer issue(s); %d questionnaire problem(s).",
                 nt[["High"]], nt[["Medium"]], nt[["Low"]], nrow(sig[level == "interviewer"]), nrow(sig[level == "survey"])), b)
  secs$overall <- list(title = "Overall", bullets = b)

  # teams to act on: one line per High/Medium team, with what is wrong and the first action
  tt <- sc$teams[priority != "Low"]
  bt <- vapply(seq_len(nrow(tt)), function(i) {
    h <- tt[i]; ts <- sig[team == h$team & level == "team"]; iv <- sig[team == h$team & level == "interviewer"]
    hi <- function(x) ifelse(x$severity == "high", " [high]", "")
    parts <- c(if (nrow(ts)) paste0(head(paste0(ts$headline, hi(ts)), 4), collapse = ". "),
               if (nrow(ts) > 4) sprintf("+%d more", nrow(ts) - 4),
               if (nrow(iv)) sprintf("Interviewers: %s", paste(vapply(split(iv, iv$interv), function(z) sprintf("%s (%s)", fmt_int(z$interv[1]),
                                     paste(lcfirst(sub(":.*", "", z$headline)), collapse = ", ")), ""), collapse = "; ")))
    first <- if (nrow(ts)) ts[1] else iv[1]
    sprintf("%s - %s. %s. First: %s (%s, %s).", h$team_label, h$priority, paste(parts, collapse = ". "), first$do, first$owner, tolower(first$timing))
  }, "")
  if (!length(bt)) bt <- "No team stands out: no team-wide issue and no team with several interviewer issues."
  secs$teams <- list(title = "Teams to act on (High and Medium priority)", bullets = bt)

  # interviewers: one line per interviewer issue (most severe first)
  ivs <- sig[level == "interviewer"]
  bi <- vapply(seq_len(min(nrow(ivs), 25)), function(i) { h <- ivs[i]
    sprintf("Interviewer %s (%s) - %s%s. Do: %s (%s, %s).", fmt_int(h$interv), h$team_label, h$headline, if (h$severity == "high") " [high]" else "",
            h$do, h$owner, tolower(h$timing))
  }, "")
  if (nrow(ivs) > 25) bi <- c(bi, sprintf("... and %d more in the team scorecard.", nrow(ivs) - 25))
  if (!length(bi)) bi <- "No interviewer stands out from his/her team-mates."
  else bi <- c(bi, "Caution: interviewers are compared with their team-mates. An interviewer doing good work can be flagged because the others record the same wrong pattern; check the flag table before acting.")
  secs$interviewers <- list(title = "Interviewers to observe", bullets = bi)

  # questionnaire problems: questions flagged in most teams, grouped by module
  if (has_flags) {
    qs <- sc$questions
    mq <- qs[, .(q_tested = .N, q_flagged = sum(flags > 0), q_highly = sum(rate >= o$highly_flag & flags >= 2), q_survey_wide = sum(survey_wide)), by = .(dataset, mod = module)]
    ms <- merge(sc$modules[, .(dataset, mod = module, rate, tests, flags, teams_flagged, teams, scope)], mq, by = c("dataset", "mod"))
    ms <- ms[tests >= 30][order(-q_survey_wide, -q_highly, -rate)]
    tabs$modules <- ms
    sw <- sig[level == "survey"]
    keys <- unique(sw[, paste(dataset, module)])
    bm <- vapply(head(keys, 10), function(k) {
      x <- sw[paste(dataset, module) == k]; m <- ms[paste(dataset, mod) == k]; q <- x[type == "question_survey_wide"]
      sprintf("%s %s: %s%s", x$dataset[1], x$module[1],
              if (nrow(q)) paste(c(head(sprintf("%s (%g/%g teams)", q$question, q$value, q$benchmark), 5), if (nrow(q) > 5) sprintf("%d more", nrow(q) - 5)), collapse = ", ")
              else "module flagged in most teams",
              if (nrow(m)) sprintf(" - %d of %d questions flagged at least once, %s of tests.", m$q_flagged, m$q_tested, pct1(m$rate)) else ".")
    }, "")
    if (length(keys)) bm <- c(bm, "For these questions: check the wording, translation and CAPI routing; send a clarification note to all teams and cover them in the next field call or refresher (Survey coordinator, this week). They are not counted against teams or interviewers.")
    if (length(keys) > 10) bm <- c(bm, sprintf("... and %d more modules in the team scorecard (questionnaire level).", length(keys) - 10))
    if (!length(bm)) bm <- "No question or module is flagged in most teams."
    # other modules with many flagged questions, concentrated in some teams (they appear under the teams above)
    oth <- head(ms[!paste(dataset, mod) %in% keys & (q_highly >= 2 | rate >= max(2 * s$flag_rate, 3))], 5)
    if (nrow(oth)) bm <- c(bm, paste0("Other modules with many flagged questions, concentrated in some teams (see the teams above): ",
      paste(sprintf("%s %s - %d of %d questions flagged at least once, %d highly flagged, %d of %d teams", oth$dataset, oth$mod, oth$q_flagged, oth$q_tested,
                    oth$q_highly, oth$teams_flagged, oth$teams), collapse = "; "), "."))
    secs$modules <- list(title = "Questionnaire problems (questions and modules flagged in most teams)", bullets = bm)

    # highly flagged questions (as in the Stata report)
    hq <- qs[rate >= o$highly_flag & flags >= 2][order(-rate, -flags)]
    tabs$highly <- hq[, .(dataset, mod = module, varname = question, rate, tests, flags, desc = description, teams, teams_flagged, interviewers_flagged)]
    bq <- if (nrow(hq)) c(
      sprintf("%d question(s) have %s%% or more of their tests flagged (flag tables: tab 'Flag explorer' or 'Flag tables by team'):", nrow(hq), fmt_int(o$highly_flag)),
      sprintf("%s %s '%s': %s of tests flagged (%d of %d) in %d of %d teams%s.",
              head(hq$dataset, 25), head(hq$question, 25), substr(head(hq$description, 25), 1, 60), pct1(head(hq$rate, 25)), head(hq$flags, 25), head(hq$tests, 25),
              head(hq$teams_flagged, 25), head(hq$teams, 25), ifelse(head(hq$survey_wide, 25), " - questionnaire problem", "")),
      if (nrow(hq) > 25) sprintf("... and %d more (full list in the report).", nrow(hq) - 25))
      else sprintf("No question has %s%% or more of its tests flagged.", fmt_int(o$highly_flag))
    secs$highly <- list(title = sprintf("Highly flagged questions (%s%% or more of tests flagged)", fmt_int(o$highly_flag)), bullets = bq)

    tabs$teams <- sc$teams[!is.na(rate), .(team = team_label, rate, tests, flags)][order(-rate)]
    tabs$interviewers <- if (!is.null(sc$interviewers)) sc$interviewers[order(-rate)][, .(team = team_label, interv, rate, tests, flags)]

    # team x module heatmap
    tm <- t[, .(rate = mean(flag) * 100, tests = .N, flags = sum(flag)), by = .(team, dataset, mod)]
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
  secs$caveats <- list(title = "How to use these findings", bullets = c(
    "Flags and warning signs are statistical signals, not proof of error or misconduct. Confirm with the flag tables, supervisor feedback, observation or re-interviews before acting.",
    "Whole-team differences can come from the area the team works in (household size, language, access): check this with the supervisor before retraining the team.",
    "Small numbers make results unstable; re-run the IPM every 1-2 weeks and act on patterns that persist."))
  list(sections = secs, tables = tabs, plots = plots)
}
