# Scorecard: deterministic, reproducible evidence base for team-level recommendations.
# Everything the rule-based or AI recommendations say must be traceable to a row of this scorecard.

pctf <- function(x, d = 1) ifelse(is.na(x), "NA", sprintf(paste0("%.", d, "f%%"), x))

#' Standard follow-up actions
#'
#' Action library (MICS field practice) used by the rule-based plan and given to the AI as reference:
#' one entry per type of warning sign, with the action text, owner and timing.
#' @export
ipm_action_library <- function() list(
  team_flags = list(owner = "Supervisor", timing = "This week",
    action = "Debrief the team with its flag tables (tab 'Flag tables by team'); observe at least one interview per flagged interviewer; re-interview 2-3 households on the flagged questions."),
  team_module = list(owner = "Field trainer", timing = "This week",
    action = "Refresher on module %s for the team: re-read the module instructions in the Instructions for Interviewers, practise the flagged questions with mock interviews, and have the supervisor spot-check the module in the next completed questionnaires."),
  interviewer_flags = list(owner = "Supervisor", timing = "Immediately",
    action = "Observe interviewer %s during a full interview; spot-check (re-ask) the flagged questions in 2-3 of his/her households; give one-to-one feedback using the flag table."),
  question_survey_wide = list(owner = "Survey coordinator", timing = "This week",
    action = "Question %s is flagged in many teams: check the wording, translation and CAPI routing; send a clarification note to all teams and cover it in the next field call."),
  module_survey_wide = list(owner = "Survey coordinator", timing = "Next refresher",
    action = "Module %s is problematic survey-wide: include it in the next refresher session for all teams and review its translation and training materials."),
  short_interviews = list(owner = "Supervisor", timing = "Immediately",
    action = "Observe interviewer %s: interviews are much shorter than the survey median - check that all questions and modules are asked (compare module time stamps)."),
  low_completion = list(owner = "Supervisor", timing = "This week",
    action = "Review the result codes of interviewer %s (refusals, not at home, partly completed); check call-backs are made at different times of day (at least 3 visits) before a non-response code is used."),
  hhm_omission = list(owner = "Supervisor", timing = "This week",
    action = "Interviewer %s lists fewer %s than team-mates: re-check the household listing in 2-3 households (probe for babies, visitors, members temporarily away) and age probing; check for ages pushed out of eligibility ranges."),
  everborn_low = list(owner = "Supervisor", timing = "This week",
    action = "Interviewer %s records fewer births per woman than team-mates: retrain on birth history probing (children living elsewhere, children who died) and review his/her birth histories."),
  u5deaths_none = list(owner = "Supervisor", timing = "This week",
    action = "Interviewer %s records no under-5 deaths despite many births: retrain on probing for children who died, including those who died soon after birth."),
  heaping = list(owner = "Measurer trainer", timing = "Immediately",
    action = "Retrain the team's measurers on reading the board to the last 0.1 cm (no rounding to .0/.5); repeat a standardisation exercise; supervisor checks a few measurements."),
  anthro_oor = list(owner = "Measurer trainer", timing = "Immediately",
    action = "Use the anthropometry Excel listing for call-backs and re-measurement; check units and decimal entry, child age and measurement position (lying/standing)."),
  team_duration = list(owner = "Field coordinator", timing = "This week",
    action = "Visit the team: observe 2 full interviews with the supervisor and compare the module time stamps - is the whole team shortening or skipping modules? If the area explains it (e.g. small households), note it; otherwise retrain the team on pace, reading questions in full and probing."),
  team_completion = list(owner = "Field coordinator", timing = "This week",
    action = "Review the team's result codes with the supervisor: call-backs (at least 3 visits at different times of day), handling of refusals, and whether the team's areas are harder (urban, nomadic, insecure); agree on call-backs before the team leaves each cluster."),
  team_dk = list(owner = "Field coordinator", timing = "This week",
    action = "Observe 2 interviews of the team: are 'don't know' answers accepted without probing? Remind the whole team to probe (and use the reference calendar for dates and ages) before recording 'don't know'."),
  team_hhm = list(owner = "Field coordinator", timing = "This week",
    action = "The whole team lists fewer %s per household than the other teams: re-check the listing in 2-3 households per interviewer (babies, visitors, members temporarily away, ages near eligibility limits); unless the area explains it, retrain the team on the household listing."),
  team_fl = list(owner = "Field coordinator", timing = "This week",
    action = "Whole team out of line on %s: observe 2 assessments with the supervisor (the child holds the book, practice items read as instructed, stop rules applied only when the child fails, story offered in another language) and retrain the team if the protocol is not followed."),
  birth_history = list(owner = "Supervisor", timing = "This week",
    action = "Interviewer %s records fewer births or no child deaths compared with team-mates: retrain on birth history probing (children living elsewhere, children who died, including soon after birth) and review his/her birth histories."),
  fl_assessment = list(owner = "Supervisor", timing = "Immediately",
    action = "Observe interviewer %s administering the foundational learning assessment to 1-2 children: the child holds the Reading & Numbers book, the practice items are read as instructed, the stop rules (reading practice, first numbers) are applied only when the child really fails, and children who say no are offered the story in another language; retrain if needed.")
)

# foundational learning checks used by the scorecard and the summary: direction of concern and threshold vs team-mates
fl_rules <- list(
  completed     = list(dir = -1, gap = 15, min_n = 10, what = "FL assessments completed"),
  minutes       = list(dir = -1, ratio = 0.6, min_n = 5, what = "minutes per FL assessment"),
  declined      = list(dir = 1, gap = 20, min_n = 10, what = "children who do not want to read"),
  practice_exit = list(dir = 1, gap = 20, min_n = 10, what = "assessments stopped at the reading practice"),
  story         = list(dir = -1, gap = 20, min_n = 10, what = "children reaching the reading story"),
  num_exit      = list(dir = 1, gap = 20, min_n = 10, what = "numeracy stopped after number reading")
)

# interviewers whose FL indicators (last weeks) differ from team-mates (minutes: from the survey median)
fl_concerns <- function(d) {
  if (is.null(d) || !nrow(d)) return(NULL)
  out <- list()
  for (k in intersect(names(fl_rules), unique(d$check))) {
    rl <- fl_rules[[k]]; x <- d[check == k]
    a <- x[, .(v = sum(value * n) / sum(n), n = sum(n)), by = .(dataset, team, interv)]
    if (k == "minutes") { a[, bm := stats::median(v)]; a <- a[n >= rl$min_n & v < rl$ratio * bm]; a[, high := v < 0.4 * bm] }
    else { a[, bm := sum(v * n) / sum(n), by = team]
      a <- a[n >= rl$min_n & rl$dir * (v - bm) > rl$gap]; a[, high := rl$dir * (v - bm) > 2 * rl$gap] }
    if (nrow(a)) out[[k]] <- a[, check := k]
  }
  if (length(out)) rbindlist(out) else NULL
}

#' Team and interviewer scorecard: the single list of issues
#'
#' The one rule engine of ipmR. The summary, the report, the rule-based plan, the AI evidence and the app
#' all read their findings from this scorecard, so they cannot contradict each other.
#' Each issue (row of `signals`) belongs to one of three levels:
#' * "survey": a questionnaire problem - a question or module flagged in most teams (wording, translation, training);
#' * "team": the whole team - its interviewers as a group differ from the other teams (flags, interview length,
#'   completion, "don't know" answers, household listing, FL assessment), or a module flagged far more in the team;
#' * "interviewer": one interviewer differs from team-mates (flags, completion, interview length, listing,
#'   birth history, FL assessment) - one issue per interviewer and area, with the evidence of each check.
#' Questions flagged in most teams are left out of the team and interviewer flag comparisons: they are counted
#' once, as questionnaire problems. Team priority: High = the team as a whole has an excess of flags or 2+
#' high-severity issues; Medium = 1 high-severity issue or 3+ issues; Low = otherwise.
#' @param res result of run_ipm()
#' @param recent_weeks number of most recent weeks used for the descriptive signals
#' @return list(survey, teams, interviewers, modules, questions, team_modules, signals, descriptive)
#' @export
ipm_scorecard <- function(res, recent_weeks = 3) {
  o <- res$cfg$options; ti <- as.data.table(res$team_info %||% res$prepared$team_info)
  tl <- function(t) { l <- ti$team_label[match(t, ti$team)]; ifelse(is.na(l), paste0("T", t), l) }
  lib <- ipm_action_library()
  sig <- list()
  add_sig <- function(level, type, team, interv = NA_real_, dataset = NA_character_, module = NA_character_, question = NA_character_,
                      severity, value = NA_real_, benchmark = NA_real_, evidence, action = NA_character_, owner = NA_character_, timing = NA_character_, area = type)
    sig[[length(sig) + 1L]] <<- data.table(level, type, area, team = as.integer(team), interv = as.numeric(interv), dataset, module, question,
                                           severity, value = as.numeric(value), benchmark = as.numeric(benchmark), evidence, action, owner, timing)
  la <- function(key, ...) { a <- lib[[key]]; list(action = if (...length()) sprintf(a$action, ...) else a$action, owner = a$owner, timing = a$timing) }

  fa <- res$outputs$flaganal
  survey <- list(name = res$cfg$survey_name %||% "", period = fa$period %||% NA_character_,
                 records = sum(res$data_summary$records), datasets = res$data_summary$dataset,
                 teams = sum(!is.na(unique(ti$team))), levelsig = o$levelsig, recent_weeks = recent_weeks)
  teams <- data.table(team = sort(unique(ti$team[!is.na(ti$team)])))
  iv <- NULL; ms <- NULL; qs <- NULL; tm <- NULL

  if (!is.null(fa$tests) && nrow(fa$tests)) {
    t <- copy(fa$tests); t[, mod := sub("\\*$", "", module)]
    R <- mean(t$flag)
    qs <- t[, .(description = vardesc[1], tests = .N, flags = sum(flag), teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1]),
                interviewers_flagged = uniqueN(interv[flag == 1])), by = .(dataset, module = mod, question = varname)]
    qs[, `:=`(rate = flags / tests * 100, survey_wide = teams_flagged >= 3 & teams_flagged / teams >= 0.5)]
    setorder(qs, -flags, -rate)
    # questions flagged in most teams are a questionnaire problem: left out of the team and interviewer comparisons
    t[, sw := paste(dataset, varname) %in% qs[survey_wide == TRUE, paste(dataset, question)]]
    tx <- t[sw == FALSE]; Rx <- if (nrow(tx)) mean(tx$flag) else R
    survey <- c(survey, list(tests = nrow(t), flags = sum(t$flag), flag_rate = R * 100, ratio_to_chance = R / o$levelsig,
                             questionnaire_questions = sum(qs$survey_wide), flag_rate_excl = Rx * 100))

    ts <- t[, .(tests = .N, flags = sum(flag), interviewers_tested = uniqueN(interv)), by = team]
    ts[, `:=`(rate = flags / tests * 100, expected = tests * R, ratio = (flags / tests) / R,
              p_excess = stats::pbinom(flags - 1, tests, R, lower.tail = FALSE))]
    tsx <- tx[, .(tests_x = .N, flags_x = sum(flag)), by = team]
    tsx[, `:=`(rate_excl = flags_x / tests_x * 100, ratio_excl = (flags_x / tests_x) / Rx,
               p_excl = stats::pbinom(flags_x - 1, tests_x, Rx, lower.tail = FALSE))]
    ts <- merge(ts, tsx, by = "team", all.x = TRUE)
    teams <- merge(teams, ts, by = "team", all.x = TRUE)

    iv <- t[, .(tests = .N, flags = sum(flag)), by = .(team, interv)]
    iv[, `:=`(rate = flags / tests * 100, ratio = (flags / tests) / R, p_excess = stats::pbinom(flags - 1, tests, R, lower.tail = FALSE))]
    ivx <- tx[, .(tests_x = .N, flags_x = sum(flag)), by = .(team, interv)]
    ivx[, `:=`(rate_excl = flags_x / tests_x * 100, ratio_excl = (flags_x / tests_x) / Rx,
               p_excl = stats::pbinom(flags_x - 1, tests_x, Rx, lower.tail = FALSE))]
    iv <- merge(iv, ivx, by = c("team", "interv"), all.x = TRUE)
    iv[, team_rate := ts$rate[match(team, ts$team)]]

    ms <- t[, .(tests = .N, flags = sum(flag), teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1]),
                interviewers_flagged = uniqueN(interv[flag == 1]), questions_survey_wide = uniqueN(varname[sw])), by = .(dataset, module = mod)]
    ms[, `:=`(rate = flags / tests * 100, ratio = (flags / tests) / R)]
    ms[, scope := fifelse(teams_flagged >= 3 & teams_flagged / teams >= 0.5, "survey-wide", "concentrated")]
    setorder(ms, -rate)

    tm <- tx[, .(tests = .N, flags = sum(flag), interviewers_flagged = paste(sort(unique(interv[flag == 1])), collapse = ", ")), by = .(team, dataset, module = mod)]
    mrx <- tx[, .(module_rate = mean(flag) * 100), by = .(dataset, module = mod)]
    tm <- merge(tm, mrx, by = c("dataset", "module"))
    tm[, `:=`(rate = flags / tests * 100, excess = flags - tests * module_rate / 100,
              p_excess = stats::pbinom(flags - 1, tests, pmax(module_rate, Rx * 100) / 100, lower.tail = FALSE))]
    setorder(tm, team, -excess)

    # --- questionnaire problems
    for (i in which(qs$survey_wide)) { h <- qs[i]
      a <- la("question_survey_wide", paste(h$dataset, h$question))
      add_sig("survey", "question_survey_wide", NA, dataset = h$dataset, module = h$module, question = h$question,
              severity = if (h$teams_flagged / h$teams >= 0.75) "high" else "medium", value = h$teams_flagged, benchmark = h$teams,
              evidence = sprintf("Question %s %s ('%s') flagged in %d of %d teams (%d interviewers; %s of tests).", h$dataset, h$question, substr(h$description, 1, 70),
                                 h$teams_flagged, h$teams, h$interviewers_flagged, pctf(h$rate, 0)),
              action = a$action, owner = a$owner, timing = a$timing, area = "questionnaire") }
    for (i in which(ms$scope == "survey-wide" & ms$ratio >= 2 & ms$tests >= 30)) { h <- ms[i]
      a <- la("module_survey_wide", paste(h$dataset, h$module))
      add_sig("survey", "module_survey_wide", NA, dataset = h$dataset, module = h$module, severity = if (h$ratio >= 4) "high" else "medium",
              value = h$rate, benchmark = R * 100,
              evidence = sprintf("Module %s %s: %s of tests flagged (%.1f times the survey rate), %d of %d teams affected.",
                                 h$dataset, h$module, pctf(h$rate), h$ratio, h$teams_flagged, h$teams),
              action = a$action, owner = a$owner, timing = a$timing, area = "questionnaire") }

    # --- team and interviewer flags (questions flagged in most teams left out)
    for (i in which(teams$flags_x >= 5 & teams$ratio_excl >= 1.5 & teams$p_excl < 0.01)) { h <- teams[i]
      a <- la("team_flags")
      add_sig("team", "team_flags", h$team, severity = if (h$ratio_excl >= 2.5) "high" else "medium", value = h$rate_excl, benchmark = Rx * 100,
              evidence = sprintf("Whole team: %s of tests flagged (%d of %d) vs %s in the survey, %.1f times the survey rate (questions flagged in most teams left out).",
                                 pctf(h$rate_excl), h$flags_x, h$tests_x, pctf(Rx * 100), h$ratio_excl),
              action = a$action, owner = a$owner, timing = a$timing, area = "flags") }
    for (i in which(tm$flags >= 4 & tm$rate >= 3 * tm$module_rate & tm$rate >= 2 * Rx * 100 & tm$p_excess < 0.01)) { h <- tm[i]
      a <- la("team_module", paste(h$dataset, h$module))
      add_sig("team", "team_module", h$team, dataset = h$dataset, module = h$module,
              severity = if (h$flags >= 6 && h$rate >= 4 * h$module_rate) "high" else "medium", value = h$rate, benchmark = h$module_rate,
              evidence = sprintf("Module %s %s: %s of the team's tests flagged (%d of %d) vs %s in the other teams; flagged interviewer(s): %s.",
                                 h$dataset, h$module, pctf(h$rate), h$flags, h$tests, pctf(h$module_rate), h$interviewers_flagged),
              action = a$action, owner = a$owner, timing = a$timing, area = paste("module", h$dataset, h$module)) }
    for (i in which(iv$flags_x >= 5 & iv$ratio_excl >= 2 & iv$p_excl < 0.01)) { h <- iv[i]
      top <- tx[interv == h$interv & flag == 1, .N, by = .(dataset, mod)][order(-N)][seq_len(min(.N, 3))]
      a <- la("interviewer_flags", fmt_int(h$interv))
      add_sig("interviewer", "interviewer_flags", h$team, h$interv, severity = if (h$ratio_excl >= 4) "high" else "medium", value = h$rate_excl, benchmark = Rx * 100,
              evidence = sprintf("%s of questions flagged (%d of %d) vs %s in the survey and %s for the team; main modules: %s.",
                                 pctf(h$rate_excl), h$flags_x, h$tests_x, pctf(Rx * 100), pctf(teams$rate_excl[match(h$team, teams$team)]),
                                 paste(sprintf("%s %s (%d)", top$dataset, top$mod, top$N), collapse = ", ")),
              action = a$action, owner = a$owner, timing = a$timing, area = "flags") }
  }

  # --- descriptive signals (most recent weeks): whole team vs the other teams, then interviewer vs team-mates
  r <- res$outputs
  recent <- function(d) { if (is.null(d) || !nrow(d)) return(NULL); d <- copy(d)
    if ("dataset" %in% names(d)) d[, wmax := max(woi, na.rm = TRUE), by = dataset] else d[, wmax := max(woi, na.rm = TRUE)]
    d[!is.na(woi) & woi > wmax - recent_weeks] }
  agg <- function(d, vcol, ncol, by = c("dataset", "team", "interv")) { d <- copy(d); d[, `:=`(.v = d[[vcol]], .n = d[[ncol]])]
    d[!is.na(.v) & !is.na(team), .(v = sum(.v * .n) / sum(.n), n = sum(.n)), by = by] }
  # whole team far from the other teams, with most of its interviewers on the same side (otherwise it is one interviewer's issue)
  team_wide <- function(a, kind, dir, gap = NA, ratio = NA, min_diff = 0, min_n, label, fmt, lib_key, lib_arg = NULL, area = kind, when = sprintf("last %d weeks", recent_weeks)) {
    if (is.null(a) || !nrow(a)) return(invisible())
    T <- a[, .(v = sum(v * n) / sum(n), n = sum(n), n_int = sum(n >= 3)), by = .(dataset, team)]
    T[, `:=`(S = sum(v * n), N = sum(n)), by = dataset]
    T[, bm := (S - v * n) / (N - n)]
    far <- function(v, bm, k = 1) (if (!is.na(gap)) dir * (v - bm) >= k * gap else if (dir < 0) v <= bm * (1 - k * (1 - ratio)) else v >= bm * (1 + k * (ratio - 1))) &
      dir * (v - bm) >= k * min_diff                                  # and a difference large enough to matter in the field
    T <- T[n >= min_n & N > n & far(v, bm)]
    for (i in seq_len(nrow(T))) { h <- T[i]
      ai <- a[dataset == h$dataset & team == h$team & n >= 3]
      side <- sum(far(ai$v, h$bm, 0.5))
      if (nrow(ai) < 2 || side < max(2, ceiling(2 / 3 * nrow(ai)))) next
      worst <- head(ai[order(dir * -v)], 2)
      ac <- if (is.null(lib_arg)) la(lib_key) else la(lib_key, lib_arg)
      add_sig("team", paste0("team_", kind), h$team, dataset = h$dataset, severity = if (far(h$v, h$bm, 2)) "high" else "medium", value = h$v, benchmark = h$bm,
              evidence = sprintf("Whole team, %s: %s vs %s in the other teams (%s, %s); %d of %d interviewers on the same side, furthest: %s.",
                                 label, fmt(h$v), fmt(h$bm), h$dataset, when, side, nrow(ai),
                                 paste(sprintf("%s (%s)", fmt_int(worst$interv), fmt(worst$v)), collapse = ", ")),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = area)
    }
  }
  f_pct <- function(x) sprintf("%.0f%%", x); f_min <- function(x) sprintf("%.0f min", x); f_dec <- function(x) sprintf("%.2f", x)
  desc <- list()
  if (!is.null(d <- recent(r$quescrates$data))) {
    a <- agg(d, "completion_rate", "n"); team_wide(a, "completion", -1, gap = 10, min_n = 30, label = "completion rate", fmt = f_pct, lib_key = "team_completion")
    a[, tm := sum(v * n) / sum(n), by = .(dataset, team)]
    desc$completion <- a
    for (i in which(a$n >= 10 & a$v < a$tm - 15)) { h <- a[i]; ac <- la("low_completion", fmt_int(h$interv))
      add_sig("interviewer", "low_completion", h$team, h$interv, dataset = h$dataset, severity = if (h$v < h$tm - 30) "high" else "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("Completion rate %s (%s questionnaires) vs %s for the team, last %d weeks.", pctf(h$v, 0), h$dataset, pctf(h$tm, 0), recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "completion") }
  }
  if (!is.null(d <- recent(r$quesmtimes$data))) {
    a <- agg(d, "median_minutes", "n"); team_wide(a, "duration", -1, ratio = 0.7, min_diff = 5, min_n = 20, label = "median interview length", fmt = f_min, lib_key = "team_duration")
    a[, sm := stats::median(v), by = dataset]
    desc$duration <- a
    for (i in which(a$n >= 5 & a$v < 0.6 * a$sm)) { h <- a[i]; ac <- la("short_interviews", fmt_int(h$interv))
      add_sig("interviewer", "short_interviews", h$team, h$interv, dataset = h$dataset, severity = if (h$v < 0.4 * h$sm) "high" else "medium",
              value = h$v, benchmark = h$sm, evidence = sprintf("Median %.0f minutes per %s questionnaire vs %.0f survey median, last %d weeks.", h$v, h$dataset, h$sm, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "duration") }
  }
  if (!is.null(fa$dk) && nrow(fa$dk)) {
    a <- fa$dk[answers > 0, .(v = 100 * sum(dk) / sum(answers), n = sum(answers)), by = .(dataset, team, interv)]
    team_wide(a, "dk", 1, ratio = 2, min_diff = 3, min_n = 500, label = "share of 'don't know' answers", fmt = function(x) sprintf("%.1f%%", x), lib_key = "team_dk",
              when = "period of analysis")
    desc$dk <- a
  }
  if (!is.null(r$meannohhm$data)) for (g in intersect(c("a00_04", "f15_49"), unique(r$meannohhm$data$group))) {
    d <- recent(r$meannohhm$data[group == g]); if (is.null(d)) next
    lbl <- hhm_group_defs[[g]]$label
    a <- agg(d, "mean_members", "households")
    team_wide(a, paste0("hhm_", g), -1, ratio = 0.75, min_diff = 0.15, min_n = 30, label = sprintf("members %s per household", lbl), fmt = f_dec, lib_key = "team_hhm", lib_arg = lbl)
    a[, tm := sum(v * n) / sum(n), by = team]
    for (i in which(a$n >= 10 & a$v < 0.7 * a$tm)) { h <- a[i]; ac <- la("hhm_omission", fmt_int(h$interv), lbl)
      add_sig("interviewer", paste0("hhm_", g), h$team, h$interv, dataset = h$dataset, severity = if (h$v < 0.5 * h$tm) "high" else "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("%.2f household members %s per household vs %.2f for team-mates (%d households, last %d weeks).", h$v, lbl, h$tm, h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = paste0("hhm_", g)) }
  }
  if (!is.null(d <- recent(r$everborn$data))) {
    a <- agg(d, "mean_ceb", "women"); a[, tm := sum(v * n) / sum(n), by = team]
    for (i in which(a$n >= 10 & a$v < 0.7 * a$tm)) { h <- a[i]; ac <- la("everborn_low", fmt_int(h$interv))
      add_sig("interviewer", "everborn_low", h$team, h$interv, dataset = h$dataset, severity = "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("%.1f births per woman vs %.1f for team-mates (%d women, last %d weeks).", h$v, h$tm, h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "births") }
  }
  if (!is.null(d <- recent(r$u5deaths$data))) {
    a <- agg(d, "pct_u5_deaths", "births"); av <- r$u5deaths$sections[[1]]$average
    for (i in which(a$n >= 30 & a$v == 0 & av >= 3)) { h <- a[i]; ac <- la("u5deaths_none", fmt_int(h$interv))
      add_sig("interviewer", "u5deaths_none", h$team, h$interv, dataset = h$dataset, severity = "medium",
              value = h$v, benchmark = av, evidence = sprintf("No under-5 death among %d births recorded (survey %s), last %d weeks.", h$n, pctf(av), recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "births") }
  }
  if (!is.null(d <- recent(r$ant_heap$data))) {
    a <- d[, .(v = sum(pct_heaping * measured) / sum(measured), n = sum(measured)), by = team]
    desc$heaping <- a
    mt <- r$ant_heap$measurers
    for (i in which(a$n >= 20 & a$v >= 30)) { h <- a[i]; ac <- la("heaping")
      ms_txt <- ""; who <- NA_character_
      if (!is.null(mt) && nrow(m <- mt[team == h$team & measured_period >= 10][order(-pct_heaping_period)])) {
        m <- head(m[pct_heaping_period >= 30], 3)
        if (nrow(m)) { ms_txt <- sprintf(" Measurer(s) (%s) with most heaping in the period of analysis: %s.", r$ant_heap$measurer_var,
                                         paste(sprintf("%s (%s of %d)", fmt_int(m$measurer), pctf(m$pct_heaping_period, 0), m$measured_period), collapse = ", "))
                       who <- paste(fmt_int(m$measurer), collapse = ", ")
                       ac$action <- sprintf("Retrain measurer(s) %s on reading the board to the last 0.1 cm (no rounding to .0/.5); repeat a standardisation exercise; supervisor checks a few of their measurements.", paste(fmt_int(m$measurer), collapse = ", ")) }
      }
      add_sig("team", "heaping", h$team, question = who, severity = if (h$v >= 50) "high" else "medium", value = h$v, benchmark = 20,
              evidence = sprintf("%s of heights end in .0 or .5 (%d children measured, last %d weeks); about 20%% expected.%s", pctf(h$v, 0), h$n, recent_weeks, ms_txt),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "anthropometry") }
  }
  if (!is.null(d <- recent(r$ant_oor$data))) {
    a <- d[, .(v = sum(pct_oor * measured) / sum(measured), n = sum(measured)), by = team]; av <- r$ant_oor$sections[[1]]$average
    desc$oor <- a
    for (i in which(a$n >= 20 & a$v >= max(2 * av, 5))) { h <- a[i]; ac <- la("anthro_oor")
      add_sig("team", "anthro_oor", h$team, severity = if (h$v >= max(4 * av, 10)) "high" else "medium", value = h$v, benchmark = av,
              evidence = sprintf("%s of children with out-of-range anthropometry vs %s survey-wide (%d measured, last %d weeks).", pctf(h$v, 0), pctf(av, 1), h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing, area = "anthropometry") }
  }
  if (!is.null(d <- recent(r$fl$data))) {
    for (k in intersect(names(fl_rules), unique(d$check))) { rl <- fl_rules[[k]]
      a <- agg(d[check == k], "value", "n")
      if (k == "minutes") team_wide(a, paste0("fl_", k), -1, ratio = 0.6, min_diff = 2, min_n = 20, label = rl$what, fmt = f_min, lib_key = "team_fl", lib_arg = rl$what, area = "fl")
      else team_wide(a, paste0("fl_", k), rl$dir, gap = rl$gap, min_n = 20, label = rl$what, fmt = f_pct, lib_key = "team_fl", lib_arg = rl$what, area = "fl")
    }
    if (!is.null(cc <- fl_concerns(d))) {
      desc$fl <- cc
      for (i in seq_len(nrow(cc))) { h <- cc[i]; rl <- fl_rules[[h$check]]; ac <- la("fl_assessment", fmt_int(h$interv))
        ev <- if (h$check == "minutes") sprintf("Median %.0f %s vs %.0f survey median (%d assessments, last %d weeks).", h$v, rl$what, h$bm, h$n, recent_weeks)
              else sprintf("%s %s vs %s for team-mates (%d children, last %d weeks).", pctf(h$v, 0), rl$what, pctf(h$bm, 0), h$n, recent_weeks)
        add_sig("interviewer", paste0("fl_", h$check), h$team, h$interv, dataset = h$dataset, module = "FL", severity = if (h$high) "high" else "medium",
                value = h$v, benchmark = h$bm, evidence = ev, action = ac$action, owner = ac$owner, timing = ac$timing, area = "fl") }
    }
  }

  signals <- if (length(sig)) rbindlist(sig) else
    data.table(level = character(), type = character(), area = character(), team = integer(), interv = numeric(), dataset = character(), module = character(),
               question = character(), severity = character(), value = numeric(), benchmark = numeric(), evidence = character(),
               action = character(), owner = character(), timing = character())
  signals <- group_signals(add_headlines(signals), lib)
  signals[, team_label := ifelse(is.na(team), NA_character_, tl(team))]
  signals[, sev_rank := match(severity, c("high", "medium"))]
  signals[, lev_rank := match(level, c("survey", "team", "interviewer"))]
  setorder(signals, lev_rank, sev_rank, team, interv); signals[, c("sev_rank", "lev_rank") := NULL]

  # --- priority per team (questionnaire problems do not count: they are not the team's)
  pts <- signals[!is.na(team) & level != "survey", .(points = sum(ifelse(severity == "high", 3, 1)), signals = .N, high = sum(severity == "high"),
                                                     team_flags = any(type == "team_flags")), by = team]
  teams <- merge(teams, pts, by = "team", all.x = TRUE)
  teams[is.na(points), `:=`(points = 0, signals = 0L, high = 0L, team_flags = FALSE)]
  teams[, priority := fifelse(team_flags | high >= 2, "High", fifelse(high >= 1 | signals >= 3, "Medium", "Low"))]
  teams[, c("high", "team_flags") := NULL]
  teams[, team_label := tl(team)]
  # without a flag analysis the flag columns are missing: add them empty so that tables and plans work on the descriptive signals alone
  for (k in c("tests", "flags", "interviewers_tested", "rate", "expected", "ratio", "p_excess", "rate_excl", "ratio_excl")) if (!k %in% names(teams)) teams[, (k) := NA_real_]
  if (!is.null(tm)) {
    top <- tm[flags >= 3 & rate >= 2 * module_rate][order(-excess)][, .(top_modules = paste(head(sprintf("%s %s", dataset, module), 3), collapse = "; ")), by = team]
    teams <- merge(teams, top, by = "team", all.x = TRUE)
  }
  if (!"top_modules" %in% names(teams)) teams[, top_modules := NA_character_]
  ic <- signals[level == "interviewer", .(interviewers_of_concern = paste(sort(unique(fmt_int(interv))), collapse = ", ")), by = team]
  teams <- merge(teams, ic, by = "team", all.x = TRUE)
  teams[, prio_rank := match(priority, c("High", "Medium", "Low"))]
  setorder(teams, prio_rank, -points); teams[, prio_rank := NULL]
  setcolorder(teams, c("team", "team_label", "priority", "points", "signals"))

  if (!is.null(iv)) {
    isig <- signals[level == "interviewer", .(signals = .N, signal_types = paste(unique(type), collapse = ", ")), by = .(team, interv)]
    iv <- merge(iv, isig, by = c("team", "interv"), all.x = TRUE)
    iv[is.na(signals), signals := 0L]; iv[, team_label := tl(team)]
    setorder(iv, -signals, -ratio)
  }
  structure(list(survey = survey, teams = teams, interviewers = iv, modules = ms, questions = qs, team_modules = tm,
                 signals = signals, descriptive = desc), class = "ipm_scorecard")
}

# Short, precise wording of each issue for the summary and the app: a label ("Whole team, short interviews"),
# a detail with the numbers ("ch 9 vs 15 min") - the issues of one area are joined as "label: detail; detail" -
# and a short action ("Observe 2 interviews with the supervisor"). The full evidence and action stay in the table.
add_headlines <- function(s) {
  if (!nrow(s)) return(s[, `:=`(label = character(), detail = character(), do = character())][])
  f0 <- function(x) sprintf("%.0f", x); f1 <- function(x) sprintf("%.1f", x); f2 <- function(x) sprintf("%.2f", x)
  ds <- function(d) ifelse(is.na(d), "", paste0(d, " "))
  flw <- function(type) { k <- sub("^(team_)?fl_", "", type); vapply(k, function(z) fl_rules[[z]]$what %||% z, "") }
  flu <- function(type) ifelse(grepl("minutes$", type), " min", "%")
  hhl <- function(type) vapply(sub("^(team_)?hhm_", "", type), function(g) hhm_group_defs[[g]]$label %||% g, "")
  s[, c("label", "detail", "do") := NA_character_]
  s[type == "question_survey_wide", `:=`(label = "Question flagged in most teams", detail = sprintf("%s %s in %g of %g teams", dataset, question, value, benchmark),
                                         do = sprintf("Check wording, translation and routing of %s %s; note to all teams", dataset, question))]
  s[type == "module_survey_wide", `:=`(label = "Module flagged in most teams", detail = sprintf("%s %s %s%% of tests", dataset, module, f1(value)),
                                       do = sprintf("Review module %s %s at the next refresher for all teams", dataset, module))]
  s[type == "team_flags", `:=`(label = "Whole team, excess of flags", detail = sprintf("%s%% vs %s%% of tests", f1(value), f1(benchmark)),
                               do = "Debrief the team with its flag tables")]
  s[type == "team_module", `:=`(label = sprintf("Module %s %s flagged far more than in other teams", dataset, module), detail = sprintf("%s%% vs %s%%", f0(value), f0(benchmark)),
                                do = sprintf("Refresher on module %s %s for the team", dataset, module))]
  s[type == "team_completion", `:=`(label = "Whole team, low completion", detail = sprintf("%s%s%% vs %s%%", ds(dataset), f0(value), f0(benchmark)),
                                    do = "Review result codes and call-backs with the supervisor")]
  s[type == "team_duration", `:=`(label = "Whole team, short interviews", detail = sprintf("%s%s vs %s min", ds(dataset), f0(value), f0(benchmark)),
                                  do = "Observe 2 interviews with the supervisor (module time stamps)")]
  s[type == "team_dk", `:=`(label = "Whole team, many 'don't know' answers", detail = sprintf("%s%s%% vs %s%%", ds(dataset), f1(value), f1(benchmark)),
                            do = "Observe how the team probes before recording 'don't know'")]
  s[grepl("^team_hhm_", type), `:=`(label = "Whole team, few household members listed", detail = sprintf("%s %s vs %s per household", hhl(type), f2(value), f2(benchmark)),
                                    do = "Re-check the listing in 2-3 households per interviewer")]
  s[grepl("^team_fl_", type), `:=`(label = "Whole team, FL assessment", detail = sprintf("%s %s%s vs %s%s", flw(type), f0(value), flu(type), f0(benchmark), flu(type)),
                                   do = "Observe 2 FL assessments with the supervisor")]
  s[type == "heaping", `:=`(label = "Height heaping", detail = sprintf("%s%% of heights on .0/.5 (about 20%% expected)%s", f0(value), ifelse(is.na(question), "", paste0(", measurers ", question))),
                            do = "Retrain the measurers; standardisation exercise")]
  s[type == "anthro_oor", `:=`(label = "Out-of-range anthropometry", detail = sprintf("%s%% vs %s%% of children", f0(value), f1(benchmark)),
                               do = "Call-backs from the anthropometry listing")]
  s[type == "interviewer_flags", `:=`(label = "Many flags", detail = sprintf("%s%% vs %s%% of questions", f1(value), f1(benchmark)),
                                      do = sprintf("Observe %s; re-ask flagged questions in 2-3 households", fmt_int(interv)))]
  s[type == "low_completion", `:=`(label = "Low completion", detail = sprintf("%s%s%% vs %s%% for the team", ds(dataset), f0(value), f0(benchmark)),
                                   do = sprintf("Review result codes and call-backs of %s", fmt_int(interv)))]
  s[type == "short_interviews", `:=`(label = "Short interviews", detail = sprintf("%s%s vs %s min (survey)", ds(dataset), f0(value), f0(benchmark)),
                                     do = sprintf("Observe %s during a full interview", fmt_int(interv)))]
  s[grepl("^hhm_", type), `:=`(label = "Few household members listed", detail = sprintf("%s %s vs %s for team-mates", hhl(type), f2(value), f2(benchmark)),
                               do = sprintf("Re-check the household listing of %s", fmt_int(interv)))]
  s[type == "everborn_low", `:=`(label = "Birth history", detail = sprintf("%s vs %s births per woman", f1(value), f1(benchmark)),
                                 do = sprintf("Review birth histories of %s; retrain on probing", fmt_int(interv)))]
  s[type == "u5deaths_none", `:=`(label = "Birth history", detail = sprintf("no under-5 death recorded (survey %s%%)", f1(benchmark)),
                                  do = sprintf("Review birth histories of %s; retrain on probing", fmt_int(interv)))]
  s[grepl("^fl_", type), `:=`(label = "FL assessment", detail = sprintf("%s %s%s vs %s%s", flw(type), f0(value), flu(type), f0(benchmark), flu(type)),
                              do = sprintf("Observe %s giving the FL assessment", fmt_int(interv)))]
  s[is.na(label), `:=`(label = gsub("_", " ", type), detail = "", do = owner)]
  s
}

# One issue per interviewer (or team) and area: e.g. the 6 FL checks of one interviewer, or short interviews in hh
# and wm, become one issue listing the evidence of each check; and no interviewer issue when the whole team has
# the same issue in the same questionnaire (it is then the team's issue, listed once).
group_signals <- function(s, lib) {
  if (!nrow(s)) return(s[, headline := character()][, c("label", "detail") := NULL][])
  tw <- unique(s[level == "team" & area %in% c("completion", "duration", "fl") | level == "team" & grepl("^hhm_", area), .(team, area, dataset)])
  if (nrow(tw)) {
    drop <- s[, .I[level == "interviewer"]][s[level == "interviewer"][tw, on = .(team, area, dataset), nomatch = NULL, which = TRUE]]
    if (length(drop)) s <- s[-unique(drop)]
  }
  s[, akey := sub("^hhm_.*", "hhm", area)]
  s[, sev_rank := match(severity, c("high", "medium"))]
  setorder(s, sev_rank)
  key_type <- c(flags = "interviewer_flags", completion = "low_completion", duration = "short_interviews", hhm = "hhm_omission",
                births = "birth_history", fl = "fl_assessment")
  g <- s[level != "survey", {
    ds <- unique(stats::na.omit(dataset))
    list(type = if (.N > 1 && level[1] == "interviewer" && !is.null(key_type[akey[1]]) && !is.na(key_type[akey[1]])) unname(key_type[akey[1]]) else type[1],
         dataset = if (length(ds)) paste(ds, collapse = ", ") else NA_character_,
         module = module[1], question = question[1], severity = severity[1], value = value[1], benchmark = benchmark[1],
         evidence = paste(unique(evidence), collapse = " "), action = action[1], owner = owner[1], timing = timing[1],
         headline = paste0(label[1], ": ", paste(unique(detail), collapse = "; ")), do = do[1])
  }, by = .(level, team, interv, area = akey)]
  sv <- s[level == "survey"][, headline := paste0(label, ": ", detail)]
  out <- rbind(sv[, !c("akey", "sev_rank", "label", "detail"), with = FALSE], g, fill = TRUE)
  setcolorder(out, c("level", "type", "area", "team", "interv", "dataset", "module", "question", "severity", "headline", "do", "value", "benchmark", "evidence", "action", "owner", "timing"))
  out
}

#' Rule-based team action plan (no AI)
#'
#' Turns the scorecard signals into a plan with the same structure as the AI plan, using the
#' action library. Always available, and used as fallback when no AI provider is configured.
#' @param sc result of ipm_scorecard()
#' @param max_teams maximum number of teams to include (highest priority first)
#' @export
ipm_plan_rules <- function(sc, max_teams = 50) {
  s <- sc$survey; sig <- sc$signals
  overall <- if (!is.null(s$flag_rate))
    sprintf("%s: %s flag tests, %s flagged (nominal level %s). %d of %d teams are high priority and %d medium priority. %d survey-wide question or module problems were detected.",
            s$period, format(s$tests, big.mark = ","), pctf(s$flag_rate), pctf(s$levelsig * 100),
            sum(sc$teams$priority == "High"), nrow(sc$teams), sum(sc$teams$priority == "Medium"), nrow(sig[level == "survey"]))
    else "No flag analysis available; recommendations are based on the descriptive outputs only."
  sw <- sig[level == "survey"]
  # one issue per module: modules flagged survey-wide, plus questions flagged in many teams grouped by module
  survey_issues <- list()
  if (nrow(sw)) {
    sw[, key := paste(dataset, module)]
    for (k in unique(sw$key)) {
      x <- sw[key == k]; mrow <- x[type == "module_survey_wide"]; qrow <- x[type == "question_survey_wide"]
      ev <- c(if (nrow(mrow)) mrow$evidence[1],
              if (nrow(qrow)) sprintf("Questions flagged in at least half of the teams: %s.",
                                      paste(sprintf("%s (%g of %g teams)", qrow$question, qrow$value, qrow$benchmark), collapse = ", ")))
      survey_issues[[length(survey_issues) + 1]] <- list(
        issue = sprintf("Module %s%s", k, if (nrow(qrow)) sprintf(" - %d question(s) flagged in many teams", nrow(qrow)) else ""),
        evidence = paste(ev, collapse = " "),
        action = if (nrow(qrow)) sprintf("Check the wording, translation and CAPI routing of %s; send a clarification note to all teams and cover the module in the next field call/refresher.",
                                         paste(head(qrow$question, 6), collapse = ", ")) else mrow$action[1],
        owner = "Survey coordinator", .n = nrow(qrow) + 2 * nrow(mrow))
    }
    survey_issues <- head(survey_issues[order(-vapply(survey_issues, `[[`, 0, ".n"))], 10)
    survey_issues <- lapply(survey_issues, function(z) { z$.n <- NULL; z })
  }
  mods <- if (!is.null(sc$modules)) head(sc$modules[tests >= 30 & ratio >= 2], 5) else NULL
  mod_list <- if (!is.null(mods) && nrow(mods)) lapply(seq_len(nrow(mods)), function(i) list(dataset = mods$dataset[i], module = mods$module[i],
    why = sprintf("%s of tests flagged (%.1f times the survey rate); %d of %d teams affected (%s).", pctf(mods$rate[i]), mods$ratio[i], mods$teams_flagged[i], mods$teams[i], mods$scope[i]),
    action = if (mods$scope[i] == "survey-wide") "Review wording/translation and retrain all teams on this module." else "Targeted refresher for the affected teams.")) else list()
  tt <- head(sc$teams[priority != "Low"], max_teams)
  lib <- ipm_action_library()
  teams <- lapply(seq_len(nrow(tt)), function(i) {
    h <- tt[i]; ts <- sig[team == h$team]
    ts[, sev_rank := match(severity, c("high", "medium"))]; setorder(ts, sev_rank)
    acts <- list()
    tmods <- ts[type == "team_module"]
    if (nrow(tmods)) acts[[length(acts) + 1]] <- list(action = sprintf(lib$team_module$action, paste(head(paste(tmods$dataset, tmods$module), 5), collapse = ", ")),
                                                       owner = lib$team_module$owner, timing = lib$team_module$timing)
    other <- unique(ts[level == "team" & type != "team_module", .(action, owner, timing)])
    for (j in seq_len(nrow(other))) acts[[length(acts) + 1]] <- as.list(other[j])
    ivs <- if (any(ts$level == "interviewer")) ts[level == "interviewer", .(reason = paste(evidence, collapse = " "), sev = min(sev_rank)), by = interv][order(sev)] else data.table(interv = numeric(), reason = character())
    if (nrow(ivs)) acts[[length(acts) + 1]] <- list(
      action = sprintf("Observe interviewer(s) %s during full interviews and spot-check 2-3 of their households on the flagged questions; give one-to-one feedback.", paste(fmt_int(ivs$interv), collapse = ", ")),
      owner = "Supervisor", timing = "Immediately")
    mods <- if (!is.null(sc$team_modules)) head(sc$team_modules[team == h$team & flags >= 3 & rate >= 2 * module_rate][order(-excess), sprintf("%s %s", dataset, module)], 5) else character()
    list(team = h$team_label, priority = h$priority,
         headline = sprintf("%d warning sign(s), %d of high severity%s.", nrow(ts), sum(ts$severity == "high"),
                            if (!is.na(h$rate)) sprintf("; %s of tests flagged (survey %s)", pctf(h$rate), pctf(s$flag_rate)) else ""),
         issues = lapply(seq_len(min(nrow(ts), 8)), function(j) list(issue = if ("headline" %in% names(ts)) ts$headline[j] else gsub("_", " ", ts$type[j]), evidence = ts$evidence[j])),
         actions = acts,
         interviewers_to_observe = lapply(seq_len(nrow(ivs)), function(j) list(interviewer = fmt_int(ivs$interv[j]), reason = ivs$reason[j])),
         modules_to_retrain = as.list(mods))
  })
  list(source = "rules", model = "IPM rule library", generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
       overall_assessment = overall, survey_wide_issues = survey_issues, most_problematic_modules = mod_list, teams = teams,
       caveats = list("Signals are statistical, not proof of error or misconduct: confirm with the flag tables, observation or re-interviews before acting.",
                      "Teams with few interviews in the period give unstable results; re-run every 1-2 weeks and act on patterns that persist."))
}
