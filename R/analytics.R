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
    action = "Use the anthropometry Excel listing for call-backs and re-measurement; check units and decimal entry, child age and measurement position (lying/standing).")
)

#' Team and interviewer scorecard
#'
#' Computes, from an IPM result, one row per team and per interviewer with the flag-analysis indicators
#' (flags vs. the number expected at the survey rate, binomial screening p-value) and the descriptive
#' warning signs of the most recent weeks, a long table of signals, and a priority level per team.
#' This is the evidence base used by the rule-based and AI recommendations.
#' @param res result of run_ipm()
#' @param recent_weeks number of most recent weeks used for the descriptive signals
#' @return list(survey, teams, interviewers, modules, questions, team_modules, signals)
#' @export
ipm_scorecard <- function(res, recent_weeks = 3) {
  o <- res$cfg$options; ti <- as.data.table(res$team_info %||% res$prepared$team_info)
  tl <- function(t) { l <- ti$team_label[match(t, ti$team)]; ifelse(is.na(l), paste0("T", t), l) }
  lib <- ipm_action_library()
  sig <- list()
  add_sig <- function(level, type, team, interv = NA_real_, dataset = NA_character_, module = NA_character_, question = NA_character_,
                      severity, value = NA_real_, benchmark = NA_real_, evidence, action = NA_character_, owner = NA_character_, timing = NA_character_)
    sig[[length(sig) + 1L]] <<- data.table(level, type, team = as.integer(team), interv = as.numeric(interv), dataset, module, question,
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
    survey <- c(survey, list(tests = nrow(t), flags = sum(t$flag), flag_rate = R * 100, ratio_to_chance = R / o$levelsig))

    ts <- t[, .(tests = .N, flags = sum(flag), interviewers_tested = uniqueN(interv)), by = team]
    ts[, `:=`(rate = flags / tests * 100, expected = tests * R, ratio = (flags / tests) / R,
              p_excess = stats::pbinom(flags - 1, tests, R, lower.tail = FALSE))]
    teams <- merge(teams, ts, by = "team", all.x = TRUE)

    iv <- t[, .(tests = .N, flags = sum(flag)), by = .(team, interv)]
    iv[, `:=`(rate = flags / tests * 100, ratio = (flags / tests) / R, p_excess = stats::pbinom(flags - 1, tests, R, lower.tail = FALSE))]
    iv[, team_rate := ts$rate[match(team, ts$team)]]

    ms <- t[, .(tests = .N, flags = sum(flag), teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1]),
                interviewers_flagged = uniqueN(interv[flag == 1])), by = .(dataset, module = mod)]
    ms[, `:=`(rate = flags / tests * 100, ratio = (flags / tests) / R)]
    ms[, scope := fifelse(teams_flagged >= 3 & teams_flagged / teams >= 0.5, "survey-wide", "concentrated")]
    setorder(ms, -rate)

    qs <- t[, .(description = vardesc[1], tests = .N, flags = sum(flag), teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1]),
                interviewers_flagged = uniqueN(interv[flag == 1])), by = .(dataset, module = mod, question = varname)]
    qs[, rate := flags / tests * 100]; setorder(qs, -flags, -rate)

    tm <- t[, .(tests = .N, flags = sum(flag), interviewers_flagged = paste(sort(unique(interv[flag == 1])), collapse = ", ")), by = .(team, dataset, module = mod)]
    tm <- merge(tm, ms[, .(dataset, module, module_rate = rate)], by = c("dataset", "module"))
    tm[, `:=`(rate = flags / tests * 100, excess = flags - tests * module_rate / 100,
              p_excess = stats::pbinom(flags - 1, tests, pmax(module_rate, R * 100) / 100, lower.tail = FALSE))]
    setorder(tm, team, -excess)

    # --- flag signals
    for (i in which(teams$flags >= 5 & teams$ratio >= 1.5 & teams$p_excess < 0.01)) { h <- teams[i]
      a <- la("team_flags")
      add_sig("team", "team_flags", h$team, severity = if (h$ratio >= 2.5) "high" else "medium", value = h$rate, benchmark = R * 100,
              evidence = sprintf("%s of tests flagged (%d of %d) vs %s survey-wide: %.1f times the survey rate (%.0f flags expected).",
                                 pctf(h$rate), h$flags, h$tests, pctf(R * 100), h$ratio, h$expected),
              action = a$action, owner = a$owner, timing = a$timing) }
    for (i in which(tm$flags >= 4 & tm$rate >= 3 * tm$module_rate & tm$rate >= 2 * R * 100 & tm$p_excess < 0.01)) { h <- tm[i]
      a <- la("team_module", paste(h$dataset, h$module))
      add_sig("team", "team_module", h$team, dataset = h$dataset, module = h$module,
              severity = if (h$flags >= 6 && h$rate >= 4 * h$module_rate) "high" else "medium", value = h$rate, benchmark = h$module_rate,
              evidence = sprintf("Module %s %s: %s of the team's tests flagged (%d of %d) vs %s for this module survey-wide; flagged interviewer(s): %s.",
                                 h$dataset, h$module, pctf(h$rate), h$flags, h$tests, pctf(h$module_rate), h$interviewers_flagged),
              action = a$action, owner = a$owner, timing = a$timing) }
    for (i in which(iv$flags >= 5 & iv$ratio >= 2 & iv$p_excess < 0.01)) { h <- iv[i]
      top <- t[interv == h$interv & flag == 1, .N, by = .(dataset, mod)][order(-N)][seq_len(min(.N, 3))]
      a <- la("interviewer_flags", fmt_int(h$interv))
      add_sig("interviewer", "interviewer_flags", h$team, h$interv, severity = if (h$ratio >= 4) "high" else "medium", value = h$rate, benchmark = R * 100,
              evidence = sprintf("Interviewer %s: %s of questions flagged (%d of %d) vs %s survey-wide and %s for the team; main modules: %s.",
                                 fmt_int(h$interv), pctf(h$rate), h$flags, h$tests, pctf(R * 100), pctf(h$team_rate),
                                 paste(sprintf("%s %s (%d)", top$dataset, top$mod, top$N), collapse = ", ")),
              action = a$action, owner = a$owner, timing = a$timing) }
    for (i in which(qs$teams_flagged >= 3 & qs$teams_flagged / qs$teams >= 0.5)) { h <- qs[i]
      a <- la("question_survey_wide", paste(h$dataset, h$question))
      add_sig("survey", "question_survey_wide", NA, dataset = h$dataset, module = h$module, question = h$question, severity = "high",
              value = h$teams_flagged, benchmark = h$teams,
              evidence = sprintf("Question %s %s ('%s') flagged in %d of %d teams (%d interviewers).", h$dataset, h$question, substr(h$description, 1, 70),
                                 h$teams_flagged, h$teams, h$interviewers_flagged),
              action = a$action, owner = a$owner, timing = a$timing) }
    for (i in which(ms$scope == "survey-wide" & ms$ratio >= 2 & ms$tests >= 30)) { h <- ms[i]
      a <- la("module_survey_wide", paste(h$dataset, h$module))
      add_sig("survey", "module_survey_wide", NA, dataset = h$dataset, module = h$module, severity = if (h$ratio >= 4) "high" else "medium",
              value = h$rate, benchmark = R * 100,
              evidence = sprintf("Module %s %s: %s of tests flagged (%.1f times the survey rate), %d of %d teams affected.",
                                 h$dataset, h$module, pctf(h$rate), h$ratio, h$teams_flagged, h$teams),
              action = a$action, owner = a$owner, timing = a$timing) }
  }

  # --- descriptive signals (most recent weeks)
  r <- res$outputs
  recent <- function(d) { if (is.null(d) || !nrow(d)) return(NULL); d <- copy(d)
    if ("dataset" %in% names(d)) d[, wmax := max(woi, na.rm = TRUE), by = dataset] else d[, wmax := max(woi, na.rm = TRUE)]
    d[!is.na(woi) & woi > wmax - recent_weeks] }
  agg <- function(d, vcol, ncol, by = c("dataset", "team", "interv")) { d <- copy(d); d[, `:=`(.v = d[[vcol]], .n = d[[ncol]])]
    d[, .(v = sum(.v * .n) / sum(.n), n = sum(.n)), by = by] }
  desc <- list()
  if (!is.null(d <- recent(r$quescrates$data))) {
    a <- agg(d, "completion_rate", "n"); a[, tm := sum(v * n) / sum(n), by = .(dataset, team)]
    desc$completion <- a
    for (i in which(a$n >= 10 & a$v < a$tm - 15)) { h <- a[i]; ac <- la("low_completion", fmt_int(h$interv))
      add_sig("interviewer", "low_completion", h$team, h$interv, dataset = h$dataset, severity = if (h$v < h$tm - 30) "high" else "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("Completion rate %s (%s questionnaires) vs %s for the team, last %d weeks.", pctf(h$v, 0), h$dataset, pctf(h$tm, 0), recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(d <- recent(r$quesmtimes$data))) {
    a <- agg(d, "median_minutes", "n"); a[, sm := stats::median(v), by = dataset]
    desc$duration <- a
    for (i in which(a$n >= 5 & a$v < 0.6 * a$sm)) { h <- a[i]; ac <- la("short_interviews", fmt_int(h$interv))
      add_sig("interviewer", "short_interviews", h$team, h$interv, dataset = h$dataset, severity = if (h$v < 0.4 * h$sm) "high" else "medium",
              value = h$v, benchmark = h$sm, evidence = sprintf("Median %.0f minutes per %s questionnaire vs %.0f survey median, last %d weeks.", h$v, h$dataset, h$sm, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(r$meannohhm$data)) for (g in intersect(c("a00_04", "f15_49"), unique(r$meannohhm$data$group))) {
    d <- recent(r$meannohhm$data[group == g]); if (is.null(d)) next
    a <- agg(d, "mean_members", "households"); a[, tm := sum(v * n) / sum(n), by = team]
    lbl <- hhm_group_defs[[g]]$label
    for (i in which(a$n >= 10 & a$v < 0.7 * a$tm)) { h <- a[i]; ac <- la("hhm_omission", fmt_int(h$interv), lbl)
      add_sig("interviewer", paste0("hhm_", g), h$team, h$interv, dataset = h$dataset, severity = if (h$v < 0.5 * h$tm) "high" else "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("%.2f household members %s per household vs %.2f for team-mates (%d households, last %d weeks).", h$v, lbl, h$tm, h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(d <- recent(r$everborn$data))) {
    a <- agg(d, "mean_ceb", "women"); a[, tm := sum(v * n) / sum(n), by = team]
    for (i in which(a$n >= 10 & a$v < 0.7 * a$tm)) { h <- a[i]; ac <- la("everborn_low", fmt_int(h$interv))
      add_sig("interviewer", "everborn_low", h$team, h$interv, dataset = h$dataset, severity = "medium",
              value = h$v, benchmark = h$tm, evidence = sprintf("%.1f births per woman vs %.1f for team-mates (%d women, last %d weeks).", h$v, h$tm, h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(d <- recent(r$u5deaths$data))) {
    a <- agg(d, "pct_u5_deaths", "births"); av <- r$u5deaths$sections[[1]]$average
    for (i in which(a$n >= 30 & a$v == 0 & av >= 3)) { h <- a[i]; ac <- la("u5deaths_none", fmt_int(h$interv))
      add_sig("interviewer", "u5deaths_none", h$team, h$interv, dataset = h$dataset, severity = "medium",
              value = h$v, benchmark = av, evidence = sprintf("No under-5 death among %d births recorded (survey %s), last %d weeks.", h$n, pctf(av), recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(d <- recent(r$ant_heap$data))) {
    a <- d[, .(v = sum(pct_heaping * measured) / sum(measured), n = sum(measured)), by = team]
    desc$heaping <- a
    for (i in which(a$n >= 20 & a$v >= 30)) { h <- a[i]; ac <- la("heaping")
      add_sig("team", "heaping", h$team, severity = if (h$v >= 50) "high" else "medium", value = h$v, benchmark = 20,
              evidence = sprintf("%s of heights end in .0 or .5 (%d children measured, last %d weeks); about 20%% expected.", pctf(h$v, 0), h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }
  if (!is.null(d <- recent(r$ant_oor$data))) {
    a <- d[, .(v = sum(pct_oor * measured) / sum(measured), n = sum(measured)), by = team]; av <- r$ant_oor$sections[[1]]$average
    desc$oor <- a
    for (i in which(a$n >= 20 & a$v >= max(2 * av, 5))) { h <- a[i]; ac <- la("anthro_oor")
      add_sig("team", "anthro_oor", h$team, severity = if (h$v >= max(4 * av, 10)) "high" else "medium", value = h$v, benchmark = av,
              evidence = sprintf("%s of children with out-of-range anthropometry vs %s survey-wide (%d measured, last %d weeks).", pctf(h$v, 0), pctf(av, 1), h$n, recent_weeks),
              action = ac$action, owner = ac$owner, timing = ac$timing) }
  }

  signals <- if (length(sig)) rbindlist(sig) else
    data.table(level = character(), type = character(), team = integer(), interv = numeric(), dataset = character(), module = character(),
               question = character(), severity = character(), value = numeric(), benchmark = numeric(), evidence = character(),
               action = character(), owner = character(), timing = character())
  signals[, team_label := ifelse(is.na(team), NA_character_, tl(team))]
  signals[, sev_rank := match(severity, c("high", "medium"))]
  setorder(signals, sev_rank, team, interv); signals[, sev_rank := NULL]

  # --- priority per team
  pts <- signals[!is.na(team), .(points = sum(ifelse(severity == "high", 3, 1)), signals = .N,
                                 high_team = any(type == "team_flags")), by = team]
  teams <- merge(teams, pts, by = "team", all.x = TRUE)
  teams[is.na(points), `:=`(points = 0, signals = 0L, high_team = FALSE)]
  # High: the team as a whole has an excess of flags, or many/severe signals; Medium: at least one severe or several minor signals
  teams[, priority := fifelse(points >= 9 | high_team, "High", fifelse(points >= 3, "Medium", "Low"))]
  teams[, high_team := NULL]
  teams[, team_label := tl(team)]
  if (!is.null(tm)) {
    top <- tm[flags >= 3 & rate >= 2 * module_rate][order(-excess)][, .(top_modules = paste(head(sprintf("%s %s", dataset, module), 3), collapse = "; ")), by = team]
    teams <- merge(teams, top, by = "team", all.x = TRUE)
  }
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
    sprintf("%s: %s flag tests, %s flagged (%.1f times the %s expected by chance). %d of %d teams are high priority and %d medium priority. %d survey-wide question or module problems were detected.",
            s$period, format(s$tests, big.mark = ","), pctf(s$flag_rate), s$ratio_to_chance, pctf(s$levelsig * 100),
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
         issues = lapply(seq_len(min(nrow(ts), 8)), function(j) list(issue = gsub("_", " ", ts$type[j]), evidence = ts$evidence[j])),
         actions = acts,
         interviewers_to_observe = lapply(seq_len(nrow(ivs)), function(j) list(interviewer = fmt_int(ivs$interv[j]), reason = ivs$reason[j])),
         modules_to_retrain = as.list(mods))
  })
  list(source = "rules", model = "IPM rule library", generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
       overall_assessment = overall, survey_wide_issues = survey_issues, most_problematic_modules = mod_list, teams = teams,
       caveats = list("Signals are statistical, not proof of error or misconduct: confirm with the flag tables, observation or re-interviews before acting.",
                      "Teams with few interviews in the period give unstable results; re-run every 1-2 weeks and act on patterns that persist."))
}
