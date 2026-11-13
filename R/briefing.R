# Briefing: the app's home page - what to act on this week, built from the one list of issues (scorecard) and its
# comparison with the last run of the same survey (history.R).

#' Contents of the briefing page
#' @param sc result of [ipm_scorecard()]
#' @param cmp result of [ipm_compare()] or [ipm_history_update()]; NULL when there is no history
#' @return a list: `kpi` (now and at the last run), `teams` (one item per High/Medium team), `interviewers`
#'   (one item per interviewer with a high-severity issue), `questionnaire`, `resolved`, `grid` (team x area) and `previous`
#' @export
ipm_briefing <- function(sc, cmp = NULL) {
  if (is.null(cmp)) cmp <- ipm_compare(sc, list(), as.Date(NA))
  s <- copy(cmp$issues); tm <- copy(cmp$teams); prev <- cmp$previous; hist <- !is.null(prev)
  s[, sev := match(severity, c("high", "medium"))]
  s[, harea := health_area(area)]
  status_txt <- function(runs, since) if (!hist) NA_character_ else if (max(runs) <= 1L) "new" else
    sprintf("%s run in a row (since %s)", ordinal(max(runs)), format(min(since[runs == max(runs)]), "%d %b"))

  # one item per team to act on, most urgent first
  act <- tm[priority != "Low"]
  teams <- rbindlist(lapply(seq_len(nrow(act)), function(i) { h <- act[i]
    ts <- s[level == "team" & team == h$team][order(sev)]; iv <- s[level == "interviewer" & team == h$team][order(sev)]
    first <- if (nrow(ts)) ts[1] else iv[1]; all <- rbind(ts, iv)
    data.table(team = h$team, team_label = h$team_label, priority = h$priority, prev_priority = h$prev_priority %||% NA_character_,
               headline = if (nrow(ts)) ts$headline[1] else sprintf("%d interviewer issue(s)", nrow(iv)), severity = first$severity,
               more = max(0L, nrow(ts) - 1L), interviewers = paste(unique(fmt_int(iv$interv)), collapse = ", "),
               do = first$do, owner = first$owner, timing = first$timing,
               status = if (nrow(all)) status_txt(all$runs, all$since) else NA_character_,
               persisting = hist && nrow(all) > 0 && any(all$runs > 1L))
  }))
  if (!nrow(teams)) teams <- data.table(team = integer(), team_label = character(), priority = character(), prev_priority = character(), headline = character(),
                                        severity = character(), more = integer(), interviewers = character(), do = character(), owner = character(),
                                        timing = character(), status = character(), persisting = logical())

  # interviewers with a high-severity issue: one item each, all their issues
  hi <- s[level == "interviewer" & severity == "high", unique(interv)]
  ivs <- s[level == "interviewer" & interv %in% hi][order(sev)]
  interviewers <- if (nrow(ivs)) ivs[, .(team = team[1], team_label = team_label[1],
                                         headline = paste0(headline, ifelse(severity == "high", "", " (medium)"), collapse = ". "),
                                         do = do[1], owner = owner[1], timing = timing[1], status = status_txt(runs, since)), by = interv] else
    data.table(interv = numeric(), team = integer(), team_label = character(), headline = character(), do = character(), owner = character(), timing = character(), status = character())

  q <- s[level == "survey"]
  # team health: worst severity of each team in each area (questionnaire problems are not the team's)
  g <- s[level != "survey" & !is.na(harea), .(sev = min(sev), n = .N, persisting = hist && any(runs > 1L), what = paste(unique(headline), collapse = "\n")), by = .(team, harea)]
  grid <- CJ(team = tm$team, harea = names(health_areas), sorted = FALSE)
  grid <- merge(grid, g, by = c("team", "harea"), all.x = TRUE, sort = FALSE)
  grid[, `:=`(team_label = tm$team_label[match(team, tm$team)], priority = tm$priority[match(team, tm$team)], area_label = unname(health_areas[harea]))]
  grid[, ord := match(team, tm$team)]; setorder(grid, ord); grid[, ord := NULL]

  n_iv <- function(x) if (is.null(x) || !nrow(x)) 0L else uniqueN(x[level == "interviewer", interv])
  kpi <- data.table(what = c("teams", "interviewers", "questionnaire", "resolved"),
                    label = c("Teams to act on", "Interviewers to observe", "Questionnaire problems", "Resolved since last run"),
                    now = c(sum(tm$priority != "Low"), length(hi), nrow(q), nrow(cmp$resolved)),
                    before = if (hist) c(sum(prev$teams$priority != "Low"), uniqueN(prev$issues[level == "interviewer" & severity == "high", interv]),
                                         nrow(prev$issues[level == "survey"]), NA) else NA_real_)
  list(kpi = kpi, teams = teams, interviewers = interviewers, questionnaire = q, resolved = cmp$resolved, grid = grid,
       previous = prev, runs = cmp$runs, survey = sc$survey)
}

ordinal <- function(n) paste0(n, ifelse(n %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[n %% 10 + 1]))
