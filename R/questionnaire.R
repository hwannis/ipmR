#' Questionnaire heat map: questions (or modules) x teams
#'
#' Share of the flag tests of each question (or module) that are flagged in each team, for the questions with the most
#' flags. A question flagged in most teams is a questionnaire problem (wording, translation, CAPI routing); one flagged in a
#' single team is the team's.
#' @param res result of `run_ipm()`
#' @param sc result of `ipm_scorecard()`
#' @param by "question" or "module"
#' @param top number of rows (the questions or modules with the most flags)
#' @param hide_survey_wide leave out the questions (modules) flagged in most teams, to see the team problems
#' @return a list: `rows` (one per question or module: dataset, module, question, description, tests, flags, rate,
#'   teams_flagged, teams, survey_wide), `cells` (row x team: tests, flags, rate, interviewers flagged) and `teams`
#'   (team, team_label, priority), or NULL without a flag analysis
#' @export
ipm_question_heat <- function(res, sc, by = c("question", "module"), top = 40, hide_survey_wide = FALSE) {
  by <- match.arg(by)
  ft <- res$outputs$flaganal$tests
  if (is.null(ft) || !nrow(ft)) return(NULL)
  t <- copy(ft)[!is.na(team)][, mod := sub("\\*$", "", module)]
  t[, row := if (by == "question") paste(dataset, varname) else paste(dataset, mod)]
  rows <- t[, .(dataset = dataset[1], module = mod[1], question = if (by == "question") varname[1] else NA_character_,
                description = if (by == "question") vardesc[1] else NA_character_, tests = .N, flags = sum(flag),
                teams = uniqueN(team), teams_flagged = uniqueN(team[flag == 1])), by = row]
  rows[, rate := flags / tests * 100]
  sw <- if (by == "question" && !is.null(sc$questions)) sc$questions[survey_wide == TRUE, paste(dataset, question)]
        else if (!is.null(sc$modules)) sc$modules[scope == "survey-wide", paste(dataset, module)] else character()
  rows[, survey_wide := row %in% sw]
  if (hide_survey_wide) rows <- rows[survey_wide == FALSE]
  rows <- rows[flags > 0][order(-flags, -rate)][seq_len(min(.N, top))]
  cells <- t[row %in% rows$row, .(tests = .N, flags = sum(flag), interviewers = paste(sort(unique(fmt_int(interv[flag == 1]))), collapse = ", ")), by = .(row, team)]
  cells[, rate := flags / tests * 100]
  tm <- sc$teams[, .(team, team_label, priority)][team %in% unique(t$team)][order(team)]
  list(rows = rows, cells = cells, teams = tm, by = by)
}
