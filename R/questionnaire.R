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

# heat colour of a % flagged (same scale as the Questionnaire page: 0 grey, 40%+ dark red)
q_heat_col <- function(r) vapply(r, function(x) { if (is.na(x)) return(NA_character_); if (x == 0) return("#EEF0F3")
  grDevices::rgb(grDevices::colorRamp(c("#FDE3C8", "#F28C28", "#B3140F"))(min(x / 40, 1)), maxColorValue = 255) }, "")
q_short <- function(x) sub(" \\([^()]*\\)$", "", x)
q_problems <- function(sc) { s <- sc$signals[level == "survey"]
  if (nrow(s)) s[, what := if ("headline" %in% names(s)) fifelse(is.na(headline), evidence, headline) else evidence]; s }
q_sub <- function(res, sc) sprintf("%s - period of analysis %s - survey flag rate %.1f%% of tests - ipmR %s, %s",
                                   res$cfg$survey_name %||% sc$survey$name %||% "", sc$survey$period %||% "-", sc$survey$flag_rate %||% NA,
                                   IPMR_VERSION, format(Sys.Date(), "%d %B %Y"))

# one heat table (questions or modules) in wide form: one column per team, % of the team's tests flagged
q_wide <- function(h) {
  r <- h$rows; tm <- h$teams; ce <- h$cells
  w <- data.table(Dataset = r$dataset, Module = r$module)
  if (h$by == "question") w[, `:=`(Question = r$question, Description = r$description)]
  w[, `:=`(`Flagged in most teams` = ifelse(r$survey_wide, "yes", ""), Tests = r$tests, Flags = r$flags, `% flagged` = round(r$rate, 1),
           `Teams flagged` = sprintf("%d of %d", r$teams_flagged, r$teams))]
  for (j in seq_len(nrow(tm))) { x <- ce[team == tm$team[j]]
    set(w, j = q_short(tm$team_label[j]), value = round(x$rate[match(r$row, x$row)], 0)) }
  w
}

#' Question and module flag summaries in an Excel workbook
#'
#' The tables of the Questionnaire page, to share: every flagged question and every flagged module with the % of tests
#' flagged in each team (coloured as in the app), one row per flagged question and team with the interviewers flagged,
#' and the questionnaire problems with their action. Codes and figures only, no records.
#' @param res result of `run_ipm()`
#' @param sc result of `ipm_scorecard()`
#' @param file the .xlsx file to write
#' @return the file name, invisibly (NULL without a flag analysis)
#' @export
ipm_question_xlsx <- function(res, sc, file) {
  hq <- ipm_question_heat(res, sc, "question", top = Inf); hm <- ipm_question_heat(res, sc, "module", top = Inf)
  if (is.null(hq)) return(invisible(NULL))
  wb <- openxlsx::createWorkbook(); sub <- q_sub(res, sc)
  st_t <- openxlsx::createStyle(fontSize = 13, textDecoration = "bold"); st_s <- openxlsx::createStyle(fontSize = 10, textDecoration = "italic")
  st_h <- openxlsx::createStyle(fontSize = 9, textDecoration = "bold", wrapText = TRUE, halign = "center", valign = "center", fgFill = "#DCEFF8",
                                border = "TopBottomLeftRight", borderColour = "#B6B7B8")
  head <- function(sh, title, note = NULL) { openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, title, startRow = 1); openxlsx::addStyle(wb, sh, st_t, 1, 1)
    openxlsx::writeData(wb, sh, sub, startRow = 2); openxlsx::addStyle(wb, sh, st_s, 2, 1)
    if (!is.null(note)) openxlsx::writeData(wb, sh, note, startRow = 3) }
  heat_sheet <- function(sh, h, title) {
    w <- q_wide(h); nt <- nrow(h$teams); c0 <- ncol(w) - nt
    head(sh, title, "Team columns: % of the team's tests flagged (darker = more; empty = not tested). Rows flagged in most teams are questionnaire problems (wording, translation, CAPI routing), not counted against teams.")
    openxlsx::writeData(wb, sh, w, startRow = 5, headerStyle = st_h)
    for (j in seq_len(nt)) { v <- w[[c0 + j]]; cols <- q_heat_col(v)
      for (cl in unique(stats::na.omit(cols))) { i <- which(cols == cl)
        openxlsx::addStyle(wb, sh, openxlsx::createStyle(fgFill = cl, halign = "center", fontColour = if (v[i[1]] >= 20) "white" else "#1F2A44"),
                           rows = 5 + i, cols = c0 + j, stack = TRUE) } }
    openxlsx::setColWidths(wb, sh, seq_len(c0), c(8, 8, if (h$by == "question") c(10, 40), 10, 7, 7, 9, 10))
    openxlsx::setColWidths(wb, sh, c0 + seq_len(nt), 6)
    openxlsx::freezePane(wb, sh, firstActiveRow = 6, firstActiveCol = c0 + 1)
  }
  heat_sheet("Questions", hq, "Flagged questions by team")
  heat_sheet("Modules", hm, "Flagged modules by team")
  ce <- merge(hq$cells[flags > 0], hq$rows[, .(row, dataset, module, question, description)], by = "row")
  ce[, Team := hq$teams$team_label[match(team, hq$teams$team)]]
  d <- ce[order(dataset, module, question, team), .(Dataset = dataset, Module = module, Question = question, Description = description, Team,
                                                   Tests = tests, Flags = flags, `% flagged` = round(rate, 1), `Interviewers flagged` = interviewers)]
  head("By team", "Flagged questions: one row per question and team")
  openxlsx::writeData(wb, "By team", d, startRow = 4, headerStyle = st_h, withFilter = TRUE)
  openxlsx::setColWidths(wb, "By team", 1:9, c(8, 8, 10, 40, 14, 7, 7, 9, 30)); openxlsx::freezePane(wb, "By team", firstActiveRow = 5)
  s <- q_problems(sc)
  if (nrow(s)) {
    p <- s[, .(Dataset = dataset, Module = module, Problem = what, Severity = severity, Action = action, Who = owner, When = timing)]
    head("Questionnaire problems", "Questionnaire problems (questions and modules flagged in most teams)")
    openxlsx::writeData(wb, "Questionnaire problems", p, startRow = 4, headerStyle = st_h, withFilter = TRUE)
    openxlsx::addStyle(wb, "Questionnaire problems", openxlsx::createStyle(wrapText = TRUE, valign = "top"), rows = 4 + seq_len(nrow(p)), cols = 1:7, gridExpand = TRUE)
    openxlsx::setColWidths(wb, "Questionnaire problems", 1:7, c(8, 8, 70, 9, 50, 16, 12))
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  invisible(file)
}

#' Question and module flag summaries on one HTML page to share
#'
#' The heat maps of the Questionnaire page (all flagged modules and the questions with the most flags, by team) and the
#' questionnaire problems, on one page that opens offline in any browser, like the briefing. Codes and figures only, no
#' records.
#' @inheritParams ipm_question_xlsx
#' @param file the .html file to write
#' @param top number of questions shown (those with the most flags)
#' @return the file name, invisibly (NULL without a flag analysis)
#' @export
ipm_question_html <- function(res, sc, file, top = 40) {
  hq <- ipm_question_heat(res, sc, "question", top = top); hm <- ipm_question_heat(res, sc, "module", top = Inf)
  if (is.null(hq)) return(invisible(NULL))
  h <- htmltools::tags; survey <- res$cfg$survey_name %||% sc$survey$name %||% ""
  pc <- c(High = "#E2231A", Medium = "#F28C28", Low = "#B0B4BA")
  heat <- function(x, title, hint) {
    rw <- x$rows; tm <- x$teams; ce <- x$cells; k <- paste(ce$row, ce$team)
    h$div(class = "card", h$h2(title), h$div(class = "hint", hint), h$div(class = "wrap", h$table(class = "qh",
      h$tr(h$th(class = "lab", if (x$by == "question") "Question" else "Module"), h$th("All teams"),
           lapply(seq_len(nrow(tm)), function(j) h$th(class = "tm", title = sprintf("%s - %s priority", tm$team_label[j], tm$priority[j]),
                                                      h$span(class = "dot", style = sprintf("background:%s", pc[[tm$priority[j]]])), q_short(tm$team_label[j])))),
      lapply(seq_len(nrow(rw)), function(i) { r <- rw[i]
        h$tr(h$td(class = "lab", h$b(if (x$by == "question") paste(r$dataset, r$question) else paste(r$dataset, r$module)),
                  if (isTRUE(r$survey_wide)) h$span(class = "chip", "most teams"),
                  if (x$by == "question" && !is.na(r$description)) h$div(class = "d", substr(r$description, 1, 70))),
             h$td(class = "tot", sprintf("%.0f%%", r$rate), h$div(class = "d", sprintf("%d of %d teams", r$teams_flagged, r$teams))),
             lapply(seq_len(nrow(tm)), function(j) { m <- match(paste(r$row, tm$team[j]), k)
               if (is.na(m)) return(h$td(class = "c na", title = sprintf("%s: not tested", tm$team_label[j])))
               z <- ce[m]
               h$td(class = "c", style = sprintf("background:%s;color:%s", q_heat_col(z$rate), if (z$rate >= 20) "white" else "#1f2a44"),
                    title = sprintf("%s - %s: %d of %d tests flagged%s", tm$team_label[j], r$row, z$flags, z$tests,
                                    if (nzchar(z$interviewers)) paste0("; interviewers ", z$interviewers) else ""),
                    if (z$flags > 0) sprintf("%.0f", z$rate)) }))
      }))))
  }
  s <- q_problems(sc)
  probs <- if (nrow(s)) h$div(class = "card", h$h2(sprintf("Questionnaire problems (%d)", nrow(s))),
    h$div(class = "hint", "Flagged in most teams: check wording, translation and CAPI routing, and send one note to all teams. Not counted against teams or interviewers."),
    lapply(seq_len(nrow(s)), function(i) h$div(class = "line", h$b(paste(s$dataset[i], s$module[i])), " ", s$what[i])))
  css <- paste(
    "body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:#1f2a44;margin:0;background:#F6F7F9}",
    ".page{max-width:1200px;margin:0 auto;padding:24px 20px 40px}.meta{font-size:13px;color:#5f6368}h1{font-size:26px;margin:4px 0 12px}h2{font-size:16px;margin:0 0 4px}",
    ".card{background:#fff;border:1px solid #e6e8ec;border-radius:10px;padding:12px 14px;margin-bottom:14px}.hint{font-size:12px;color:#5f6368;margin-bottom:8px}",
    ".wrap{overflow-x:auto}table.qh{border-collapse:separate;border-spacing:2px;font-size:11px}table.qh th{font-weight:500;color:#5f6368;white-space:nowrap}",
    "table.qh th.tm{writing-mode:vertical-rl;transform:rotate(180deg);padding:2px 0}td.lab{min-width:190px;padding-right:8px}",
    "td.tot{white-space:nowrap;padding-right:8px;text-align:right}td.c{min-width:24px;height:20px;border-radius:3px;text-align:center;font-size:10px}",
    "td.na{background:repeating-linear-gradient(45deg,#f3f4f6,#f3f4f6 3px,#fff 3px,#fff 6px)}.d{font-size:10.5px;color:#5f6368}",
    ".dot{display:inline-block;width:7px;height:7px;border-radius:50%;margin-bottom:3px}",
    ".chip{font-size:10px;background:#FFF3D6;color:#7A5300;border-radius:10px;padding:1px 6px;margin-left:6px}",
    ".line{font-size:13px;padding:5px 0;border-top:1px solid #eef0f3}.foot{font-size:11px;color:#5f6368;margin-top:18px}",
    "@media print{body{background:#fff}.card{break-inside:avoid}}")
  head <- sprintf('<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>%s</title><style>%s</style></head>',
                  htmltools::htmlEscape(sprintf("Question and module flags - %s", survey)), css)
  body <- h$body(h$div(class = "page",
      h$div(class = "meta", sprintf("%s · period of analysis %s · survey flag rate %.1f%% of tests", survey, sc$survey$period %||% "-", sc$survey$flag_rate %||% NA)),
      h$h1("Question and module flags"),
      heat(hm, "Flagged modules by team", paste("Each cell: % of the team's tests of that module flagged (darker = more; hatched = not tested).",
           "A row dark in most teams is a questionnaire problem; a row dark in one or two teams is that team's problem. Point at a cell for the figures and the interviewers flagged.")),
      heat(hq, sprintf("The %d questions with the most flags, by team", nrow(hq$rows)), "Same reading, question by question."),
      probs,
      h$div(class = "foot", sprintf("Interviewer Performance Monitoring (ipmR %s), generated %s. Codes and figures only: no interview records.",
                                    IPMR_VERSION, format(Sys.time(), "%d %b %Y %H:%M")))))
  con <- file(file, "wb"); on.exit(close(con))
  writeLines(enc2utf8(c("<!DOCTYPE html>", '<html lang="en">', head, as.character(body), "</html>")), con, useBytes = TRUE)
  invisible(file)
}
