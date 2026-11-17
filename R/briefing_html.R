# Shareable briefing: one self-contained HTML page (no internet needed, no records) to email or put on a shared drive,
# for managers who do not use the app.

#' Write the briefing as one HTML page to share
#'
#' The briefing of the app on one page that opens offline in any browser and prints on A4: the top numbers, the teams
#' to act on with their first action and follow-up, the interviewers to observe, the team health grid, the questionnaire
#' problems and what was resolved. Codes and figures only, no records.
#' @param res result of `run_ipm()`
#' @param sc result of `ipm_scorecard()`
#' @param file the .html file to write
#' @param cmp result of `ipm_compare()` or `ipm_history_update()`, or NULL
#' @param actions result of `ipm_actions()`, or NULL (no follow-up shown)
#' @return the file name, invisibly
#' @export
ipm_briefing_html <- function(res, sc, file, cmp = NULL, actions = NULL) {
  b <- ipm_briefing(sc, cmp); h <- htmltools::tags
  if (is.null(cmp)) cmp <- ipm_compare(sc, list(), as.Date(NA))
  last <- history_dates(res)$last
  iss <- copy(cmp$issues); if (!"key" %in% names(iss)) iss[, key := issue_key(iss)]
  if (!is.null(actions)) iss <- ipm_action_status(iss, actions, last)
  survey <- res$cfg$survey_name %||% sc$survey$name %||% ""; if (!nzchar(survey)) survey <- "MICS survey"
  short <- function(x) sub(" \\([^()]*\\)$", "", x)
  dt <- function(d) if (is.null(d) || is.na(d)) "-" else format(as.Date(d), "%d %b %Y")
  pill <- function(cls, txt) h$span(class = paste("pill", cls), txt)

  kp <- b$kpi
  kpis <- lapply(seq_len(nrow(kp)), function(i) { k <- kp[i]
    if (k$what == "resolved" && is.null(b$previous)) return(h$div(class = "kpi", h$div(class = "l", "Teams in the survey"), h$div(class = "v", nrow(sc$teams))))
    d <- if (is.na(k$before)) NULL else { x <- k$now - k$before
      h$span(class = paste("d", if (x == 0) "same" else if ((x > 0) == (k$what != "resolved")) "up" else "down"), if (x == 0) "same" else sprintf("%+d", x)) }
    h$div(class = "kpi", h$div(class = "l", k$label), h$div(class = "v", k$now, d),
          if (k$what == "teams" && k$now > 0) h$div(class = "s", sprintf("%d high · %d medium", sum(b$teams$priority == "High"), sum(b$teams$priority == "Medium")))) })

  follow <- function(t) {
    if (is.null(iss$action)) return(NULL)
    x <- iss[level != "survey" & team == t]; if (!nrow(x)) return(NULL)
    n <- c(done = sum(x$action == "done" & !x$still_found), assigned = sum(x$action == "assigned"), escalated = sum(x$action == "escalated"), still = sum(x$still_found))
    txt <- paste(c(if (n[["done"]]) paste(n[["done"]], "done"), if (n[["assigned"]]) paste(n[["assigned"]], "assigned"), if (n[["escalated"]]) paste(n[["escalated"]], "escalated")), collapse = " · ")
    h$div(class = "fu", paste0("Follow-up: ", if (nzchar(txt)) txt else sprintf("none recorded (%d issue%s)", nrow(x), if (nrow(x) > 1) "s" else "")),
          if (n[["still"]]) pill("warn", sprintf("%d done but still found", n[["still"]]))) }
  chip <- function(st) if (!is.na(st) && st != "first") pill(if (st == "new") "new" else "old", if (st == "new") "New since last run" else st)
  team_row <- function(t) h$tr(
    h$td(pill(tolower(t$priority), sprintf("%s · %s", short(t$team_label), t$priority))),
    h$td(h$div(class = "hd", t$headline, " ", chip(t$status)),
         h$div(class = "sub", paste(c(if (t$more > 0) sprintf("+%d more team issue%s", t$more, if (t$more > 1) "s" else ""),
                                      if (nzchar(t$interviewers)) paste("interviewers", t$interviewers)), collapse = " · ")),
         h$div(class = "do", "→ ", sprintf("%s (%s, %s)", t$do, t$owner, tolower(t$timing))), follow(t$team)))
  hi <- b$teams[priority == "High"]; md <- b$teams[priority != "High"]

  g <- b$grid; ha <- health_areas
  used <- names(ha)[names(ha) %in% g[!is.na(sev), harea] | names(ha) %in% c("flags", "duration", "completion")]
  tms <- unique(g[, .(team, team_label, priority)])
  grid <- h$table(class = "grid",
    h$tr(h$th(""), lapply(used, function(a) h$th(ha[[a]]))),
    lapply(seq_len(nrow(tms)), function(i) { t <- tms[i]
      h$tr(h$td(class = "t", h$span(class = paste("dot", tolower(t$priority))), short(t$team_label)),
           lapply(used, function(a) { z <- g[team == t$team & harea == a]
             if (!nrow(z) || is.na(z$sev)) return(h$td(class = "c"))
             h$td(class = paste("c", paste0("s", z$sev), if (isTRUE(z$persisting)) "p"), title = z$what, if (z$n > 1) z$n) })) }))

  q <- b$questionnaire
  qm <- if (nrow(q)) q[, .(qs = paste(head(stats::na.omit(question), 5), collapse = ", "), n = .N), by = .(dataset, module)][order(-n)] else NULL
  still <- if (!is.null(iss$still_found)) iss[still_found == TRUE] else iss[0]
  cmp_txt <- if (!is.null(b$previous)) sprintf("Compared with the export of %s (%d earlier run%s)", dt(b$previous$data_last), b$runs, if (b$runs > 1) "s" else "")
             else "First run kept for this survey"

  css <- "
  body {font-family:'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color:#1f2a44; margin:0; background:#F6F7F9}
  .page {max-width:1000px; margin:0 auto; padding:24px 20px 40px 20px}
  .meta {font-size:13px; color:#5f6368} h1 {font-size:26px; margin:4px 0 2px 0} h2 {font-size:16px; margin:0 0 4px 0}
  .kpis {display:grid; grid-template-columns:repeat(4, minmax(0, 1fr)); gap:10px; margin:16px 0}
  .kpi, .card {background:white; border:1px solid #e6e8ec; border-radius:10px; padding:12px 14px}
  .kpi .l {font-size:12px; color:#5f6368} .kpi .v {font-size:26px; font-weight:600} .kpi .s {font-size:11px; color:#5f6368}
  .d {font-size:12px; font-weight:600; margin-left:6px} .up {color:#C4161C} .down {color:#00833D} .same {color:#8A8A8A}
  .card {margin-bottom:14px} .hint {font-size:12px; color:#5f6368; margin-bottom:6px}
  table.items {width:100%; border-collapse:collapse} table.items td {border-top:1px solid #eef0f3; padding:8px 6px; vertical-align:top; font-size:13px}
  table.items td:first-child {width:120px}
  .hd {font-weight:600} .sub, .fu {font-size:12px; color:#5f6368; margin-top:2px} .do {font-size:12.5px; color:#2B3F87; margin-top:3px}
  .pill {display:inline-block; font-size:11px; font-weight:600; padding:2px 8px; border-radius:20px; white-space:nowrap}
  .high {background:#FDE8E7; color:#A3120F} .medium {background:#FEF0E6; color:#9A4A0F} .low {background:#F1F2F4; color:#555} .iv {background:#EAEFFA; color:#2B3F87}
  .new {background:#E6F4FB; color:#0B5A75; font-weight:400} .old {background:#FFF3D6; color:#7A5300; font-weight:400} .warn {background:#FDE8E7; color:#A3120F; margin-left:6px}
  .grid {border-collapse:separate; border-spacing:3px; font-size:11px} .grid th {font-weight:500; color:#5f6368}
  .grid td.t {white-space:nowrap; padding-right:6px} .grid td.c {width:46px; height:18px; border-radius:3px; background:#F3F4F6; text-align:center; color:white}
  .grid td.s1 {background:#E2231A} .grid td.s2 {background:#F28C28} .grid td.p {outline:2px solid #1f2a44; outline-offset:-2px}
  .dot {display:inline-block; width:7px; height:7px; border-radius:50%; margin-right:5px} .dot.high {background:#E2231A} .dot.medium {background:#F28C28} .dot.low {background:#8A8A8A}
  .line {font-size:13px; padding:5px 0; border-top:1px solid #eef0f3}
  .foot {font-size:11px; color:#5f6368; margin-top:18px}
  @media (max-width:700px) {.kpis {grid-template-columns:repeat(2, minmax(0, 1fr))}}
  @media print {body {background:white} .page {padding:0} .card, .kpi {break-inside:avoid}}"

  head <- sprintf('<head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>%s</title><style>%s</style></head>',
                  htmltools::htmlEscape(sprintf("IPM briefing - %s - %s", survey, dt(last))), css)
  body <- h$body(h$div(class = "page",
      h$div(class = "meta", sprintf("%s · data up to %s · period of analysis %s", survey, dt(last), sc$survey$period %||% "-")),
      h$h1("IPM briefing"), h$div(class = "meta", cmp_txt),
      h$div(class = "kpis", kpis),
      h$div(class = "card", h$h2("Teams to act on this week"),
            h$div(class = "hint", "High priority teams first; the arrow is the first thing to do."),
            if (nrow(b$teams)) h$table(class = "items", lapply(seq_len(nrow(hi)), function(i) team_row(hi[i])), lapply(seq_len(nrow(md)), function(i) team_row(md[i])))
            else h$div(class = "line", "No team stands out this week.")),
      h$div(class = "card", h$h2("Interviewers to observe now"),
            if (nrow(b$interviewers)) h$table(class = "items", lapply(seq_len(nrow(b$interviewers)), function(i) { x <- b$interviewers[i]
              h$tr(h$td(pill("iv", sprintf("Int. %s · %s", fmt_int(x$interv), short(x$team_label)))),
                   h$td(h$div(class = "hd", x$headline, " ", chip(x$status)), h$div(class = "do", "→ ", sprintf("%s (%s, %s)", x$do, x$owner, tolower(x$timing))))) }))
            else h$div(class = "line", "No interviewer with a high-severity issue.")),
      h$div(class = "card", h$h2("Team health"),
            h$div(class = "hint", "Worst issue of each team in each area: red high, orange medium; number = issues in the area; dark outline = also at the last run."), grid),
      if (nrow(still)) h$div(class = "card", h$h2(sprintf("Done but still found (%d)", nrow(still))),
            lapply(seq_len(nrow(still)), function(i) { x <- still[i]; h$div(class = "line", h$b(if (is.na(x$team)) "Survey" else short(x$team_label)), " ", x$headline) })),
      h$div(class = "card", h$h2(sprintf("Questionnaire problems (%d)", nrow(q))),
            h$div(class = "hint", "Questions flagged in most teams: check wording, translation and CAPI routing; one note to all teams."),
            if (!is.null(qm)) lapply(seq_len(min(nrow(qm), 8)), function(i) h$div(class = "line", h$b(paste(qm$dataset[i], qm$module[i])), " ", if (nzchar(qm$qs[i])) qm$qs[i] else "whole module"))
            else h$div(class = "line", "None.")),
      if (!is.null(b$previous)) h$div(class = "card", h$h2(sprintf("Resolved since the last run (%d)", nrow(b$resolved))),
            if (nrow(b$resolved)) lapply(seq_len(min(nrow(b$resolved), 12)), function(i) { x <- b$resolved[i]
              h$div(class = "line", h$b(if (is.na(x$team)) "Survey" else if (x$level == "interviewer") sprintf("Int. %s", fmt_int(x$interv)) else short(x$team_label)), " no longer found: ", x$headline) })
            else h$div(class = "line", "Nothing resolved yet.")),
      h$div(class = "foot", sprintf("Interviewer Performance Monitoring (ipmR %s), generated %s. Codes and figures only: no interview records.", IPMR_VERSION, format(Sys.time(), "%d %b %Y %H:%M")))))
  # head written by hand: htmltools moves tags$head content out of the page when rendering
  txt <- enc2utf8(c("<!DOCTYPE html>", '<html lang="en">', head, as.character(body), "</html>"))
  con <- file(file, open = "wb"); on.exit(close(con), add = TRUE)
  writeLines(txt, con, useBytes = TRUE)
  invisible(file)
}
