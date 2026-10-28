# Report generation (port of putdocx / asdoc / export excel parts)

save_plot_png <- function(g, file, w = 10, h = 6.4, dpi = 150) {
  if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png(file, width = w, height = h, units = "in", res = dpi)
  else grDevices::png(file, width = w, height = h, units = "in", res = dpi)
  on.exit(grDevices::dev.off())
  print(g)
  invisible(file)
}

ipm_flextable <- function(df, font_size = 8, title = NULL) {
  ft <- flextable::flextable(df)
  ft <- flextable::fontsize(ft, size = font_size, part = "all")
  ft <- flextable::font(ft, fontname = "Arial", part = "all")
  ft <- flextable::bg(ft, bg = unicef_cyan, part = "header")
  ft <- flextable::color(ft, color = "white", part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::padding(ft, padding = 1.5, part = "all")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = "grey40", width = 0.75), part = "body")
  ft <- flextable::hline(ft, border = officer::fp_border(color = "grey85", width = 0.5), part = "body")
  num <- names(df)[vapply(df, is.numeric, logical(1))]
  if (length(num)) ft <- flextable::align(ft, j = num, align = "right", part = "all")
  if (!is.null(title)) {
    ft <- flextable::add_header_lines(ft, values = title)
    ft <- flextable::bg(ft, i = 1, bg = "white", part = "header")
    ft <- flextable::color(ft, i = 1, color = "grey10", part = "header")
  }
  flextable::autofit(ft)
}

new_doc <- function() {
  doc <- officer::read_docx()
  sect <- officer::prop_section(
    page_size = officer::page_size(orient = "landscape", width = 8.27, height = 11.69),
    page_margins = officer::page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5, header = 0.3, footer = 0.3))
  officer::body_set_default_section(doc, sect)
}

add_heading <- function(doc, txt, size = 13, color = unicef_cyan) {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(txt, officer::fp_text(color = color, bold = TRUE, font.size = size, font.family = "Arial"))))
}
add_text <- function(doc, txt, size = 9, italic = FALSE) {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(txt, officer::fp_text(font.size = size, italic = italic, font.family = "Arial"))))
}
add_bullet <- function(doc, txt, size = 9) {
  officer::body_add_fpar(doc, officer::fpar(officer::ftext(paste0("\u2022  ", txt), officer::fp_text(font.size = size, font.family = "Arial")),
                                            fp_p = officer::fp_par(padding.left = 12, padding.bottom = 3)))
}
add_plot <- function(doc, g, dir, o) {
  f <- tempfile(tmpdir = dir, fileext = ".png")
  save_plot_png(g, f, dpi = o$fig_dpi)
  officer::body_add_img(doc, f, width = 10.3, height = 6.6)
}

#' Write the IPM report and companion files
#' @param res result of run_ipm(..., write = FALSE)
#' @param results_dir output folder
#' @param progress function(message) for progress reporting
#' @export
ipm_report <- function(res, results_dir = res$cfg$results_dir, progress = function(...) NULL) {
  cfg <- res$cfg; o <- cfg$options
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  figdir <- file.path(results_dir, ".fig"); dir.create(figdir, showWarnings = FALSE)
  on.exit(unlink(figdir, recursive = TRUE), add = TRUE)
  stamp <- format(Sys.time(), "%d%b%Y_%Hh%M")
  doc <- new_doc()
  doc <- add_heading(doc, sprintf("Interviewer Performance Monitoring (IPM) report - %s", format(Sys.Date(), "%d %b %Y")), 16)
  doc <- add_heading(doc, cfg$survey_name, 13, "grey15")
  doc <- add_text(doc, sprintf("Produced with ipmR %s (R port of the Stata IPM application v7.0.0 by Pierre Martel).", utils_pkgver()), 8, TRUE)
  doc <- add_text(doc, "Outputs included in this report:", 10)
  for (id in names(res$outputs)) doc <- add_text(doc, paste("-", ipm_output_names[[id]]), 9)
  if (!is.null(res$data_summary) && nrow(res$data_summary)) {
    fd <- suppressWarnings(range(as.Date(c(res$data_summary$first_day, res$data_summary$last_day)), na.rm = TRUE))
    if (all(is.finite(fd))) doc <- add_text(doc, sprintf("Fieldwork covered by the data: %s to %s (%d days).", format(fd[1], "%d %b %Y"), format(fd[2], "%d %b %Y"), as.integer(diff(fd)) + 1L), 9)
  }
  if (!is.null(cfg$period$start)) doc <- add_text(doc, sprintf("Period of analysis for anthropometry case listing and flag analysis: %s", period_bounds(cfg)$label), 9)
  # key findings
  progress("Writing report: key findings")
  sm <- tryCatch(ipm_summary(res), error = function(e) NULL)
  ht <- ipm_howto()
  if (!is.null(sm)) {
    doc <- officer::body_add_break(doc)
    doc <- add_heading(doc, "KEY FINDINGS AND RECOMMENDED FOLLOW-UP", 14)
    doc <- add_text(doc, "Automatically generated from the results below. Treat as a starting point for discussion with the survey team.", 8, TRUE)
    for (sec in sm$sections) {
      doc <- add_heading(doc, sec$title, 11, "grey15")
      for (b in sec$bullets) doc <- add_bullet(doc, b)
    }
    if (!is.null(sm$plots$team_module)) {
      doc <- officer::body_add_break(doc)
      doc <- add_plot(doc, sm$plots$team_module, figdir, o)
      doc <- add_text(doc, paste("How to read:", ht$team_module), 8.5, TRUE)
    }
    sc <- tryCatch(ipm_scorecard(res), error = function(e) NULL)
    if (!is.null(sc) && nrow(sc$teams)) {
      doc <- officer::body_add_break(doc)
      doc <- add_heading(doc, "Team scorecard and priorities", 11)
      doc <- add_text(doc, "Priority from the warning signs of each team (high-severity sign = 3 points, medium = 1; High = 9+ points or an excess of flags for the team as a whole, Medium = 3-8 points). Details in team_action_plan.docx and data/scorecard_*.csv.", 8, TRUE)
      tb <- sc$teams[, .(Team = team_label, Priority = priority, Points = points, Signals = signals, `% flagged` = round(rate, 1), `x survey rate` = round(ratio, 1),
                         `Modules` = top_modules, `Interviewers of concern` = interviewers_of_concern)]
      tb[is.na(tb)] <- ""
      doc <- flextable::body_add_flextable(doc, ipm_flextable(as.data.frame(tb), 8))
    }
    if (!is.null(sm$tables$interviewers) && nrow(sm$tables$interviewers)) {
      doc <- officer::body_add_break(doc)
      doc <- add_heading(doc, "Flag rate by interviewer (highest first)", 11)
      ti <- head(sm$tables$interviewers, 40)
      tb <- data.frame(Team = ti$team, Interviewer = ti$interv, `% flagged` = round(ti$rate, 1), Tests = ti$tests, Flags = ti$flags, check.names = FALSE)
      doc <- flextable::body_add_flextable(doc, ipm_flextable(tb, 8))
    }
  }
  for (id in names(res$outputs)) {
    r <- res$outputs[[id]]
    progress(sprintf("Writing report: %s", id))
    doc <- officer::body_add_break(doc)
    if (id == "quesperday") {
      doc <- add_heading(doc, r$title, 12)
      doc <- add_text(doc, r$note, 8, TRUE)
      doc <- add_text(doc, paste("How to read:", ipm_howto()$quesperday), 8.5, TRUE)
      if (!length(r$tables)) doc <- add_text(doc, "NO DATA TO PROCESS")
      for (tb in r$tables) { doc <- flextable::body_add_flextable(doc, ipm_flextable(tb, 7)); doc <- add_text(doc, "", 4) }
      next
    }
    if (id == "flaganal") { doc <- report_flags(doc, r, res, figdir, o, results_dir); next }
    doc <- add_heading(doc, r$title, 12)
    if (!is.null(ht[[id]])) doc <- add_text(doc, paste("How to read:", ht[[id]]), 8.5, TRUE)
    for (s in r$sections) for (g in s$plots) doc <- add_plot(doc, g, figdir, o)
  }
  out <- file.path(results_dir, "IPM_report.docx")
  print(doc, target = out)
  file.copy(out, file.path(results_dir, sprintf("IPM_report_%s.docx", stamp)), overwrite = TRUE)
  # anthropometry case listing (Excel)
  if (!is.null(res$outputs$ant_oor)) {
    a <- res$outputs$ant_oor
    cases <- as.data.frame(a$cases)
    if (nrow(cases)) cases$doi <- as.Date(cases$doi)
    labs <- vapply(names(cases), function(v) { l <- a$case_labels[v]; if (is.na(l) || !nzchar(l)) v else sprintf("%s: %s", v, l) }, character(1))
    wb <- openxlsx::createWorkbook()
    sh <- substr(gsub("[^A-Za-z0-9-]", "", a$period), 1, 31)
    openxlsx::addWorksheet(wb, sh)
    openxlsx::writeData(wb, sh, as.data.frame(as.list(labs)), colNames = FALSE, startRow = 1)
    openxlsx::writeData(wb, sh, cases, colNames = FALSE, startRow = 2)
    openxlsx::addStyle(wb, sh, openxlsx::createStyle(textDecoration = "bold", fgFill = unicef_cyan, fontColour = "white", wrapText = TRUE), rows = 1, cols = seq_along(labs))
    openxlsx::setColWidths(wb, sh, cols = seq_along(labs), widths = 14)
    openxlsx::freezePane(wb, sh, firstRow = TRUE)
    openxlsx::saveWorkbook(wb, file.path(results_dir, sprintf("anthro_report_%s.xlsx", format(Sys.Date(), "%d%b%Y"))), overwrite = TRUE)
  }
  # tidy data for dashboards (Power BI / Fabric)
  ddir <- file.path(results_dir, "data"); dir.create(ddir, showWarnings = FALSE)
  for (id in names(res$outputs)) {
    r <- res$outputs[[id]]
    if (id == "flaganal") {
      if (!is.null(r$tests) && nrow(r$tests)) data.table::fwrite(r$tests, file.path(ddir, "flag_tests.csv"))
      if (!is.null(r$scores)) data.table::fwrite(r$scores, file.path(ddir, "flag_scores.csv"))
    } else if (!is.null(r$data) && nrow(r$data)) data.table::fwrite(r$data, file.path(ddir, paste0(id, ".csv")))
  }
  # scorecard and team action plan (AI plan if one was generated in the session, otherwise rule-based)
  sc <- tryCatch(ipm_scorecard(res), error = function(e) NULL)
  if (!is.null(sc)) {
    progress("Writing team action plan")
    data.table::fwrite(sc$teams, file.path(ddir, "scorecard_teams.csv"))
    if (!is.null(sc$interviewers)) data.table::fwrite(sc$interviewers, file.path(ddir, "scorecard_interviewers.csv"))
    data.table::fwrite(sc$signals, file.path(ddir, "scorecard_signals.csv"))
    plan <- res$action_plan %||% ipm_plan_rules(sc)
    tryCatch(ipm_plan_docx(plan, file.path(results_dir, "team_action_plan.docx"), sc, cfg$survey_name, sc$survey$period),
             error = function(e) warning("Action plan not written: ", conditionMessage(e)))
  }
  invisible(out)
}

IPMR_VERSION <- "0.4.5.9000"
utils_pkgver <- function() IPMR_VERSION

report_flags <- function(doc, r, res, figdir, o, results_dir) {
  doc <- add_heading(doc, "FLAG ANALYSIS OF INTERVIEWER BIAS IN SPECIFIED MODULES AND DATASETS", 12)
  if (is.null(r$tests) || !nrow(r$tests)) return(add_text(doc, "No flag test could be performed (check modules, teams and period of analysis)."))
  ht <- ipm_howto()
  doc <- add_text(doc, sprintf("Period %s. Tables of each flag, with a one-line reading of the pattern, are in the 'flag_results' folder (per dataset and per team).", r$period), 8, TRUE)
  doc <- add_text(doc, paste("What the test does:", ht$flaganal), 8.5, TRUE)
  plots <- r$plots
  summ <- Filter(function(z) z$type == "summary", plots)
  for (z in summ) { doc <- add_plot(doc, z$plot, figdir, o); doc <- add_text(doc, paste("How to read:", ht$flag_summary), 8.5, TRUE) }
  seen <- character()
  for (z in Filter(function(z) z$type != "summary", plots)) {
    doc <- add_plot(doc, z$plot, figdir, o)
    if (!z$type %in% seen) { doc <- add_text(doc, paste("How to read:", ht[[paste0("flag_", z$type)]]), 8.5, TRUE); seen <- c(seen, z$type) }
  }
  doc <- officer::body_add_break(doc)
  h <- r$highly
  doc <- add_heading(doc, "Highly flagged questions", 12)
  doc <- add_text(doc, sprintf("List of questions with %s%% or more flags, for the period %s", o$highly_flag, r$period), 9)
  if (nrow(h)) {
    tb <- data.frame(Dataset = h$dataset, Module = sub("\\*$", "", h$module), Variable = h$variable,
                     `% of flags` = round(h$flag_score, 0), Tests = h$tests, Description = h$vardesc, check.names = FALSE)
    ft <- ipm_flextable(tb, 8)
    ft <- flextable::width(ft, j = 1:6, width = c(0.7, 0.7, 1.2, 0.75, 0.55, 6.6))
    ft <- flextable::set_table_properties(ft, layout = "fixed")
    doc <- flextable::body_add_flextable(doc, ft)
  } else doc <- add_text(doc, "None.")
  write_flag_tables(r, res, results_dir)
  doc
}

#' Group flagged tests by team and question (one table per question, all flagged interviewers listed)
#' @param tables the `tables` element of flag_analysis()
#' @export
group_flag_tables <- function(tables) {
  if (!length(tables)) return(list())
  key <- vapply(tables, function(z) paste(z$dataset, z$team, z$variable, sep = "|"), "")
  lapply(split(seq_along(tables), factor(key, levels = unique(key))), function(ix) {
    it <- tables[ix]; z <- it[[1]]
    ints <- vapply(it, `[[`, 0, "interv")
    tb <- z$table
    flagged_cols <- names(tb)[vapply(names(tb), function(n) any(startsWith(n, paste0(format(ints, trim = TRUE), " "))), logical(1))]
    names(tb)[names(tb) %in% flagged_cols] <- paste0(names(tb)[names(tb) %in% flagged_cols], " *")
    list(dataset = z$dataset, team = z$team, variable = z$variable, module = z$module, desc = z$desc, test = z$test,
         interviewers = ints, p = vapply(it, `[[`, 0, "p"), readings = vapply(it, function(y) y$pattern %||% NA_character_, ""),
         title = sprintf("%s (%s)  -  flagged interviewer%s: %s  -  %s", z$variable, z$dataset, if (length(ints) > 1) "s" else "",
                         paste(format(ints, trim = TRUE), collapse = ", "), z$test),
         table = tb)
  })
}

write_flag_doc <- function(groups, file, heading, period, survey) {
  doc <- new_doc()
  doc <- add_heading(doc, heading, 13)
  doc <- add_text(doc, sprintf("%s. Survey: %s. %d flagged tests on %d questions. One table per question and team: columns marked * are the flagged interviewer(s); the table shows the answers recorded by all interviewers of the team.",
                               period, survey, sum(vapply(groups, function(g) length(g$interviewers), 0)), length(groups)), 8, TRUE)
  for (g in groups) {
    doc <- add_text(doc, "", 4)
    doc <- add_heading(doc, g$title, 9, "grey10")
    doc <- add_text(doc, g$desc, 8, TRUE)
    for (k in seq_along(g$readings)) if (!is.na(g$readings[k]))
      doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext(sprintf("Reading (p=%s): %s", signif(g$p[k], 2), g$readings[k]),
                                                                      officer::fp_text(font.size = 8.5, bold = TRUE, color = unicef_dark, font.family = "Arial"))))
    if (length(g$interviewers) > 1)
      doc <- add_text(doc, "Several interviewers of the same team are flagged on this question: their answers differ from each other, which often means the question is understood or asked differently within the team.", 8, TRUE)
    doc <- officer::body_add_table(doc, g$table, style = "table_template", first_column = TRUE)
  }
  print(doc, target = file)
  invisible(file)
}

write_flag_tables <- function(r, res, results_dir) {
  fd <- file.path(results_dir, "flag_results"); dir.create(fd, showWarnings = FALSE)
  old <- list.files(fd, pattern = "\\.docx$", full.names = TRUE); if (length(old)) unlink(old)
  if (!length(r$tables)) return(invisible())
  tl <- res$team_info %||% res$prepared$team_info
  groups <- group_flag_tables(r$tables)
  ds <- vapply(groups, `[[`, "", "dataset"); tm <- vapply(groups, `[[`, 0, "team")
  for (d in unique(ds)) write_flag_doc(groups[ds == d], file.path(fd, sprintf("flags_dataset_%s.docx", d)), sprintf('Flag tables: "%s" dataset', d), r$period, res$cfg$survey_name)
  for (t in sort(unique(tm))) write_flag_doc(groups[tm == t], file.path(fd, sprintf("flags_team_%d.docx", t)),
                                             sprintf("Flag tables: team %s", tl$team_label[match(t, tl$team)] %||% paste0("T", t)), r$period, res$cfg$survey_name)
  invisible()
}
