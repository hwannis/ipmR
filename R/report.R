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
add_plot <- function(doc, g, dir, o, drawn = NULL) {
  f <- if (!is.null(drawn)) drawn[[rlang::obj_address(g)]]
  if (is.null(f) || !file.exists(f)) { f <- tempfile(tmpdir = dir, fileext = ".png"); save_plot_png(g, f, dpi = o$fig_dpi) }
  officer::body_add_img(doc, f, width = 10.3, height = 6.6)
}

# one PNG (a job for ipm_parallel)
png_job <- function(j, dpi) { save_plot_png(j$plot, j$file, dpi = dpi); j$file }

# Draw every chart of the report in advance, on several processes when the machine allows it.
# Returns file names indexed by the address of each plot object, used by add_plot().
draw_report_plots <- function(plots, dir, o, progress = function(...) NULL) {
  plots <- Filter(function(g) inherits(g, "ggplot"), plots)
  if (!length(plots)) return(list())
  key <- vapply(plots, rlang::obj_address, "")
  plots <- plots[!duplicated(key)]; key <- key[!duplicated(key)]
  jobs <- lapply(seq_along(plots), function(i) list(plot = slim_plot(plots[[i]]), file = file.path(dir, sprintf("fig_%04d.png", i))))
  nw <- if (length(jobs) < 12) 1L else ipm_workers(o, length(jobs))
  progress(sprintf("Drawing %d graphs%s", length(jobs), workers_note(nw)))
  files <- ipm_parallel(jobs, png_job, dpi = o$fig_dpi, workers = nw, progress = progress)
  stats::setNames(as.list(unlist(files)), key)
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
  later <- new.env(); later$tables <- list()                         # tables put into the saved report (add_table_later)
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
  sc <- tryCatch(ipm_scorecard(res), error = function(e) NULL)                 # the one rule engine: summary, tables and plan all read it
  sm <- tryCatch(if (!is.null(sc)) ipm_summary(res, sc) else ipm_summary(res), error = function(e) NULL)
  ht <- ipm_howto()
  allp <- list(); addp <- function(g) if (inherits(g, "ggplot")) allp[[length(allp) + 1]] <<- g
  if (!is.null(sm)) addp(sm$plots$team_module)
  for (id in names(res$outputs)) {
    r <- res$outputs[[id]]
    if (id == "flaganal") { for (z in r$plots) addp(z$plot) } else for (s in r$sections) for (g in s$plots) addp(g)
  }
  drawn <- draw_report_plots(allp, figdir, o, progress)
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
      doc <- add_plot(doc, sm$plots$team_module, figdir, o, drawn)
      doc <- add_text(doc, paste("How to read:", ht$team_module), 8.5, TRUE)
    }
    # sc computed above
    if (!is.null(sc) && nrow(sc$teams)) {
      doc <- officer::body_add_break(doc)
      doc <- add_heading(doc, "Team scorecard and priorities", 11)
      doc <- add_text(doc, "Priority: High = the team as a whole has an excess of flags, or 2 or more high-severity issues; Medium = 1 high-severity issue or 3 or more issues; Low = otherwise. Questionnaire problems (questions flagged in most teams) do not count against teams. Points: high = 3, medium = 1. Details in team_action_plan.docx and data/scorecard_*.csv.", 8, TRUE)
      tb <- sc$teams[, .(Team = team_label, Priority = priority, Points = points, Signals = signals, `% flagged` = round(rate, 1), `x survey rate` = round(ratio, 1),
                         `Modules` = top_modules, `Interviewers of concern` = interviewers_of_concern)]
      tb[is.na(tb)] <- ""
      doc <- add_table_later(doc, ipm_flextable(as.data.frame(tb), 8), later)
    }
    if (!is.null(sm$tables$interviewers) && nrow(sm$tables$interviewers)) {
      doc <- officer::body_add_break(doc)
      doc <- add_heading(doc, "Flag rate by interviewer (highest first)", 11)
      ti <- head(sm$tables$interviewers, 40)
      tb <- data.frame(Team = ti$team, Interviewer = ti$interv, `% flagged` = round(ti$rate, 1), Tests = ti$tests, Flags = ti$flags, check.names = FALSE)
      doc <- add_table_later(doc, ipm_flextable(tb, 8), later)
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
      for (tb in r$tables) { doc <- add_table_later(doc, ipm_flextable(tb, 7), later); doc <- add_text(doc, "", 4) }
      next
    }
    if (id == "flaganal") { doc <- report_flags(doc, r, res, figdir, o, results_dir, progress, drawn, later); next }
    doc <- add_heading(doc, r$title, 12)
    if (!is.null(ht[[id]])) doc <- add_text(doc, paste("How to read:", ht[[id]]), 8.5, TRUE)
    for (s in r$sections) for (g in s$plots) doc <- add_plot(doc, g, figdir, o, drawn)
    if (id == "ant_heap" && !is.null(r$measurers) && nrow(r$measurers)) {
      doc <- officer::body_add_break(doc)
      doc <- add_heading(doc, sprintf("Heaping on height by measurer (%s)", r$measurer_var), 11)
      doc <- add_text(doc, sprintf("Percentage of heights ending in .0 or .5 per measurer, whole fieldwork and period of analysis (%s); about 20%% expected. Measurers well above 20%% with many children measured should be retrained first.", r$period), 8.5, TRUE)
      mt <- r$measurers[, .(Team = team_label, Measurer = fmt_int(measurer), Measured = measured, `% heaping` = round(pct_heaping, 0),
                            `Measured (period)` = measured_period, `% heaping (period)` = round(pct_heaping_period, 0))]
      doc <- add_table_later(doc, ipm_flextable(as.data.frame(mt), 7.5), later)
    }
  }
  out <- file.path(results_dir, "IPM_report.docx")
  print(doc, target = out)
  docx_splice(out, later$tables)
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
  # sc computed above (same scorecard as the summary)
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

IPMR_VERSION <- "0.6.2.9004"   # keep equal to DESCRIPTION (checked by tests/testthat/test-version.R); the shinyapps.io bundle has no DESCRIPTION
utils_pkgver <- function() IPMR_VERSION

report_flags <- function(doc, r, res, figdir, o, results_dir, progress = function(...) NULL, drawn = NULL, later = NULL) {
  doc <- add_heading(doc, "FLAG ANALYSIS OF INTERVIEWER BIAS IN SPECIFIED MODULES AND DATASETS", 12)
  if (is.null(r$tests) || !nrow(r$tests)) return(add_text(doc, "No flag test could be performed (check modules, teams and period of analysis)."))
  ht <- ipm_howto()
  doc <- add_text(doc, sprintf("Period %s. Tables of each flag, with a one-line reading of the pattern, are in the 'flag_results' folder (per dataset and per team).", r$period), 8, TRUE)
  doc <- add_text(doc, paste("What the test does:", ht$flaganal), 8.5, TRUE)
  plots <- r$plots
  summ <- Filter(function(z) z$type == "summary", plots)
  for (z in summ) { doc <- add_plot(doc, z$plot, figdir, o, drawn); doc <- add_text(doc, paste("How to read:", ht$flag_summary), 8.5, TRUE) }
  seen <- character(); fp <- Filter(function(z) z$type != "summary", plots)
  for (k in seq_along(fp)) { z <- fp[[k]]
    if (k %% 5 == 1) progress(sprintf("Writing report: flag graphs %d of %d", k, length(fp)))
    doc <- add_plot(doc, z$plot, figdir, o, drawn)
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
    doc <- if (is.null(later)) flextable::body_add_flextable(doc, ft) else add_table_later(doc, ft, later)
  } else doc <- add_text(doc, "None.")
  write_flag_tables(r, res, results_dir, progress)
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

# ---- Flag tables documents ----
# Hundreds of small tables: adding them one by one with officer gets slower with every table (400 tables: 15 s,
# the Somalia wm file: about 2 minutes). The same paragraphs and tables are written as WordprocessingML text
# and put into the saved document in one step (officer saving a large document is itself slow).
xml_esc <- function(x) {
  x <- gsub("[\x01-\x08\x0B\x0C\x0E-\x1F]", "", enc2utf8(as.character(x)), perl = TRUE)   # characters not allowed in XML
  x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE)
}
wx_col <- function(col) toupper(sub("^#", "", if (grepl("^#", col)) col else grDevices::rgb(t(grDevices::col2rgb(col)), maxColorValue = 255)))
wx_par <- function(txt, size, bold = FALSE, italic = FALSE, color = "#000000")
  sprintf(paste0('<w:p><w:pPr><w:jc w:val="left"/><w:spacing w:after="0" w:before="0" w:line="240"/></w:pPr>',
                 '<w:r><w:rPr><w:rFonts w:ascii="Arial" w:hAnsi="Arial" w:eastAsia="Arial" w:cs="Arial"/><w:i w:val="%s"/><w:b w:val="%s"/>',
                 '<w:sz w:val="%d"/><w:szCs w:val="%d"/><w:color w:val="%s"/></w:rPr><w:t xml:space="preserve">%s</w:t></w:r></w:p>'),
          tolower(italic), tolower(bold), round(size * 2), round(size * 2), wx_col(color), xml_esc(txt))
wx_table <- function(df, tstyle, pstyle) {
  cell <- function(v) paste0('<w:tc><w:p><w:pPr><w:pStyle w:val="', pstyle, '"/></w:pPr><w:r><w:t xml:space="preserve">', xml_esc(v), "</w:t></w:r></w:p></w:tc>")
  m <- vapply(df, function(z) if (is.numeric(z)) format(z, trim = TRUE, scientific = FALSE) else as.character(z), character(nrow(df)))
  if (!is.matrix(m)) m <- matrix(m, nrow = nrow(df))
  rows <- if (nrow(m)) apply(m, 1, function(rw) paste0("<w:tr>", paste0(cell(rw), collapse = ""), "</w:tr>")) else character()
  paste0('<w:tbl><w:tblPr><w:tblStyle w:val="', tstyle, '"/><w:tblLayout w:type="autofit"/><w:jc w:val="center"/><w:tblW w:type="pct" w:w="5000"/>',
         '<w:tblLook w:firstRow="1" w:lastRow="0" w:firstColumn="1" w:lastColumn="0" w:noHBand="0" w:noVBand="1"/></w:tblPr>',
         "<w:tr><w:trPr><w:tblHeader/></w:trPr>", paste0(cell(names(df)), collapse = ""), "</w:tr>", paste0(rows, collapse = ""), "</w:tbl>")
}
# Replace the paragraph holding `marker` in a saved .docx by WordprocessingML text (officer's own saving step
# re-reads the whole document and takes most of the time on large documents, so the tables are put in afterwards)
# repl: named list, marker text -> WordprocessingML (character vector, pasted together)
docx_splice <- function(file, repl) {
  if (!length(repl)) return(invisible(file))
  td <- tempfile("docx_"); dir.create(td); on.exit(unlink(td, recursive = TRUE), add = TRUE)
  zip::unzip(file, exdir = td)
  f <- file.path(td, "word", "document.xml")
  s <- readBin(f, "raw", file.size(f))
  starts <- sort(c(grepRaw("<w:p>", s, fixed = TRUE, all = TRUE), grepRaw("<w:p ", s, fixed = TRUE, all = TRUE)))
  cut <- lapply(names(repl), function(mk) {
    m <- grepRaw(mk, s, fixed = TRUE)
    if (!length(m)) stop("docx_splice: marker not found: ", mk)
    pe <- m + grepRaw("</w:p>", s[m:length(s)], fixed = TRUE) - 1L + 6L        # first byte after the marker's </w:p>
    c(max(starts[starts < m]), pe)
  })
  o <- order(vapply(cut, `[[`, 0, 1)); pieces <- list(); pos <- 1L
  for (k in o) {
    pieces[[length(pieces) + 1]] <- s[pos:(cut[[k]][1] - 1L)]
    pieces[[length(pieces) + 1]] <- charToRaw(enc2utf8(paste0(repl[[k]], collapse = "")))
    pos <- cut[[k]][2]
  }
  pieces[[length(pieces) + 1]] <- s[pos:length(s)]
  writeBin(unlist(pieces, use.names = FALSE), f)
  unlink(file)
  zip::zip(file, files = list.files(td, recursive = TRUE, all.files = TRUE, include.dirs = FALSE), root = td, mode = "mirror")
  invisible(file)
}

# A flextable whose XML is put into the saved document afterwards (see docx_splice): officer's saving step scans
# the whole document once per image, so large tables in a report with 100 graphs made saving slow.
# `later` is an environment collecting marker -> table XML; style names are replaced by style ids as officer does.
add_table_later <- function(doc, ft, later) {
  w <- officer::to_wml(ft, add_ns = FALSE, base_document = doc)
  si <- officer::styles_info(doc)
  for (sn in unique(regmatches(w, gregexpr('w:(pstlname|tstlname)="[^"]+"', w))[[1]])) {
    nm <- sub('^w:(pstlname|tstlname)="([^"]+)"$', "\\2", sn)
    id <- si$style_id[match(nm, si$style_name)]
    if (!is.na(id)) w <- gsub(sn, sprintf('w:val="%s"', id), w, fixed = TRUE)
  }
  mk <- sprintf("@@IPMR_TABLE_%04d@@", length(later$tables) + 1L)
  later$tables[[mk]] <- w
  add_text(doc, mk, 8)
}

write_flag_doc <- function(groups, file, heading, period, survey) {
  doc <- new_doc()
  doc <- add_heading(doc, heading, 13)
  doc <- add_text(doc, sprintf("%s. Survey: %s. %d flagged tests on %d questions. One table per question and team: columns marked * are the flagged interviewer(s); the table shows the answers recorded by all interviewers of the team.",
                               period, survey, sum(vapply(groups, function(g) length(g$interviewers), 0)), length(groups)), 8, TRUE)
  si <- officer::styles_info(doc)
  tstyle <- si$style_id[si$style_type == "table" & si$style_name == "table_template"][1]
  pstyle <- si$style_id[si$style_type == "paragraph" & si$style_name == "Normal"][1]
  parts <- vapply(groups, function(g) paste0(
    wx_par("", 4),
    wx_par(g$title, 9, bold = TRUE, color = "grey10"),
    wx_par(g$desc, 8, italic = TRUE),
    paste0(vapply(seq_along(g$readings), function(k) if (is.na(g$readings[k])) "" else
      wx_par(sprintf("Reading (p=%s): %s", signif(g$p[k], 2), g$readings[k]), 8.5, bold = TRUE, color = unicef_dark), ""), collapse = ""),
    if (length(g$interviewers) > 1) wx_par("Several interviewers of the same team are flagged on this question: their answers differ from each other, which often means the question is understood or asked differently within the team.", 8, italic = TRUE) else "",
    wx_table(g$table, tstyle, pstyle)), "")
  marker <- "@@IPMR_FLAG_TABLES@@"
  doc <- add_text(doc, marker, 8)
  print(doc, target = file)
  docx_splice(file, stats::setNames(list(parts), marker))
  invisible(file)
}

# one flag tables document (a job for ipm_parallel; top-level so that nothing else is sent to the workers)
flag_doc_job <- function(j, fd, period, survey) { write_flag_doc(j$groups, file.path(fd, j$file), j$heading, period, survey); j$file }

write_flag_tables <- function(r, res, results_dir, progress = function(...) NULL) {
  fd <- file.path(results_dir, "flag_results"); dir.create(fd, showWarnings = FALSE)
  old <- list.files(fd, pattern = "\\.docx$", full.names = TRUE); if (length(old)) unlink(old)
  if (!length(r$tables)) return(invisible())
  tl <- res$team_info %||% res$prepared$team_info
  groups <- group_flag_tables(r$tables)
  ds <- vapply(groups, `[[`, "", "dataset"); tm <- vapply(groups, `[[`, 0, "team")
  jobs <- c(lapply(unique(ds), function(d) list(groups = groups[ds == d], file = sprintf("flags_dataset_%s.docx", d), heading = sprintf('Flag tables: "%s" dataset', d))),
            lapply(sort(unique(tm)), function(t) list(groups = groups[tm == t], file = sprintf("flags_team_%d.docx", t),
                                                       heading = sprintf("Flag tables: team %s", tl$team_label[match(t, tl$team)] %||% paste0("T", t)))))
  jobs <- jobs[order(-vapply(jobs, function(j) length(j$groups), 0))]          # largest first
  nw <- if (length(groups) < 5000) 1L else ipm_workers(res$cfg$options, length(jobs))   # about 1 s per 1,000 tables: extra processes only pay off for very large runs
  progress(sprintf("Writing flag tables: %d documents, %d tables%s", length(jobs), length(groups), if (nw > 1) sprintf(" (%d processes)", nw) else ""))
  ipm_parallel(jobs, flag_doc_job, fd = fd, period = r$period, survey = res$cfg$survey_name, workers = nw, progress = progress)
  invisible()
}
