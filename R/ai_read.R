# ---------------------------------------------------------------- AI reading of one table or one graph

# The figures behind a ggplot: its data (or its layers' data), without plotting positions; reference lines kept as notes
ai_plot_data <- function(g, max_rows = 300) {
  drop <- c("x", "yf", "panel", "row", "col", "cat", "pattern", "test", "var_cat", "n_var", "flag100", "level", "ylab", "k")
  notes <- character()
  base <- is.data.frame(g$data) && nrow(g$data) > 0
  frames <- if (base) list(g$data) else list()
  for (l in g$layers) {
    d <- l$data; if (!is.data.frame(d) || !nrow(d)) next
    ref <- intersect(names(d), c("xintercept", "yintercept"))
    if (nrow(d) == 1 && length(ref)) { notes <- c(notes, sprintf("Reference line (%s) at %s", sub("intercept", "", ref[1]), format(round(d[[ref[1]]], 2)))); next }
    if (nrow(d) == 1 && all(c("xmin", "xmax") %in% names(d))) { notes <- c(notes, sprintf("Shaded weeks: %s to %s", d$xmin + 0.5, d$xmax - 0.5)); next }
    if (base && all(names(d) %in% names(g$data))) next                         # a subset of the plot data
    if (any(vapply(frames, identical, logical(1), d))) next                    # the same data drawn twice (tiles and their text)
    frames[[length(frames) + 1]] <- d
  }
  lab <- g$labels
  notes <- c(notes, unlist(lab[intersect(c("title", "subtitle", "caption", "x", "y"), names(lab))][vapply(lab[intersect(c("title", "subtitle", "caption", "x", "y"), names(lab))], is.character, logical(1))]))
  out <- lapply(frames, function(d) {
    d <- as.data.frame(d)
    d <- d[, vapply(d, function(v) is.atomic(v) && !is.list(v), logical(1)), drop = FALSE]
    if ("interv_label" %in% names(d)) d$interv <- NULL
    if ("team_label" %in% names(d)) d$team <- NULL
    d <- d[, setdiff(names(d), drop), drop = FALSE]
    d <- d[, vapply(d, function(v) !all(is.na(v)), logical(1)), drop = FALSE]
    if ("flag" %in% names(d) && nrow(d) > max_rows / 2) {        # many tests: send the flagged ones
      notes <<- c(notes, sprintf("%d tests, %d flagged: only the flagged tests are listed", nrow(d), sum(d$flag == 1, na.rm = TRUE)))
      d <- d[d$flag %in% 1, , drop = FALSE]; d$flag <- NULL
    }
    unique(d)
  })
  list(data = out, notes = unique(notes))
}

ai_table_rows <- function(d, max_rows = 300) {
  d <- as.data.frame(d)
  for (n in names(d)) {
    v <- d[[n]]
    if (inherits(v, "Date")) d[[n]] <- format(v, "%Y-%m-%d")
    else if (is.factor(v)) d[[n]] <- as.character(v)
    else if (is.numeric(v) && !is.integer(v)) d[[n]] <- if (n == "p") signif(v, 3) else round(v, 1)
  }
  cut <- nrow(d) > max_rows
  list(rows = utils::head(d, max_rows), note = if (cut) sprintf("first %d of %d rows", max_rows, nrow(d)))
}

ai_read_prompt <- function(title, x, how = NULL, notes = NULL, language = "English", max_rows = 300) {
  if (inherits(x, "ggplot")) { pd <- ai_plot_data(x, max_rows); x <- pd$data; notes <- c(notes, pd$notes) }
  if (is.data.frame(x)) x <- list(x)
  tabs <- lapply(x, ai_table_rows, max_rows = max_rows)
  notes <- c(notes, unlist(lapply(tabs, `[[`, "note")))
  ev <- list(title = title, how_to_read = how, notes = notes, tables = lapply(tabs, `[[`, "rows"))
  system <- paste(
    "You are a senior MICS (Multiple Indicator Cluster Surveys) fieldwork data-quality expert.",
    "You read one table or one graph of the Interviewer Performance Monitoring (IPM) for the survey coordinator, during fieldwork.",
    "You receive only the figures behind that table or graph: aggregated counts, rates, medians or test results per team, interviewer, week, day or question; no records.",
    "Rules:",
    "1. Use only the figures provided. Copy numbers exactly; do not compute new averages or totals.",
    "2. Name teams and interviewers exactly as given.",
    "3. Say what stands out: unusual teams, interviewers, weeks, days or questions, trends, gaps (missing days, small numbers).",
    "4. Flags and unusual values are signals to verify, not proof of misconduct.",
    "5. End with 1-3 concrete things to check.",
    sprintf("6. Write in %s, in short markdown bullet points (at most 10).", language), sep = "\n")
  user <- paste0("Table or graph (JSON):\n", ai_json(ev), "\n\nRead it for the survey coordinator.")
  list(system = system, user = user, max_tokens = 2000, evidence = ev)
}

#' AI reading of one table or one graph
#'
#' Sends only the figures behind the table or graph (aggregated, never the records) with its title and how-to-read note,
#' and returns a short reading: what stands out and what to check.
#' @param title title of the table or graph
#' @param x a data frame, a list of data frames, or a ggplot (its data, without plotting positions; many flag tests: the flagged ones)
#' @param how how-to-read note shown with the table or graph
#' @param st AI settings from ipm_ai_settings()
#' @param language answer language
#' @param max_rows rows sent per table
#' @return list(answer (markdown), usage, model, sent (characters sent))
#' @export
ipm_ai_read <- function(title, x, how = NULL, st = ipm_ai_settings(), language = "English", max_rows = 300) {
  if (!isTRUE(st$ready)) stop("No AI provider is configured.", call. = FALSE)
  pr <- ai_read_prompt(title, x, how, language = language, max_rows = max_rows)
  if (st$provider == "mock") attr(st, "mock_text") <- sprintf("Mock reading of %s.", title)
  out <- ipm_ai_call(st, pr$system, pr$user, max_tokens = pr$max_tokens)
  list(answer = out$text, usage = out$usage, model = out$model, sent = nchar(pr$user))
}
