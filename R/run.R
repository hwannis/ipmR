# Orchestrator (port of MASTER.do "Run")

#' Run the IPM application
#'
#' @param cfg configuration list or path to a YAML file
#' @param write write the report and companion files to cfg$results_dir
#' @param progress function(message) called at each step (used by the Shiny app)
#' @param keep_data keep the prepared datasets in the result (set FALSE to save memory)
#' @return invisible list(cfg, prepared, outputs, issues, report)
#' @export
run_ipm <- function(cfg, write = TRUE, progress = message, keep_data = TRUE) {
  if (is.character(cfg)) cfg <- read_config(cfg)
  cfg <- normalise_config(cfg)
  progress("Loading datasets")
  data <- ipm_load(cfg, quiet = TRUE, slim = TRUE)
  issues <- validate_config(cfg, data)
  if (any(issues$level == "error")) {
    stop("Configuration errors:\n", paste(sprintf("- [%s] %s", issues$where, issues$message)[issues$level == "error"], collapse = "\n"), call. = FALSE)
  }
  progress("Preparing analysis files (date, week, interviewer, team)")
  p <- ipm_prepare(data, cfg, quiet = TRUE, inplace = TRUE)
  rm(data); invisible(gc(verbose = FALSE))
  fns <- list(quesperday = out_quesperday, quescrates = out_quescrates, quesmtimes = out_quesmtimes,
              meannohhm = out_meannohhm, everborn = out_everborn, u5deaths = out_u5deaths,
              ant_heap = out_ant_heap, ant_oor = out_ant_oor)
  outputs <- list()
  for (id in names(ipm_output_names)) {
    if (!isTRUE(cfg$outputs[[id]])) next
    progress(paste("Computing:", ipm_output_names[[id]]))
    o_ <- tryCatch(if (id == "flaganal") flag_analysis(p, progress = progress) else fns[[id]](p),
                   error = function(e) { progress(sprintf("WARNING: %s skipped (%s)", ipm_output_names[[id]], conditionMessage(e)))
                     issues <<- rbind(issues, data.frame(level = "warning", where = id, message = paste("Output skipped:", conditionMessage(e)))); NULL })
    if (!is.null(o_)) outputs[[id]] <- o_
  }
  dsum <- rbindlist(lapply(names(p$ds), function(n) { x <- p$ds[[n]]
    data.table(dataset = n, records = nrow(x), first_day = format(min(x$doi, na.rm = TRUE)), last_day = format(max(x$doi, na.rm = TRUE)),
               week1_starts = format(p$stdates[[n]]), interviewers = uniqueN(x$interv), not_in_a_team = uniqueN(x[is.na(team), interv]),
               completed = if ("result" %in% names(x)) sum(x$result %in% cfg$options$interco) else NA_integer_,
               linked_to = cfg$datasets[[n]]$parent %||% "") }))
  res <- list(cfg = cfg, prepared = if (keep_data) p, team_info = p$team_info, data_summary = dsum, outputs = outputs, issues = issues)
  rm(p)
  if (write) {
    progress("Writing report")
    res$report <- ipm_report(res, progress = progress)
    progress(paste("Done:", res$report))
  }
  invisible(res)
}

#' Internal objects used by the Shiny app
#' @export
ipm_internals <- function() list(output_names = ipm_output_names, hhm_defs = hhm_group_defs,
                                 normalise = normalise_config, read_cols = read_any_cols, assign_teams = assign_teams,
                                 flag_doc = write_flag_doc)
