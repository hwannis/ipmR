# Launcher for the installed package (not included in the shinyapps.io bundle)

#' Launch the IPM Shiny application
#' @param data_dir optional folder with survey datasets to pre-load
#' @param ... passed to shiny::runApp()
#' @export
run_ipm_app <- function(data_dir = NULL, ...) {
  for (pk in c("shiny", "bslib", "DT")) if (!requireNamespace(pk, quietly = TRUE)) stop("Install package '", pk, "' to use the app")
  app <- system.file("app", package = "ipmR")
  if (!nzchar(app)) stop("App folder not found; reinstall ipmR")
  options(ipmR.local = TRUE)
  if (!is.null(data_dir)) options(ipmR.data_dir = normalizePath(data_dir))
  shiny::runApp(app, ...)
}

