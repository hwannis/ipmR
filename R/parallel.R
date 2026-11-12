# Speed and memory helpers: slim stored charts, and run independent jobs on several cores.

# ---- Slim charts -------------------------------------------------------------------------------------------
# A ggplot keeps the environment it was built in (aes() quosures, facets, plot_env, closures such as
# breaks = function(l) ...). Built inside an output function, that environment holds the prepared data and
# every intermediate table, so one stored chart could weigh hundreds of MB (1.1 GB for all Somalia outputs).
# Each environment is replaced by a small one holding only the free variables the expression uses
# (objects over 1 MB are left out: those names are data columns, which take precedence when the plot is drawn).

slim_env <- function(vars, env, parent) {
  ne <- new.env(parent = parent)
  for (v in unique(vars)) {
    if (!exists(v, envir = env, inherits = TRUE)) next
    val <- get(v, envir = env, inherits = TRUE)
    if (is.function(val) || utils::object.size(val) > 2^20) next
    assign(v, val, envir = ne)
  }
  ne
}
slim_parent <- function(env) if (is.environment(env)) topenv(env) else asNamespace("ipmR")
slim_quo <- function(q) {
  if (!rlang::is_quosure(q)) return(q)
  env <- rlang::quo_get_env(q)
  if (identical(env, emptyenv()) || isNamespace(env) || identical(env, globalenv())) return(q)
  rlang::quo_set_env(q, slim_env(all.vars(rlang::quo_get_expr(q)), env, slim_parent(env)))
}
slim_quos <- function(m) {
  if (!is.list(m) || !length(m)) return(m)
  a <- attributes(m); m <- lapply(m, slim_quo); attributes(m) <- a; m
}
slim_fun <- function(f) {
  if (!is.function(f) || is.primitive(f)) return(f)
  env <- environment(f)
  if (is.null(env) || isNamespace(env) || identical(env, globalenv()) || identical(env, baseenv())) return(f)
  environment(f) <- slim_env(setdiff(all.vars(body(f)), names(formals(f))), env, slim_parent(env)); f
}
# A ggproto object finds its parent with a function `super` whose environment is the frame of the function that
# created it (layer(), facet_wrap(), continuous_scale() ...), and through it the caller's frame with all its data.
# Replace it by a function returning the same parent from a tiny environment (objects created when ggplot2
# was loaded are left alone).
slim_ggproto <- function(o, depth = 0L) {
  if (!is.environment(o) || !inherits(o, "ggproto") || depth > 10L) return(invisible(o))
  fs <- get0("super", envir = o, inherits = FALSE)
  if (is.function(fs)) {
    fe <- environment(fs)
    if (!is.null(fe) && !isNamespace(fe) && !(is.environment(fe$env) && isNamespace(fe$env))) {
      parent <- fs()
      slim_ggproto(parent, depth + 1L)                     # the parent may itself be built in a function (e.g. a scale copied from a scale)
      assign("super", local(function() parent, list2env(list(parent = parent), parent = baseenv())), envir = o)
    }
  }
  for (k in c("breaks", "minor_breaks", "labels", "limits", "data")) {
    f <- get0(k, envir = o, inherits = FALSE)
    if (is.function(f) && !inherits(f, "ggproto_method")) assign(k, slim_fun(f), envir = o)
  }
  invisible(o)
}
gg_get <- function(g, nm) if (inherits(g, "S7_object")) S7::prop(g, nm) else g[[nm]]
gg_set <- function(g, nm, value) { if (inherits(g, "S7_object")) S7::prop(g, nm) <- value else g[[nm]] <- value; g }

#' Remove the hidden copy of the data that a ggplot keeps in its environments
#' @param g a ggplot
#' @return the same plot, drawing identically, with small environments
#' @keywords internal
slim_plot <- function(g) {
  if (!inherits(g, "ggplot")) return(g)
  pe <- gg_get(g, "plot_env")
  g <- gg_set(g, "plot_env", new.env(parent = slim_parent(pe)))
  g <- gg_set(g, "mapping", slim_quos(gg_get(g, "mapping")))
  for (l in gg_get(g, "layers")) {                       # layers, facet and scales are ggproto environments: changed in place
    assign("mapping", slim_quos(get0("mapping", envir = l, inherits = FALSE)), envir = l)
    cm <- grep("^computed_", ls(l, all.names = TRUE), value = TRUE)   # left by an earlier drawing; recomputed at the next one
    if (length(cm)) rm(list = cm, envir = l)
    slim_ggproto(l)
    for (k in c("geom", "stat", "position")) slim_ggproto(get0(k, envir = l, inherits = FALSE))
  }
  fc <- gg_get(g, "facet")
  pr <- get0("params", envir = fc, inherits = FALSE)
  if (is.list(pr)) {
    for (k in intersect(c("facets", "rows", "cols"), names(pr))) pr[[k]] <- slim_quos(pr[[k]])
    if (is.function(pr$labeller)) pr$labeller <- slim_fun(pr$labeller)
    assign("params", pr, envir = fc)
  }
  slim_ggproto(fc)
  slim_ggproto(gg_get(g, "coordinates")); slim_ggproto(gg_get(g, "layout"))
  sc <- gg_get(g, "scales"); slim_ggproto(sc)
  for (s in get0("scales", envir = sc, inherits = FALSE)) slim_ggproto(s)
  g
}

# Walk an output (nested lists) and slim every chart in it
slim_plots <- function(x) {
  if (inherits(x, "ggplot")) return(slim_plot(x))
  if (is.data.frame(x) || !is.list(x)) return(x)
  a <- attributes(x); x <- lapply(x, slim_plots); attributes(x) <- a; x
}

# ---- Several cores -----------------------------------------------------------------------------------------

#' Number of worker processes for the report
#'
#' Option `workers` ("auto" or a number; 1 = no parallel work). "auto" uses one core less than the machine has,
#' at most one worker per GB of free memory, at most 6 (more brings nothing: drawing 97 Somalia graphs took
#' 52 s on 1 process, 22 s on 4, 21 s on 6), and never more than there are jobs.
#' Parallel work needs the installed ipmR package (workers load it); otherwise it falls back to one process.
#' @param o options list
#' @param jobs number of jobs
#' @keywords internal
ipm_workers <- function(o, jobs) {
  w <- o$workers %||% getOption("ipmR.workers", "auto")
  if (!identical(w, "auto")) return(max(1L, min(as.integer(w), jobs)))
  cores <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L); if (is.na(cores)) cores <- 1L
  free_gb <- tryCatch(ps::ps_system_memory()[["avail"]] / 2^30, error = function(e) 4)
  w <- max(1L, min(cores - 1L, floor(free_gb), 6L, jobs))
  if (nzchar(Sys.getenv("_R_CHECK_LIMIT_CORES_")) && !identical(Sys.getenv("_R_CHECK_LIMIT_CORES_"), "false")) w <- min(w, 2L)   # R CMD check
  as.integer(w)
}

# Source folder when ipmR was loaded with pkgload::load_all(), else ""
ipm_dev_path <- function() {
  pth <- tryCatch(getNamespaceInfo("ipmR", "path"), error = function(e) "")
  if (length(pth) == 1 && file.exists(file.path(pth, "R", "parallel.R"))) pth else ""
}
# ipmR must be loadable by a fresh R process for workers to start (not the case in the shinyapps.io bundle)
ipm_can_fork_workers <- function() isNamespaceLoaded("ipmR") && (nzchar(ipm_dev_path()) || nzchar(system.file(package = "ipmR")))

# Run once in each worker (top-level function, so that nothing else is sent with it)
ipm_worker_init <- function(lp, dev) {
  .libPaths(lp)
  if (nzchar(dev) && requireNamespace("pkgload", quietly = TRUE))
    suppressMessages(pkgload::load_all(dev, quiet = TRUE, export_all = FALSE, helpers = FALSE))   # development copy (load_all)
  else loadNamespace("ipmR")
  NULL
}

# Run one job and return its error instead of stopping (top-level: sent to the workers as a reference to ipmR)
ipm_safe_job <- function(job, fun, ...) tryCatch(fun(job, ...), error = function(e) structure(list(message = conditionMessage(e)), class = "ipm_job_error"))

#' Apply a function to jobs, on several worker processes when possible
#'
#' Linux and macOS: forked processes (nothing to copy, no package to load). Windows: background R sessions
#' that load ipmR. A job that fails in a worker, or every job if the workers cannot start, is run again in
#' this process, so the result is always complete. Jobs should be sorted largest first.
#' @param jobs list of jobs
#' @param fun function(job, ...) run on each job; a top-level ipmR function (it is sent to the workers by name)
#' @param ... further arguments for fun
#' @param workers number of workers
#' @param progress function(message)
#' @keywords internal
ipm_parallel <- function(jobs, fun, ..., workers = 1L, progress = function(...) NULL) {
  if (workers <= 1L || length(jobs) <= 1L || !ipm_can_fork_workers()) return(lapply(jobs, fun, ...))
  out <- if (.Platform$OS.type == "unix") {
    progress(sprintf("Using %d forked processes", workers))
    tryCatch(parallel::mclapply(jobs, ipm_safe_job, fun, ..., mc.cores = workers, mc.preschedule = FALSE),
             error = function(e) { progress(sprintf("Parallel work not available (%s)", conditionMessage(e))); NULL })
  } else {
    cl <- tryCatch({ cl <- parallel::makePSOCKcluster(workers); parallel::clusterCall(cl, ipm_worker_init, .libPaths(), ipm_dev_path()); cl },
                   error = function(e) { progress(sprintf("Parallel work not available (%s)", conditionMessage(e))); NULL })
    if (!is.null(cl)) {
      on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
      progress(sprintf("Using %d worker processes", length(cl)))
      tryCatch(parallel::clusterApplyLB(cl, jobs, ipm_safe_job, fun, ...),
               error = function(e) { progress(sprintf("Parallel work stopped (%s)", conditionMessage(e))); NULL })
    }
  }
  if (is.null(out) || length(out) != length(jobs)) out <- vector("list", length(jobs))
  redo <- which(vapply(out, function(z) is.null(z) || inherits(z, c("ipm_job_error", "try-error")), logical(1)))
  if (length(redo) && length(redo) < length(jobs)) progress(sprintf("%d of %d jobs redone in this process", length(redo), length(jobs)))
  for (i in redo) out[[i]] <- fun(jobs[[i]], ...)
  out
}
