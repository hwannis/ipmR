# Phase 2 (speed): slim charts, fast flag documents, parallel jobs
test_that("a stored chart no longer carries the data of the function that built it, and draws the same", {
  f <- function() { big <- stats::rnorm(2e6); d <- data.frame(x = 1:10, y = (1:10)^2); lim <- 3
    ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point() + ggplot2::facet_wrap(~ x > lim) +
      ggplot2::scale_x_continuous(breaks = function(l) unique(pmax(lim, round(pretty(l, 4))))) }
  g <- f()
  size <- function(x) length(serialize(x, NULL)) / 2^20
  expect_gt(size(g), 15)
  a <- tempfile(fileext = ".png"); b <- tempfile(fileext = ".png")
  ipmR:::save_plot_png(g, a, dpi = 40)
  s <- ipmR:::slim_plot(g)
  expect_lt(size(s), 8)
  ipmR:::save_plot_png(s, b, dpi = 40)
  expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(b)))
  s2 <- unserialize(serialize(s, NULL)); c2 <- tempfile(fileext = ".png")   # as sent to a worker
  ipmR:::save_plot_png(s2, c2, dpi = 40)
  expect_identical(unname(tools::md5sum(a)), unname(tools::md5sum(c2)))
})

test_that("run outputs keep slim charts", {
  cfg <- mics_autoconfig(make_synth(), "Synthetic"); cfg$period$start <- "2026-01-05"; cfg$period$days <- 28
  res <- run_ipm(cfg, write = FALSE, progress = function(...) NULL, keep_data = FALSE)
  for (id in names(res$outputs)) expect_lt(length(serialize(res$outputs[[id]], NULL)) / 2^20, 60, label = id)
})

test_that("flag tables documents hold one table per flagged question and team, readable by officer", {
  dir <- make_synth(); cfg <- mics_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 28; cfg$datasets$hh$flag_modules <- "HC*"
  for (k in names(cfg$outputs)) cfg$outputs[[k]] <- k == "flaganal"
  cfg$results_dir <- file.path(dir, "out")
  res <- run_ipm(cfg, write = TRUE, progress = function(...) NULL, keep_data = FALSE)
  groups <- ipmR::group_flag_tables(res$outputs$flaganal$tables)
  f <- list.files(file.path(dir, "out", "flag_results"), pattern = "^flags_dataset_.*docx$", full.names = TRUE)
  expect_length(f, 1)
  doc <- officer::read_docx(f)
  expect_length(xml2::xml_find_all(doc$doc_obj$get(), "//w:tbl"), length(groups))
  txt <- paste(officer::docx_summary(doc)$text, collapse = " ")
  expect_match(txt, "Reading \\(p=")
  expect_false(grepl("@@IPMR_FLAG_TABLES@@", txt, fixed = TRUE))
  expect_match(txt, "HC2 (hh)", fixed = TRUE)
})

test_that("the main report has its tables in place of the placeholders", {
  dir <- make_synth(); cfg <- mics_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 28; cfg$datasets$hh$flag_modules <- "HC*"
  cfg$results_dir <- file.path(dir, "out")
  res <- run_ipm(cfg, write = TRUE, progress = function(...) NULL, keep_data = FALSE)
  doc <- officer::read_docx(file.path(dir, "out", "IPM_report.docx"))
  s <- officer::docx_summary(doc)
  expect_false(any(grepl("@@IPMR_", s$text, fixed = TRUE)))
  expect_gte(length(xml2::xml_find_all(doc$doc_obj$get(), "//w:tbl")), 3)
  expect_true(any(s$content_type == "table cell" & s$text == "Interviewer"))   # flag rate by interviewer table
  expect_gt(length(xml2::xml_find_all(doc$doc_obj$get(), "//a:blip", xml2::xml_ns(doc$doc_obj$get()))), 5)
})

test_that("special characters in labels give a valid document", {
  g <- list(list(dataset = "hh", team = 1, variable = "Q1", module = "Q*", desc = "Score < 5 & \"quoted\" été – \x01ctrl",
                 test = "chi2-test", interviewers = 11, p = 1e-5, readings = "Interviewer 11 recorded '1. <A&B>'",
                 title = "Q1 (hh) - flagged interviewer: 11", table = data.frame(Value = c("1. <A&B>", "Total"), `11 Ann *` = c(4, 4), Total = c(9, 9), check.names = FALSE)))
  f <- tempfile(fileext = ".docx")
  ipmR:::write_flag_doc(g, f, "Flag tables", "period", "survey")
  s <- officer::docx_summary(officer::read_docx(f))
  expect_true(any(grepl("Score < 5 & \"quoted\" été – ctrl", s$text, fixed = TRUE)))
  expect_true(any(s$text == "1. <A&B>"))
})

test_that("parallel jobs give the same result as one process, and a failing job is redone locally", {
  jobs <- as.list(1:6)
  expect_equal(ipmR:::ipm_parallel(jobs, sqrt, workers = 2L), lapply(jobs, sqrt))
  expect_equal(ipmR:::ipm_parallel(jobs, sqrt, workers = 1L), lapply(jobs, sqrt))
  pid <- Sys.getpid(); only_here <- function(j) if (Sys.getpid() != pid) stop("fails in a worker") else j * 2
  msgs <- character()
  expect_equal(ipmR:::ipm_parallel(jobs, only_here, workers = 2L, progress = function(m) msgs <<- c(msgs, m)), lapply(jobs, function(j) j * 2))
  expect_equal(ipmR:::ipm_workers(list(workers = 3), 10), 3L)
  expect_equal(ipmR:::ipm_workers(list(workers = 3), 2), 2L)
  expect_true(ipmR:::ipm_workers(list(workers = "auto"), 100) %in% 1:6)
})
