synth_res <- function() {
  dir <- make_synth()
  # interviewer value labels carrying names: they must never reach the AI evidence pack
  hh <- haven::read_sav(file.path(dir, "hh.sav"))
  hh$HH3 <- haven::labelled(as.numeric(hh$HH3), c(`Amina Hassan` = 13, `Joseph Otieno` = 22), label = "Interviewer number")
  haven::write_sav(hh, file.path(dir, "hh.sav"))
  cfg <- mics7_autoconfig(dir, "Synthetic")
  cfg$period$start <- "2026-01-05"; cfg$period$days <- 30
  cfg$datasets$hh$flag_modules <- "HC*"; cfg$datasets$hh$flag_units <- "HC4U"
  cfg$results_dir <- file.path(dir, "results")
  run_ipm(cfg, write = FALSE, progress = function(m) NULL, keep_data = FALSE)
}

test_that("scorecard finds the planted biased interviewers and sets priorities", {
  res <- synth_res()
  sc <- ipm_scorecard(res)
  expect_s3_class(sc, "ipm_scorecard")
  expect_equal(nrow(sc$teams), 3)
  expect_true(all(sc$teams$priority %in% c("High", "Medium", "Low")))
  expect_true(all(c(13, 22) %in% sc$interviewers[flags > 0]$interv))
  expect_equal(sum(sc$teams$flags), sc$survey$flags)
  expect_true(all(c("evidence", "action", "owner", "timing") %in% names(sc$signals)))
})

test_that("rule-based plan passes validation and the evidence pack has no names or record-level data", {
  res <- synth_res(); sc <- ipm_scorecard(res)
  ev <- ipm_ai_evidence(res, sc, team = sc$teams$team_label[1])
  j <- as.character(jsonlite::toJSON(ev, auto_unbox = TRUE, null = "null"))
  expect_false(grepl("Amina|Hassan|Otieno", j))
  expect_false(any(c("HH1", "HH2", "doi", "records_data") %in% names(unlist(ev))))
  pl <- ipm_plan_rules(sc)
  expect_identical(pl$source, "rules")
  expect_length(ipm_validate_plan(pl, sc, j)$validation, 0)
})

test_that("validation removes unknown teams and flags invented interviewers and numbers", {
  res <- synth_res(); sc <- ipm_scorecard(res)
  j <- as.character(jsonlite::toJSON(ipm_ai_evidence(res, sc), auto_unbox = TRUE, null = "null"))
  t1 <- sc$teams$team_label[1]
  plan <- list(teams = list(
    list(team = t1, priority = "High", headline = "x", issues = list(list(issue = "a", evidence = "93.7% of tests flagged")),
         actions = list(), interviewers_to_observe = list(list(interviewer = "999", reason = "r")), modules_to_retrain = list()),
    list(team = "T77", priority = "Low", headline = "", issues = list(), actions = list(), interviewers_to_observe = list(), modules_to_retrain = list())))
  v <- ipm_validate_plan(plan, sc, j)
  expect_length(v$teams, 1)
  expect_true(isTRUE(v$teams[[1]]$interviewers_to_observe[[1]]$unverified))
  expect_true("93.7" %in% v$teams[[1]]$unverified)
  expect_true(any(grepl("T77", v$validation)))
})

test_that("AI plan and questions work end to end with a mocked Anthropic API", {
  skip_if_not_installed("httr2")
  res <- synth_res(); sc <- ipm_scorecard(res)
  canned <- as.character(jsonlite::toJSON(ipm_plan_rules(sc), auto_unbox = TRUE, null = "null"))
  seen <- new.env()
  mock <- function(req) {
    seen$url <- req$url; seen$body <- req$body$data; seen$headers <- req$headers
    b <- jsonlite::fromJSON(rawToChar(if (is.raw(req$body$data)) req$body$data else charToRaw(req$body$data)), simplifyVector = FALSE)
    txt <- if (!is.null(b$output_config)) canned else "Team answer"
    httr2::response_json(200, body = list(content = list(list(type = "text", text = txt)), stop_reason = "end_turn",
                                          model = b$model, usage = list(input_tokens = 10, output_tokens = 5)))
  }
  st <- ipm_ai_settings(provider = "anthropic", api_key = "k", base_url = "https://api.example.org")
  pl <- httr2::with_mocked_responses(mock, ipm_ai_plan(res, sc, st, language = "French"))
  expect_identical(pl$source, "ai")
  expect_equal(length(pl$teams), length(ipm_plan_rules(sc)$teams))
  expect_match(seen$url, "/v1/messages$")
  body <- jsonlite::fromJSON(rawToChar(if (is.raw(seen$body)) seen$body else charToRaw(seen$body)), simplifyVector = FALSE)
  expect_identical(body$output_config$format$type, "json_schema")
  expect_true(is.list(body$output_config$format$schema$required))           # arrays stay arrays
  expect_match(body$system, "French")
  a <- httr2::with_mocked_responses(mock, ipm_ai_ask("What about team 1?", res, sc, st))
  expect_identical(a$answer, "Team answer")
})

test_that("without a provider the plan falls back to rules and ask() refuses", {
  res <- synth_res(); sc <- ipm_scorecard(res)
  st <- ipm_ai_settings(provider = "none")
  expect_false(st$ready)
  expect_identical(ipm_ai_plan(res, sc, st)$source, "rules")
  expect_error(ipm_ai_ask("x", res, sc, st), "No AI provider")
  f <- tempfile(fileext = ".docx"); ipm_plan_docx(ipm_plan_rules(sc), f, sc, "Synthetic", sc$survey$period)
  expect_true(file.size(f) > 5000)
})

test_that("several server connections can be configured and the model chosen", {
  withr_env <- function(vars, code) { old <- Sys.getenv(names(vars), unset = NA); do.call(Sys.setenv, as.list(vars))
    on.exit({ for (n in names(old)) if (is.na(old[[n]])) Sys.unsetenv(n) else do.call(Sys.setenv, stats::setNames(list(old[[n]]), n)) }); code }
  withr_env(c(ANTHROPIC_API_KEY = "k", IPM_AI_OLLAMA_URL = "http://box:11434/v1", IPM_AI_PROVIDER = "ollama", IPM_AI_MODEL = "qwen2.5:14b"), {
    cs <- ipm_ai_connections()
    expect_identical(names(cs), c("ollama", "anthropic"))                 # default first
    expect_identical(cs$ollama$base_url, "http://box:11434")              # /v1 removed for the native API
    expect_identical(cs$ollama$model, "qwen2.5:14b")
    expect_identical(cs$anthropic$model, "claude-sonnet-5-5")
    expect_identical(ipm_ai_settings(model = "llama3.1:8b")$model, "llama3.1:8b")
    expect_false(grepl("\\bk\\b", paste(utils::capture.output(print(cs$anthropic)), collapse = "")))   # key never printed
  })
  expect_false(ipm_ai_settings("ollama", base_url = "http://box:11434")$ready)                        # no model chosen yet
})

test_that("Ollama native API: context sized to the evidence, JSON schema passed as format", {
  skip_if_not_installed("httr2")
  res <- synth_res(); sc <- ipm_scorecard(res)
  canned <- as.character(jsonlite::toJSON(ipm_plan_rules(sc), auto_unbox = TRUE, null = "null"))
  seen <- new.env()
  mock <- function(req) {
    if (grepl("/api/tags$", req$url)) return(httr2::response_json(200, body = list(models = list(list(name = "small:3b", size = 2e9), list(name = "big:14b", size = 9e9)))))
    b <- jsonlite::fromJSON(rawToChar(if (is.raw(req$body$data)) req$body$data else charToRaw(req$body$data)), simplifyVector = FALSE)
    seen$b <- b; seen$url <- req$url
    httr2::response_json(200, body = list(model = b$model, message = list(role = "assistant", content = if (!is.null(b$format)) canned else "OK"),
                                          done = TRUE, done_reason = "stop", prompt_eval_count = 900, eval_count = 100))
  }
  st <- ipm_ai_settings("ollama", base_url = "http://box:11434")
  expect_identical(httr2::with_mocked_responses(mock, ipm_ai_models(st)), c("big:14b", "small:3b"))   # largest first
  st <- ipm_ai_settings("ollama", model = "big:14b", base_url = "http://box:11434")
  pl <- httr2::with_mocked_responses(mock, ipm_ai_plan(res, sc, st))
  expect_match(seen$url, "/api/chat$")
  expect_true(seen$b$options$num_ctx >= 8192)
  expect_identical(seen$b$format$type, "object")
  expect_false(isTRUE(seen$b$stream))
  expect_identical(pl$model, "big:14b")
  expect_true(httr2::with_mocked_responses(mock, ipm_ai_test(st))$ok)
})
