# AI layer: turns the scorecard into a team-by-team action plan and answers questions on the results.
# Only aggregated statistics leave the server (no record-level data, no names): see ipm_ai_evidence().
# Providers: Anthropic Messages API, Ollama (native API, local models), any OpenAI-compatible endpoint, or none.

# ---------------------------------------------------------------- settings

ai_env <- function(x, d = "") { v <- Sys.getenv(x, ""); if (nzchar(v)) v else d }
ai_providers <- c(anthropic = "Claude (Anthropic API)", ollama = "Local model (Ollama)", openai = "OpenAI-compatible endpoint (vLLM, LM Studio, OpenAI...)",
                  mock = "Test mode", none = "None")
ai_default_url <- list(anthropic = "https://api.anthropic.com", ollama = "http://localhost:11434", openai = "", mock = "", none = "")

#' AI connection settings
#'
#' Builds the settings for one AI connection. Without arguments, returns the default connection
#' configured on the server (see \code{ipm_ai_connections()}).
#' Providers: "anthropic" (Claude API), "ollama" (local models through the native Ollama API, which lets
#' the app size the context window to the evidence), "openai" (any OpenAI-compatible endpoint: vLLM,
#' LM Studio, llama.cpp server, OpenAI, Azure gateways), "none".
#' @param provider "anthropic", "ollama", "openai" or "none"
#' @param model model name (e.g. "claude-sonnet-5-5", "qwen2.5:14b")
#' @param api_key API key (Anthropic; optional for OpenAI-compatible endpoints; not used by Ollama)
#' @param base_url endpoint (default https://api.anthropic.com, http://localhost:11434 for Ollama; required for "openai", e.g. http://host:8000/v1)
#' @param label display name
#' @export
ipm_ai_settings <- function(provider = NULL, model = NULL, api_key = NULL, base_url = NULL, label = NULL) {
  if (is.null(provider) && is.null(api_key) && is.null(base_url)) {
    cs <- ipm_ai_connections()
    st <- if (length(cs)) cs[[1]] else ipm_ai_settings("none")
    return(if (is.null(model)) st else ipm_ai_settings(st$provider, model, st$api_key, st$base_url, st$label))
  }
  prov <- tolower(provider %||% "anthropic")
  if (!prov %in% names(ai_providers)) stop("AI provider must be one of: anthropic, ollama, openai, none", call. = FALSE)
  base <- sub("/+$", "", base_url %||% ai_default_url[[prov]])
  if (prov == "ollama") base <- sub("/v1$", "", base)
  key <- api_key %||% ""
  mdl <- model %||% switch(prov, anthropic = "claude-sonnet-5-5", mock = "mock", "")
  structure(list(provider = prov, model = mdl, api_key = key, base_url = base, label = label %||% ai_providers[[prov]],
                 allow_user_key = ipm_ai_allow_custom(),
                 timeout = as.numeric(ai_env("IPM_AI_TIMEOUT", if (prov == "ollama") "900" else "300")),
                 ready = prov == "mock" || (prov == "anthropic" && nzchar(key) && nzchar(mdl)) || (prov %in% c("ollama", "openai") && nzchar(base) && nzchar(mdl))),
            class = "ipm_ai_settings")
}

#' @export
print.ipm_ai_settings <- function(x, ...) {
  cat(sprintf("<AI connection> %s | model: %s | url: %s | key: %s | ready: %s\n", x$label, if (nzchar(x$model)) x$model else "(none)",
              if (nzchar(x$base_url)) x$base_url else "-", if (nzchar(x$api_key)) "set (hidden)" else "none", x$ready))
  invisible(x)
}

#' Users may define their own AI connection for their session (IPM_AI_ALLOW_CUSTOM, default true)
#' @export
ipm_ai_allow_custom <- function() tolower(ai_env("IPM_AI_ALLOW_CUSTOM", ai_env("IPM_AI_ALLOW_USER_KEY", "true"))) %in% c("true", "1", "yes")

#' AI connections configured on the server
#'
#' Read from environment variables (server-side; keys are never sent to the browser). Several connections
#' can be defined at the same time; the app lets users choose the connection and the model:
#' \itemize{
#'   \item Claude: \code{ANTHROPIC_API_KEY} (optional \code{IPM_AI_ANTHROPIC_MODEL}, \code{IPM_AI_ANTHROPIC_URL})
#'   \item Ollama: \code{IPM_AI_OLLAMA_URL}, e.g. http://192.168.1.10:11434 (optional \code{IPM_AI_OLLAMA_MODEL})
#'   \item OpenAI-compatible: \code{IPM_AI_OPENAI_URL}, e.g. http://host:8000/v1 (optional \code{IPM_AI_OPENAI_KEY}, \code{IPM_AI_OPENAI_MODEL})
#'   \item \code{IPM_AI_PROVIDER}: connection selected by default; \code{IPM_AI_MODEL}: its default model
#' }
#' Older settings \code{IPM_AI_BASE_URL} and \code{IPM_AI_API_KEY} are still read.
#' @return named list of ipm_ai_settings (default first)
#' @export
ipm_ai_connections <- function() {
  def <- tolower(ai_env("IPM_AI_PROVIDER")); dm <- ai_env("IPM_AI_MODEL")
  old_url <- ai_env("IPM_AI_BASE_URL"); old_key <- ai_env("IPM_AI_API_KEY")
  pick <- function(p, own) if (def == p && nzchar(dm)) dm else ai_env(own)
  cs <- list()
  akey <- ai_env("ANTHROPIC_API_KEY", if (def == "anthropic" || (def == "" && !nzchar(old_url))) old_key else "")
  am <- pick("anthropic", "IPM_AI_ANTHROPIC_MODEL")
  if (nzchar(akey)) cs$anthropic <- ipm_ai_settings("anthropic", if (nzchar(am)) am else "claude-sonnet-5-5", akey,
                                                    ai_env("IPM_AI_ANTHROPIC_URL", if (def == "anthropic" && nzchar(old_url)) old_url else "https://api.anthropic.com"))
  ourl <- ai_env("IPM_AI_OLLAMA_URL", if (def == "ollama") old_url else "")
  if (nzchar(ourl)) cs$ollama <- ipm_ai_settings("ollama", pick("ollama", "IPM_AI_OLLAMA_MODEL"), "", ourl)
  purl <- ai_env("IPM_AI_OPENAI_URL", if (def == "openai" || (!nzchar(def) && nzchar(old_url))) old_url else "")
  if (nzchar(purl)) cs$openai <- ipm_ai_settings("openai", pick("openai", "IPM_AI_OPENAI_MODEL"), ai_env("IPM_AI_OPENAI_KEY", if (def == "openai") old_key else ""), purl)
  if (def == "mock") cs$mock <- ipm_ai_settings("mock")
  if (def %in% names(cs)) cs <- c(cs[def], cs[setdiff(names(cs), def)])
  cs
}

ai_get <- function(st, path, query = list()) {
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Package httr2 is required for the AI features.", call. = FALSE)
  r <- httr2::request(paste0(st$base_url, path)) |> httr2::req_timeout(20)
  if (length(query)) r <- do.call(httr2::req_url_query, c(list(r), query))
  if (st$provider == "anthropic") r <- httr2::req_headers(r, `x-api-key` = st$api_key, `anthropic-version` = "2023-06-01")
  else if (nzchar(st$api_key)) r <- httr2::req_auth_bearer_token(r, st$api_key)
  resp <- tryCatch(httr2::req_perform(r), error = function(e) stop("Cannot reach ", st$base_url, ": ", conditionMessage(e), call. = FALSE))
  httr2::resp_body_json(resp, simplifyVector = FALSE)
}

#' Models available on a connection
#' @param st settings from ipm_ai_settings()
#' @return character vector of model names (Ollama: largest first)
#' @export
ipm_ai_models <- function(st) {
  switch(st$provider,
    mock = "mock", none = character(),
    anthropic = { b <- ai_get(st, "/v1/models", list(limit = 100)); vapply(b$data, function(m) m$id, "") },
    ollama = { b <- ai_get(st, "/api/tags")                                     # largest model first: better plans
               nm <- vapply(b$models, function(m) m$name, ""); sz <- vapply(b$models, function(m) as.numeric(m$size %||% 0), 0); nm[order(-sz, nm)] },
    openai = { b <- ai_get(st, "/models"); sort(vapply(b$data, function(m) m$id, "")) })
}

#' Test a connection with a one-word request
#' @param st settings from ipm_ai_settings()
#' @return list(ok, message, seconds)
#' @export
ipm_ai_test <- function(st) {
  t0 <- Sys.time()
  r <- tryCatch(ipm_ai_call(st, "You are a connection test.", "Reply with the single word OK.", max_tokens = 300), error = function(e) e)
  sec <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (inherits(r, "error")) list(ok = FALSE, message = conditionMessage(r), seconds = sec)
  else list(ok = TRUE, message = sprintf("%s answered '%s' in %s s.", r$model, substr(trimws(r$text), 1, 30), sec), seconds = sec)
}

# ---------------------------------------------------------------- evidence pack

#' Evidence pack sent to the AI
#'
#' Aggregated statistics only: survey totals, the team and interviewer scorecards, modules and questions
#' with their flag rates, the warning signs with their evidence, and one-line readings of the main flagged
#' questions of the priority teams. Interviewers are identified by their code; no record-level data,
#' household identifiers, names or free text from the questionnaires are included.
#' @param res result of run_ipm()
#' @param sc result of ipm_scorecard()
#' @param max_teams maximum number of teams detailed (highest priority first)
#' @param team optional team label: adds that team's flag tables (answer counts per interviewer) for questions about it
#' @export
ipm_ai_evidence <- function(res, sc = ipm_scorecard(res), max_teams = 40, team = NULL) {
  r1 <- function(x) round(x, 1)
  s <- sc$survey
  tms <- head(sc$teams, max_teams)
  sig <- sc$signals
  fa <- res$outputs$flaganal
  readings <- if (!is.null(fa$tests) && nrow(fa$tests)) {
    f <- fa$tests[flag == 1 & !is.na(pattern)]; f[, team_label := sc$teams$team_label[match(team, sc$teams$team)]]
    f[order(p)][, head(.SD, 8), by = team]
  } else NULL
  teams <- lapply(seq_len(nrow(tms)), function(i) {
    h <- tms[i]; ts <- sig[team == h$team]
    list(team = h$team_label, priority_rule_based = h$priority, warning_points = h$points,
         flag_tests = h$tests %||% NA, flags = h$flags %||% NA, pct_flagged = r1(h$rate %||% NA), ratio_to_survey_rate = r1(h$ratio %||% NA),
         flags_expected_at_survey_rate = r1(h$expected %||% NA), interviewers_tested = h$interviewers_tested %||% NA,
         top_modules = h$top_modules %||% NA, interviewers_of_concern = h$interviewers_of_concern %||% NA,
         signals = lapply(seq_len(nrow(ts)), function(j) list(type = ts$type[j], severity = ts$severity[j],
           interviewer = if (is.na(ts$interv[j])) NULL else fmt_int(ts$interv[j]),
           module = if (is.na(ts$module[j])) NULL else paste(ts$dataset[j], ts$module[j]), evidence = ts$evidence[j])),
         flagged_question_readings = if (!is.null(readings)) {
           rr <- readings[team == h$team]
           lapply(seq_len(nrow(rr)), function(j) list(question = paste(rr$dataset[j], rr$varname[j]), label = substr(rr$vardesc[j], 1, 80),
                                                      interviewer = fmt_int(rr$interv[j]), reading = rr$pattern[j]))
         })
  })
  ivs <- if (!is.null(sc$interviewers)) head(sc$interviewers[signals > 0 | ratio >= 2], 60) else NULL
  ev <- list(
    survey = list(name = s$name, period_of_analysis = s$period, records = s$records, datasets = s$datasets, teams = s$teams,
                  flag_tests = s$tests, flags = s$flags, pct_flagged = r1(s$flag_rate %||% NA), chance_level_pct = s$levelsig * 100,
                  ratio_to_chance = r1(s$ratio_to_chance %||% NA), descriptive_signals_cover_last_weeks = s$recent_weeks),
    method = c("Flag analysis: for each question and interviewer, answers are compared with the rest of the same team (chi-square or t-test); flag = p < 0.001, so about 0.1% of tests are flagged by chance alone.",
               "A flag is a statistical signal, not proof: it may reflect a misunderstood or misread question, misuse of answer categories, skipped questions, leading respondents, or genuinely different respondents.",
               "Questions flagged in many teams usually point to a question, translation or training problem rather than to interviewers.",
               "priority_rule_based and warning_points come from a transparent rule set (high-severity signal = 3 points, medium = 1); you may adjust priorities if the evidence justifies it."),
    teams = teams,
    interviewers = if (!is.null(ivs) && nrow(ivs)) lapply(seq_len(nrow(ivs)), function(i) list(team = ivs$team_label[i], interviewer = fmt_int(ivs$interv[i]),
      flag_tests = ivs$tests[i], flags = ivs$flags[i], pct_flagged = r1(ivs$rate[i]), ratio_to_survey_rate = r1(ivs$ratio[i]),
      signal_types = if (is.na(ivs$signal_types[i])) NULL else ivs$signal_types[i])),
    modules = if (!is.null(sc$modules)) { m <- head(sc$modules[tests >= 30], 25)
      lapply(seq_len(nrow(m)), function(i) list(module = paste(m$dataset[i], m$module[i]), flag_tests = m$tests[i], pct_flagged = r1(m$rate[i]),
                                                ratio_to_survey_rate = r1(m$ratio[i]), teams_flagged = m$teams_flagged[i], teams = m$teams[i], scope = m$scope[i])) },
    survey_wide_signals = { w <- sig[level == "survey"]; lapply(seq_len(min(nrow(w), 30)), function(i) list(type = w$type[i], severity = w$severity[i], evidence = w$evidence[i])) },
    standard_actions = lapply(ipm_action_library(), function(a) a[c("action", "owner", "timing")])
  )
  if (!is.null(team) && !is.null(fa$tables)) {
    tn <- sc$teams$team[match(team, sc$teams$team_label)]
    tb <- Filter(function(z) identical(as.integer(z$team), as.integer(tn)), fa$tables)
    ev$team_flag_tables <- lapply(head(tb, 25), function(z) list(question = paste(z$dataset, z$variable), label = unname(z$desc), flagged_interviewer = fmt_int(z$interv),
                                                                 p = signif(z$p, 2), reading = z$pattern,
                                                                 answer_counts = utils::capture.output(print(z$table, row.names = FALSE))))
  }
  ev
}

ai_json <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA, pretty = FALSE)

# ---------------------------------------------------------------- output schema

ipm_plan_schema <- function() jsonlite::fromJSON('{
  "type": "object", "additionalProperties": false,
  "required": ["overall_assessment", "survey_wide_issues", "most_problematic_modules", "teams", "caveats"],
  "properties": {
    "overall_assessment": {"type": "string", "description": "3-5 sentences: overall level of interviewer effects and where the main problems are"},
    "survey_wide_issues": {"type": "array", "items": {"type": "object", "additionalProperties": false,
      "required": ["issue", "evidence", "action", "owner"],
      "properties": {"issue": {"type": "string"}, "evidence": {"type": "string"}, "action": {"type": "string"}, "owner": {"type": "string"}}}},
    "most_problematic_modules": {"type": "array", "items": {"type": "object", "additionalProperties": false,
      "required": ["dataset", "module", "why", "action"],
      "properties": {"dataset": {"type": "string"}, "module": {"type": "string"}, "why": {"type": "string"}, "action": {"type": "string"}}}},
    "teams": {"type": "array", "items": {"type": "object", "additionalProperties": false,
      "required": ["team", "priority", "headline", "issues", "actions", "interviewers_to_observe", "modules_to_retrain"],
      "properties": {
        "team": {"type": "string", "description": "team label exactly as in the evidence"},
        "priority": {"type": "string", "enum": ["High", "Medium", "Low"]},
        "headline": {"type": "string", "description": "one sentence: the main problem of the team"},
        "issues": {"type": "array", "items": {"type": "object", "additionalProperties": false, "required": ["issue", "evidence"],
          "properties": {"issue": {"type": "string"}, "evidence": {"type": "string", "description": "numbers copied from the evidence"}}}},
        "actions": {"type": "array", "items": {"type": "object", "additionalProperties": false, "required": ["action", "owner", "timing"],
          "properties": {"action": {"type": "string"}, "owner": {"type": "string"},
                         "timing": {"type": "string", "enum": ["Immediately", "This week", "Next refresher"]}}}},
        "interviewers_to_observe": {"type": "array", "items": {"type": "object", "additionalProperties": false, "required": ["interviewer", "reason"],
          "properties": {"interviewer": {"type": "string", "description": "interviewer code exactly as in the evidence"}, "reason": {"type": "string"}}}},
        "modules_to_retrain": {"type": "array", "items": {"type": "string"}}}}},
    "caveats": {"type": "array", "items": {"type": "string"}}
  }}', simplifyVector = FALSE)

ai_system_prompt <- function(language = "English") paste(
  "You are a senior MICS (Multiple Indicator Cluster Surveys) fieldwork data-quality expert advising the survey coordinator during fieldwork.",
  "You receive the output of the Interviewer Performance Monitoring (IPM): aggregated statistics per team, interviewer, module and question.",
  "Rules:",
  "1. Use only the evidence provided. Copy numbers exactly as they appear; never compute or invent new figures, teams, interviewers or questions.",
  "2. Identify teams by their label and interviewers by their code exactly as given.",
  "3. Distinguish: (a) question/translation/training problems (flagged in many teams) -> survey-wide action; (b) team problems -> supervisor and refresher actions; (c) individual interviewers -> observation, spot-checks, one-to-one feedback.",
  "4. Prioritise: for each team give the 2-5 most useful, concrete actions with an owner (Supervisor, Survey coordinator, Field trainer, Measurer trainer, Data processing/CAPI team) and a timing. Prefer the standard actions provided, adapted to the evidence. Group similar actions.",
  "5. Flags are statistical signals, not proof of misconduct: phrase findings as things to verify, never as accusations.",
  "6. Be concise and practical: the plan is used to brief supervisors this week.",
  sprintf("7. Write all text in %s.", language), sep = "\n")

# ---------------------------------------------------------------- prompts (one place, used for the calls and the preview)

ai_plan_prompt <- function(res, sc, language = "English", max_teams = 30) {
  evj <- ai_json(ipm_ai_evidence(res, sc, max_teams = max_teams))
  user <- paste0("Evidence (JSON):\n", evj, "\n\nWrite the action plan. Include every team whose rule-based priority is High or Medium, and any other team you consider needs action; ",
                 "list survey-wide question/module problems separately. Use at most 6 issues and 5 actions per team.")
  list(system = ai_system_prompt(language), user = user, schema = ipm_plan_schema(), max_tokens = 16000, evidence_json = evj)
}

ai_ask_prompt <- function(question, res, sc, history = list(), language = "English") {
  labs <- sc$teams$team_label
  hit <- labs[vapply(labs, function(l) grepl(paste0("(^|[^A-Za-z0-9])", l, "([^0-9]|$)"), question, ignore.case = TRUE) ||
                                      grepl(paste0("team\\s*", sub("^\\D*", "", l), "([^0-9]|$)"), question, ignore.case = TRUE), logical(1))]
  ev <- ipm_ai_evidence(res, sc, team = if (length(hit)) hit[1] else NULL)
  hist <- paste(vapply(utils::tail(history, 4), function(h) sprintf("Q: %s\nA: %s", h$q, h$a), ""), collapse = "\n\n")
  user <- paste0("Evidence (JSON):\n", ai_json(ev), if (nzchar(hist)) paste0("\n\nEarlier questions in this session:\n", hist) else "",
                 "\n\nQuestion: ", question,
                 "\n\nAnswer in a few short paragraphs or bullet points (markdown). Quote the numbers from the evidence. If the evidence does not contain the answer, say so and say which IPM output or flag table to look at.")
  list(system = ai_system_prompt(language), user = user, schema = NULL, max_tokens = 3000, team = hit)
}

#' Exactly what will be sent to the AI model
#'
#' Builds, with the same code as the real calls, the full request for an action plan or for a question:
#' the endpoint, the headers (API key hidden) and the JSON body with the instructions (system prompt),
#' the evidence, the request and, for the plan, the required answer format.
#' @param res,sc,st as in ipm_ai_plan()
#' @param what "plan" or "question"
#' @param question the question (for what = "question")
#' @param history earlier questions in the session
#' @param language language of the answer
#' @return list(url, headers, body, json, chars, tokens_estimate, parts)
#' @export
ipm_ai_preview <- function(res, sc = ipm_scorecard(res), st = ipm_ai_settings(), what = c("plan", "question"),
                           question = "Which team should the coordinator visit first, and why?", history = list(), language = "English") {
  what <- match.arg(what)
  pr <- if (what == "plan") ai_plan_prompt(res, sc, language) else ai_ask_prompt(question, res, sc, history, language)
  if (!st$provider %in% c("anthropic", "ollama", "openai")) st <- ipm_ai_settings("anthropic", api_key = "x")   # show the Claude format when nothing is set up
  rq <- ai_request(st, pr$system, pr$user, pr$schema, pr$max_tokens)
  j <- ai_json(rq$body)
  list(url = rq$url, headers = rq$headers, body = rq$body, provider = st$provider, model = st$model,
       json = jsonlite::prettify(j, indent = 1), chars = nchar(j), tokens_estimate = round(nchar(j) / 3.5),
       parts = c(instructions = nchar(pr$system), evidence_and_request = nchar(pr$user),
                 answer_format = if (is.null(pr$schema)) 0L else nchar(ai_json(pr$schema))),
       max_output_tokens = pr$max_tokens)
}

# ---------------------------------------------------------------- provider calls

ai_extract_json <- function(txt) {
  txt <- gsub("^\\s*```(json)?|```\\s*$", "", txt)
  a <- regexpr("\\{", txt); b <- max(gregexpr("\\}", txt)[[1]])
  if (a < 0 || b < a) stop("The AI response contained no JSON object.", call. = FALSE)
  jsonlite::fromJSON(substr(txt, a, b), simplifyVector = FALSE)
}

# The exact HTTP request for a provider (shared by ipm_ai_call() and ipm_ai_preview(), so the preview is what is sent)
ai_request <- function(st, system, user, schema = NULL, max_tokens = 8000) {
  if (st$provider == "anthropic") {
    body <- list(model = st$model, max_tokens = max_tokens, system = system, messages = list(list(role = "user", content = user)))
    if (!is.null(schema)) body$output_config <- list(format = list(type = "json_schema", schema = schema))
    return(list(url = paste0(st$base_url, "/v1/messages"), headers = c(`x-api-key` = "<API key, not shown>", `anthropic-version` = "2023-06-01"), body = body))
  }
  if (st$provider == "ollama") {
    # native API: the context window is sized to the evidence (Ollama's default would silently truncate it)
    nctx <- as.integer(min(131072, max(8192, 1024 * ceiling(((nchar(system) + nchar(user)) / 3 + max_tokens) / 1024))))
    body <- list(model = st$model, stream = FALSE, messages = list(list(role = "system", content = system), list(role = "user", content = user)),
                 options = list(num_ctx = nctx, temperature = 0.2, num_predict = max_tokens))
    if (!is.null(schema)) body$format <- schema
    # reasoning models (gemma4, qwen3, deepseek-r1...) would spend the output budget "thinking": thinking is switched off
    body$think <- !identical(tolower(ai_env("IPM_AI_THINK", "false")), "false")
    return(list(url = paste0(st$base_url, "/api/chat"), headers = character(), body = body))
  }
  body <- list(model = st$model, max_tokens = max_tokens, temperature = 0.2,
               messages = list(list(role = "system", content = system), list(role = "user", content = user)))
  if (!is.null(schema)) body$response_format <- list(type = "json_schema", json_schema = list(name = "ipm_output", strict = TRUE, schema = schema))
  list(url = paste0(st$base_url, "/chat/completions"), headers = if (nzchar(st$api_key)) c(Authorization = "Bearer <API key, not shown>") else character(), body = body)
}

#' Low-level call to the configured model
#' @param st settings from ipm_ai_settings()
#' @param system system prompt
#' @param user user message
#' @param schema optional JSON schema (list) for structured output
#' @param max_tokens maximum output tokens
#' @return list(text, usage, model)
#' @export
ipm_ai_call <- function(st, system, user, schema = NULL, max_tokens = 8000) {
  if (!isTRUE(st$ready)) stop(if (st$provider %in% c("ollama", "openai") && !nzchar(st$model)) "Choose a model for this connection." else "No AI provider is configured.", call. = FALSE)
  if (st$provider == "mock") return(list(text = attr(st, "mock_text") %||% '{"answer":"mock"}', usage = list(input = nchar(user) %/% 4, output = 0), model = "mock"))
  if (!requireNamespace("httr2", quietly = TRUE)) stop("Package httr2 is required for the AI features.", call. = FALSE)
  send <- function(req) {
    resp <- tryCatch(httr2::req_perform(req), error = function(e) {
      r <- tryCatch(e$resp, error = function(...) NULL)
      msg <- if (!is.null(r)) tryCatch({ b <- httr2::resp_body_json(r); if (is.character(b$error)) b$error else b$error$message %||% b$message %||% httr2::resp_status_desc(r) }, error = function(...) conditionMessage(e)) else conditionMessage(e)
      structure(class = c("ipm_ai_http_error", "error", "condition"), list(message = paste0("AI request failed: ", msg), status = if (!is.null(r)) httr2::resp_status(r) else NA))
    })
    if (inherits(resp, "error")) stop(resp)
    httr2::resp_body_json(resp, simplifyVector = FALSE)
  }
  base_req <- function(url) httr2::request(url) |> httr2::req_timeout(st$timeout) |>
    httr2::req_retry(max_tries = 3, is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 529), backoff = function(i) 5 * i)
  rq <- ai_request(st, system, user, schema, max_tokens); body <- rq$body
  if (st$provider == "anthropic") {
    req <- base_req(rq$url) |>
      httr2::req_headers(`x-api-key` = st$api_key, `anthropic-version` = "2023-06-01") |>
      httr2::req_body_raw(ai_json(body), type = "application/json")
    out <- send(req)
    if (identical(out$stop_reason, "max_tokens")) stop("The AI response was cut off (output limit reached). Reduce the number of teams or raise the limit.", call. = FALSE)
    if (identical(out$stop_reason, "refusal")) stop("The model declined to answer this request.", call. = FALSE)
    txt <- paste(vapply(Filter(function(b) identical(b$type, "text"), out$content), `[[`, "", "text"), collapse = "")
    return(list(text = txt, usage = list(input = out$usage$input_tokens, output = out$usage$output_tokens), model = out$model %||% st$model))
  }
  if (st$provider == "ollama") {
    req_of <- function(b) base_req(rq$url) |> httr2::req_body_raw(ai_json(b), type = "application/json")
    # retry without the thinking flag for models/servers that do not accept it
    out <- tryCatch(send(req_of(body)), ipm_ai_http_error = function(e) {
      if (isTRUE(e$status == 400) && grepl("think", conditionMessage(e), ignore.case = TRUE)) { body$think <- NULL; send(req_of(body)) } else stop(e) })
    if (identical(out$done_reason, "length"))
      stop(if (!nzchar(out$message$content %||% "")) "The model used its whole output budget without answering (reasoning model?). Set IPM_AI_THINK=false or choose another model."
           else "The AI response was cut off (output limit reached).", call. = FALSE)
    return(list(text = out$message$content %||% "", usage = list(input = out$prompt_eval_count %||% NA, output = out$eval_count %||% NA),
                model = out$model %||% st$model, num_ctx = body$options$num_ctx))
  }
  # OpenAI-compatible (vLLM, LM Studio, llama.cpp server, OpenAI, Azure OpenAI through a /v1-compatible gateway)
  mk <- function(b) { r <- base_req(rq$url) |> httr2::req_body_raw(ai_json(b), type = "application/json")
    if (nzchar(st$api_key)) r <- httr2::req_auth_bearer_token(r, st$api_key); r }
  out <- tryCatch(send(mk(body)), ipm_ai_http_error = function(e) {
    if (!is.null(schema) && isTRUE(e$status == 400)) { body$response_format <- list(type = "json_object"); send(mk(body)) } else stop(e) })
  ch <- out$choices[[1]]
  if (identical(ch$finish_reason, "length")) stop("The AI response was cut off (output limit reached).", call. = FALSE)
  list(text = ch$message$content %||% "", usage = list(input = out$usage$prompt_tokens %||% NA, output = out$usage$completion_tokens %||% NA),
       model = out$model %||% st$model)
}

# ---------------------------------------------------------------- plan + validation

num_tokens <- function(x) { m <- regmatches(x, gregexpr("-?[0-9]+(\\.[0-9]+)?", x))[[1]]; suppressWarnings(as.numeric(m)) }

#' Check an AI plan against the scorecard
#'
#' Removes teams that do not exist, flags interviewer codes that are not in the team, and lists numbers
#' quoted in the plan that cannot be found in the evidence pack (possible invented figures).
#' @param plan parsed plan
#' @param sc scorecard
#' @param evidence_json the JSON text sent to the model
#' @export
ipm_validate_plan <- function(plan, sc, evidence_json) {
  warn <- character()
  known <- unique(c(num_tokens(evidence_json), 0:20))
  near <- function(v) any(abs(known - v) < 0.051) || any(abs(round(known) - v) < 0.51 & v == round(v))
  ivteam <- if (!is.null(sc$interviewers)) sc$interviewers[, .(team_label, code = fmt_int(interv))] else data.table(team_label = character(), code = character())
  keep <- list()
  for (t in plan$teams %||% list()) {
    if (!t$team %in% sc$teams$team_label) { warn <- c(warn, sprintf("Team '%s' is not in the data: removed.", t$team)); next }
    t$unverified <- character()
    for (k in seq_along(t$interviewers_to_observe)) {
      cc <- sub("^\\D*", "", t$interviewers_to_observe[[k]]$interviewer); tt <- t$team; code <- cc
      if (!nrow(ivteam[ivteam$team_label == tt & ivteam$code == cc]) && !code %in% fmt_int(sc$signals[team_label == t$team]$interv)) {
        t$interviewers_to_observe[[k]]$unverified <- TRUE
        warn <- c(warn, sprintf("Team %s: interviewer '%s' is not a tested interviewer of this team.", t$team, code))
      }
    }
    txt <- paste(c(t$headline, vapply(t$issues, function(z) z$evidence %||% "", ""), vapply(t$interviewers_to_observe, function(z) z$reason %||% "", "")), collapse = " ")
    bad <- unique(Filter(function(v) !is.na(v) && !near(v), num_tokens(txt)))
    if (length(bad)) { t$unverified <- format(bad); warn <- c(warn, sprintf("Team %s: number(s) %s not found in the scorecard - check before use.", t$team, paste(format(bad), collapse = ", "))) }
    keep[[length(keep) + 1]] <- t
  }
  plan$teams <- keep
  plan$validation <- warn
  plan
}

#' AI action plan per team
#'
#' Sends the evidence pack to the configured model and returns a validated team-by-team action plan
#' with the same structure as ipm_plan_rules(). Falls back to the rule-based plan when no provider is set.
#' @param res result of run_ipm()
#' @param sc scorecard (computed if missing)
#' @param st settings from ipm_ai_settings()
#' @param language language of the plan (e.g. "English", "French")
#' @param max_teams maximum number of teams detailed
#' @export
ipm_ai_plan <- function(res, sc = ipm_scorecard(res), st = ipm_ai_settings(), language = "English", max_teams = 30) {
  if (!isTRUE(st$ready)) { p <- ipm_plan_rules(sc); p$note <- "No AI provider configured: rule-based plan."; return(p) }
  pr <- ai_plan_prompt(res, sc, language, max_teams); evj <- pr$evidence_json
  if (st$provider == "mock") attr(st, "mock_text") <- ai_json(ipm_plan_rules(sc, max_teams))
  t0 <- Sys.time()
  out <- ipm_ai_call(st, pr$system, pr$user, schema = pr$schema, max_tokens = pr$max_tokens)
  plan <- ai_extract_json(out$text)
  for (f in c("survey_wide_issues", "most_problematic_modules", "teams", "caveats")) plan[[f]] <- plan[[f]] %||% list()
  plan <- ipm_validate_plan(plan, sc, evj)
  plan$source <- if (st$provider == "mock") "mock" else "ai"
  plan$model <- out$model; plan$usage <- out$usage; plan$seconds <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")))
  plan$generated <- format(Sys.time(), "%Y-%m-%d %H:%M"); plan$language <- language
  plan
}

#' Ask a question about the IPM results
#' @param question the user's question
#' @param res,sc,st as in ipm_ai_plan()
#' @param history list of previous list(q, a) exchanges in this session (last 4 are sent)
#' @param language answer language
#' @return list(answer (markdown), usage, model, team)
#' @export
ipm_ai_ask <- function(question, res, sc = ipm_scorecard(res), st = ipm_ai_settings(), history = list(), language = "English") {
  if (!isTRUE(st$ready)) stop("No AI provider is configured.", call. = FALSE)
  pr <- ai_ask_prompt(question, res, sc, history, language); hit <- pr$team
  if (st$provider == "mock") attr(st, "mock_text") <- sprintf("Mock answer about %s.", if (length(hit)) hit[1] else "the survey")
  out <- ipm_ai_call(st, pr$system, pr$user, max_tokens = pr$max_tokens)
  list(answer = out$text, usage = out$usage, model = out$model, team = if (length(hit)) hit[1] else NA_character_)
}

# ---------------------------------------------------------------- Word export

#' Write a team action plan to Word
#' @param plan plan from ipm_ai_plan() or ipm_plan_rules()
#' @param file output .docx
#' @param sc scorecard (adds the team scorecard table)
#' @param survey,period titles
#' @export
ipm_plan_docx <- function(plan, file, sc = NULL, survey = "", period = "") {
  doc <- new_doc()
  doc <- officer::body_add_fpar(doc, officer::fpar(officer::ftext("TEAM ACTION PLAN", officer::fp_text(font.size = 18, bold = TRUE, color = unicef_cyan, font.family = "Arial"))))
  doc <- add_text(doc, sprintf("%s - period %s - generated %s by %s%s.", survey, period, plan$generated %||% "",
                               if (identical(plan$source, "rules")) "the IPM rule library" else paste("AI model", plan$model %||% ""),
                               if (!identical(plan$source, "rules")) " from the aggregated IPM scorecard (no record-level data were sent)" else ""), italic = TRUE)
  doc <- add_heading(doc, "Overall assessment"); doc <- add_text(doc, plan$overall_assessment %||% "")
  if (length(plan$survey_wide_issues)) { doc <- add_heading(doc, "Survey-wide issues (questions, translation, training)")
    for (x in plan$survey_wide_issues) doc <- add_bullet(doc, sprintf("%s - %s Action (%s): %s", x$issue, x$evidence, x$owner, x$action)) }
  if (length(plan$most_problematic_modules)) { doc <- add_heading(doc, "Most problematic modules")
    for (x in plan$most_problematic_modules) doc <- add_bullet(doc, sprintf("%s %s: %s Action: %s", x$dataset, x$module, x$why, x$action)) }
  if (!is.null(sc)) {
    doc <- add_heading(doc, "Team scorecard")
    tb <- sc$teams[, .(Team = team_label, Priority = priority, `Warning points` = points, `% flagged` = round(rate, 1), `x survey rate` = round(ratio, 1),
                       `Modules` = top_modules %||% "", `Interviewers of concern` = interviewers_of_concern %||% "")]
    tb[is.na(tb)] <- ""
    doc <- flextable::body_add_flextable(doc, ipm_flextable(as.data.frame(tb)))
  }
  for (t in plan$teams) {
    doc <- add_heading(doc, sprintf("Team %s - %s priority", t$team, t$priority), size = 12)
    doc <- add_text(doc, t$headline %||% "")
    if (length(t$issues)) { doc <- add_text(doc, "Issues", size = 9.5); for (x in t$issues) doc <- add_bullet(doc, sprintf("%s: %s", x$issue, x$evidence)) }
    if (length(t$actions)) {
      a <- data.frame(Action = vapply(t$actions, `[[`, "", "action"), Owner = vapply(t$actions, `[[`, "", "owner"), When = vapply(t$actions, `[[`, "", "timing"))
      doc <- flextable::body_add_flextable(doc, flextable::width(ipm_flextable(a), j = 1:3, width = c(7.2, 1.6, 1.2)))
    }
    if (length(t$interviewers_to_observe)) { doc <- add_text(doc, "Interviewers to observe", size = 9.5)
      for (x in t$interviewers_to_observe) doc <- add_bullet(doc, sprintf("%s: %s%s", x$interviewer, x$reason, if (isTRUE(x$unverified)) " [not verified in the data]" else "")) }
    if (length(t$modules_to_retrain)) doc <- add_text(doc, paste("Modules to retrain:", paste(unlist(t$modules_to_retrain), collapse = ", ")))
    if (length(t$unverified)) doc <- add_text(doc, sprintf("Check before use: number(s) %s not found in the scorecard.", paste(t$unverified, collapse = ", ")), italic = TRUE)
  }
  if (length(plan$caveats)) { doc <- add_heading(doc, "Caveats"); for (x in plan$caveats) doc <- add_bullet(doc, x) }
  print(doc, target = file)
  invisible(file)
}
