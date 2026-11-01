# Live traffic through lm15 for R, with receipts.
#
# It needs API keys (and, with --managed, connections saved in the lm15
# store) and costs a little money, so it is not a test and no CI runs it;
# the contract harness is the gate. From lm15-r/:
#
#   set -a; . ../.env; set +a
#   Rscript tools/live-smoke.R --out receipts/DATE-live-smoke [--managed] [--only openai,groq]
#
# Each binding is called through new_router(), the way a program uses it:
#
#   hello      complete() and stream() of one request: same text and finish
#              reason, the text deltas concatenate to the text; usage obeys
#              MAP-19 (parts within wholes, total = input + output).
#   order      a json_schema listing "reasoning" before "answer"; the body
#              sent keeps that order; parse_json() reads the answer.
#   tools      a function tool: the model calls it (complete and stream agree
#              on the call), the result goes back, the model answers with it.
#   models     list_models() lists something.
#   bad-key    a wrong key: AuthError, and the key is not in the message.
#   image      (vision bindings) a JPEG in the message.
#   reasoning  (reasoning bindings) a reasoning setting; reasoning tokens are
#              part of the output count.
#   cache      (openai, anthropic) the same long prefix twice: the second
#              call reads the cache, the input count includes the cached
#              tokens (MAP-19), and estimate_cost() prices it exactly.
#   judgments  (typesafe) declared judgments with probabilities required;
#              a bounding box on the same picture.
#
# Receipts: <binding>-<check>.json with every exchange (the request as sent,
# credential headers and query values redacted; status; response headers;
# the body or the SSE text) and the verdict. Run
# lm15-contract/tools/check_secrecy.py --root on the directory before keeping it.

suppressPackageStartupMessages(library(lm15))

args <- commandArgs(trailingOnly = TRUE)
flag <- function(name) name %in% args
value <- function(name, default = NULL) { i <- match(name, args); if (is.na(i) || i == length(args)) default else args[[i + 1L]] }
out_dir <- value("--out", file.path("receipts", paste0(Sys.Date(), "-live-smoke")))
only <- if (is.null(value("--only"))) NULL else strsplit(value("--only"), ",", fixed = TRUE)[[1]]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
photo_path <- normalizePath(value("--photo", "../website/public/docs-media/badger.jpg"), mustWork = TRUE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# ─── Bindings ────────────────────────────────────────────────────────

b <- function(name, model, env = NULL, tokens = 1000L, vision = FALSE, reasoning = NULL, cache = FALSE,
              judgment = FALSE, managed = FALSE, tools = TRUE, order = TRUE)
  list(name = name, model = model, env = env, tokens = if (is.null(tokens)) NULL else as.integer(tokens), vision = vision, reasoning = reasoning,
       cache = cache, judgment = judgment, managed = managed, tools = tools, order = order)

api_key_bindings <- list(
  b("openai", "openai:gpt-5-mini", "OPENAI_API_KEY", 4000, vision = TRUE, reasoning = list(effort = "low"), cache = TRUE),
  b("anthropic", "anthropic:claude-haiku-4-5", "ANTHROPIC_API_KEY", 1000, vision = TRUE,
    reasoning = list(effort = "low", thinking_budget = 1024L), cache = TRUE),
  b("gemini", "gemini:gemini-2.5-flash", "GEMINI_API_KEY", 4000, vision = TRUE, reasoning = list(effort = "low")),
  b("groq", "groq:openai/gpt-oss-20b", "GROQ_API_KEY", 2000, reasoning = list(effort = "low")),
  b("openrouter", "openrouter:openai/gpt-4.1-nano", "OPENROUTER_API_KEY", 1000, vision = TRUE),
  b("deepseek", "deepseek:deepseek-v4-flash", "DEEPSEEK_API_KEY", 2000),
  b("zai", "zai:glm-5.3-flash", "ZAI_API_KEY", 2000),
  b("meta", "meta:muse-spark-1.3", "META_API_KEY", 2000),
  b("moonshotai", "moonshotai:kimi-k3", "MOONSHOTAI_API_KEY", 2000),
  b("deepinfra", "deepinfra:deepseek-ai/DeepSeek-V4.1-Flash", "DEEPINFRA_API_KEY", 2000),
  b("together", "together:meta-llama/Llama-3.3-70B-Instruct-Turbo", "TOGETHER_API_KEY", 1000),
  b("fireworks", "fireworks:accounts/fireworks/models/deepseek-v4p1-flash", "FIREWORKS_API_KEY", 2000),
  b("parasail", "parasail:meta-llama/Llama-3.3-70B-Instruct", "PARASAIL_API_KEY", 1000),
  b("typesafe", "typesafe:jev-latest", "TYPESAFE_API_KEY", judgment = TRUE, tools = FALSE, order = FALSE)
)
managed_bindings <- list(
  b("xai-account", "xai:grok-4.7", tokens = 2000, managed = TRUE, reasoning = list(effort = "low")),
  b("claude-code-account", "claude-code:claude-haiku-4-5", tokens = 1000, managed = TRUE),
  # Codex takes no max_tokens (refused before sending, as in every language).
  b("openai-codex-account", "openai-codex:gpt-5.6-sol", tokens = NULL, managed = TRUE, vision = TRUE, reasoning = list(effort = "low"), cache = TRUE),
  b("openrouter-account", "openrouter:openai/gpt-4.1-nano", tokens = 1000, managed = TRUE),
  b("github-copilot-account", "github-copilot:gpt-4.1", tokens = 1000, managed = TRUE),
  b("meta-account", "meta:muse-spark-1.3", tokens = 2000, managed = TRUE)
)
bindings <- c(api_key_bindings, if (flag("--managed")) managed_bindings)
if (!is.null(only)) bindings <- Filter(function(x) x$name %in% only, bindings)

# ─── Recording transport ─────────────────────────────────────────────

keep_header <- c("content-type", "accept", "anthropic-version", "anthropic-beta", "openai-beta", "user-agent",
                 "accept-encoding", "x-goog-api-client", "editor-version", "copilot-integration-id", "openai-intent")
redact_url <- function(url) {
  parts <- strsplit(url, "?", fixed = TRUE)[[1]]
  if (length(parts) < 2L) return(url)
  query <- vapply(strsplit(parts[[2]], "&", fixed = TRUE)[[1]], function(kv) {
    kv <- strsplit(kv, "=", fixed = TRUE)[[1]]
    if (length(kv) < 2L) kv[[1]] else paste0(kv[[1]], "=<redacted>")
  }, "")
  paste0(parts[[1]], "?", paste(query, collapse = "&"))
}
as_text <- function(x) if (is.raw(x)) rawToChar(x) else if (is.null(x)) "" else paste(x, collapse = "")
body_value <- function(text) {
  if (!nzchar(text)) return(NULL)
  parsed <- tryCatch(jsonlite::fromJSON(text, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) text else parsed
}

recorder <- function() {
  inner <- transport_curl(timeout = 300)
  log <- list()
  send <- function(wire, on_chunk = NULL, on_response = NULL) {
    headers <- as.list(wire$headers)
    for (h in names(headers)) if (!(tolower(h) %in% keep_header)) headers[[h]] <- "<redacted>"
    sent_text <- as_text(wire$body)
    exchange <- list(sent = list(method = wire$method, url = redact_url(wire$url), headers = headers, body = body_value(sent_text)),
                     sent_text = sent_text)
    streamed <- list(); status <- NA_integer_; response_headers <- list()
    inner_chunk <- if (is.null(on_chunk)) NULL else function(chunk) { streamed[[length(streamed) + 1L]] <<- chunk; on_chunk(chunk) }
    inner_response <- function(s, h) { status <<- as.integer(s); response_headers <<- h; if (is.function(on_response)) on_response(s, h) }
    result <- tryCatch(inner(wire, inner_chunk, inner_response), error = function(e) {
      exchange$transport_error <<- conditionMessage(e); log[[length(log) + 1L]] <<- exchange; stop(e)
    })
    keep <- c("content-type", "request-id", "x-request-id", "openai-processing-ms", "anthropic-organization-id")
    rh <- result$headers
    exchange$status <- as.integer(result$status)
    exchange$response_headers <- rh[intersect(names(rh), keep)]
    body <- if (length(streamed)) do.call(c, streamed) else result$body
    exchange$body <- if (length(streamed)) as_text(body) else body_value(as_text(body))
    log[[length(log) + 1L]] <<- exchange
    result
  }
  structure(send, class = c("lm15_transport", "function"),
            take = function() { x <- log; log <<- list(); x })
}
take_log <- function(t) attr(t, "take")()

# ─── Helpers ─────────────────────────────────────────────────────────

usage_problems <- function(u, where) {
  if (is.null(u)) return(paste(where, ": no usage"))
  g <- function(k) u[[k]]
  p <- character()
  inp <- g("input_tokens"); out <- g("output_tokens"); tot <- g("total_tokens")
  for (k in c("cache_read_tokens", "cache_write_tokens", "input_audio_tokens"))
    if (!is.null(g(k)) && !is.null(inp) && g(k) > inp) p <- c(p, sprintf("%s: %s %d > input_tokens %d", where, k, g(k), inp))
  for (k in c("reasoning_tokens", "output_audio_tokens"))
    if (!is.null(g(k)) && !is.null(out) && g(k) > out) p <- c(p, sprintf("%s: %s %d > output_tokens %d", where, k, g(k), out))
  if (!is.null(inp) && !is.null(out) && !is.null(tot) && tot != inp + out)
    p <- c(p, sprintf("%s: total_tokens %d != input %d + output %d", where, tot, inp, out))
  if (is.null(inp) || is.null(out)) p <- c(p, sprintf("%s: input or output count unknown (input %s, output %s)", where, format(inp), format(out)))
  p
}
usage_note <- function(u) {
  if (is.null(u)) return("usage: none")
  keys <- c("input_tokens", "output_tokens", "total_tokens", "cache_read_tokens", "cache_write_tokens", "reasoning_tokens")
  vals <- vapply(keys, function(k) if (is.null(u[[k]])) "-" else as.character(u[[k]]), "")
  paste0("usage: ", paste(sprintf("%s=%s", sub("_tokens", "", keys), vals), collapse = " "))
}
err_text <- function(e) paste0(paste(class(e)[1:min(2, length(class(e)))], collapse = "/"), ": ", conditionMessage(e))
text_stream <- function(router, req) {
  pieces <- character()
  final <- stream(router, req, function(event) {
    if (identical(event$type, "delta") && identical(event$delta$type, "text")) pieces <<- c(pieces, event$delta$text)
  })
  list(response = final, text = paste(pieces, collapse = ""))
}
mk_request <- function(bind, messages, ..., config_args = list()) {
  cfg <- do.call(config, utils::modifyList(if (is.null(bind$tokens)) list() else list(max_tokens = bind$tokens), config_args))
  request(bind$model, messages, ..., config = cfg)
}

# ─── Checks ──────────────────────────────────────────────────────────

check_hello <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  req <- mk_request(bind, list(message_user("Reply with exactly the two words: hello world")))
  done <- complete(router, req)
  s <- text_stream(router, req)
  ct <- response_text(done) %||% ""; st <- response_text(s$response) %||% ""
  if (!grepl("hello", tolower(ct))) r$problems <- c(r$problems, paste0("complete text: ", ct))
  if (!grepl("hello", tolower(st))) r$problems <- c(r$problems, paste0("stream text: ", st))
  if (!identical(s$text, st)) r$problems <- c(r$problems, "the text deltas do not concatenate to the streamed response's text")
  if (!identical(done$finish_reason, s$response$finish_reason))
    r$problems <- c(r$problems, sprintf("finish reasons differ: complete %s, stream %s", done$finish_reason, s$response$finish_reason))
  r$problems <- c(r$problems, usage_problems(done$usage, "complete"), usage_problems(s$response$usage, "stream"))
  r$notes <- c(r$notes, sprintf("complete: %s (%s; %s)", ct, done$finish_reason, usage_note(done$usage)),
               sprintf("stream: %s (%s; %s)", st, s$response$finish_reason, usage_note(s$response$usage)))
  r
}

check_order <- function(router, bind, transport) {
  r <- list(problems = character(), notes = character())
  schema <- json_object(type = "object",
    properties = json_object(reasoning = json_object(type = "string"), answer = json_object(type = "string")),
    required = json_array("reasoning", "answer"), additionalProperties = FALSE)
  req <- mk_request(bind, list(message_user("What is 17 times 3? Think it through briefly, then give the answer.")),
    config_args = list(response_format = json_object(type = "json_schema", name = "worked_answer", schema = schema, strict = TRUE)))
  done <- complete(router, req)
  log <- take_log(transport); body <- log[[length(log)]]$sent_text
  dropped <- Filter(function(a) identical(a$field, "config.response_format") && identical(a$action, "dropped"), done$adaptations)
  if (length(dropped)) {
    # The provider cannot apply a schema: lm15 drops it and records why (MAP-14/16); nothing to check in the body.
    r$notes <- c(r$notes, paste("adapted, as the contract says:", dropped[[1]]$reason), paste0("answer: ", response_text(done) %||% ""))
    r$log <- log
    return(r)
  }
  pos <- function(k) regexpr(paste0('"', k, '"'), body, fixed = TRUE)[[1]]
  if (pos("reasoning") < 0 || pos("answer") < 0 || pos("reasoning") > pos("answer"))
    r$problems <- c(r$problems, "the body sent does not list reasoning before answer")
  data <- tryCatch(parse_json(done), error = function(e) { r$problems <<- c(r$problems, paste("parse_json:", conditionMessage(e))); NULL })
  if (!is.null(data)) {
    if (!grepl("51", as.character(data$answer %||% ""))) r$problems <- c(r$problems, paste0("answer: ", format(data$answer)))
    r$notes <- c(r$notes, sprintf("the model's keys, in the order written: %s; answer %s", paste(names(data), collapse = ", "), format(data$answer)))
  }
  r$problems <- c(r$problems, usage_problems(done$usage, "complete"))
  for (a in done$adaptations) r$notes <- c(r$notes, sprintf("adapted: %s %s (%s)", a$field, a$action, a$reason %||% ""))
  r$log <- log
  r
}

weather_tool <- function_tool("get_weather", description = "Get the weather forecast for a place on a date.",
  parameters = json_object(type = "object",
    properties = json_object(location = json_object(type = "string", description = "City name"),
                             date = json_object(type = "string", description = "Date as YYYY-MM-DD")),
    required = json_array("location", "date"), additionalProperties = FALSE))

check_tools <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  msgs <- list(message_user("What will the weather be in Paris on 2026-11-02? Use the tool, then answer in one sentence."))
  req <- mk_request(bind, msgs, tools = list(weather_tool))
  first <- complete(router, req)
  calls <- tool_calls(first)
  if (!identical(first$finish_reason, "tool_call") || !length(calls)) {
    r$problems <- c(r$problems, sprintf("no tool call (finish %s, text %s)", first$finish_reason, response_text(first) %||% ""))
    return(r)
  }
  call <- calls[[1]]
  if (!identical(call$name, "get_weather") || is.null(call$input$location))
    r$problems <- c(r$problems, paste("tool call:", call$name, jsonlite::toJSON(call$input, auto_unbox = TRUE)))
  r$notes <- c(r$notes, paste("complete called", call$name, jsonlite::toJSON(call$input, auto_unbox = TRUE)))
  streamed <- stream(router, req, function(event) NULL)
  scalls <- tool_calls(streamed)
  if (!length(scalls) || !identical(scalls[[1]]$name, "get_weather") || is.null(scalls[[1]]$input$location))
    r$problems <- c(r$problems, sprintf("stream: no complete tool call (finish %s)", streamed$finish_reason))
  else r$notes <- c(r$notes, paste("stream called", scalls[[1]]$name, jsonlite::toJSON(scalls[[1]]$input, auto_unbox = TRUE)))
  results <- lapply(calls, function(c) tool_result_part(c$id, list(text_part('{"forecast": "light rain", "high_celsius": 12}'))))
  msgs <- c(msgs, list(first$message, new_message("tool", results)))
  second <- complete(router, mk_request(bind, msgs, tools = list(weather_tool)))
  t <- response_text(second) %||% ""
  if (!grepl("rain|12", tolower(t))) r$problems <- c(r$problems, paste0("final answer does not use the result: ", t))
  r$notes <- c(r$notes, paste0("answer: ", t))
  r$problems <- c(r$problems, usage_problems(first$usage, "first"), usage_problems(second$usage, "second"))
  r
}

check_models <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  found <- list_models(router_lm(router, bind$model))
  ids <- vapply(found, function(m) m$id %||% "", "")
  if (!length(ids)) r$problems <- c(r$problems, "no models listed")
  wanted <- sub("^[^:]+:", "", bind$model)
  r$notes <- c(r$notes, sprintf("%d models; %s %s", length(ids), wanted, if (wanted %in% ids) "listed" else "not listed"))
  r
}

check_bad_key <- function(bind) {
  r <- list(problems = character(), notes = character())
  provider <- sub(":.*$", "", bind$model)
  fake <- "lm15-live-smoke-invalid-key-0123456789"
  router <- new_router(api_keys = setNames(list(fake), provider), env = character())
  req <- if (bind$judgment) request(bind$model, list(message_user("The wine is oaky.")),
    config = config(response_format = judgments(oaky = yes_no("Is the wine oaky?"))))
  else mk_request(bind, list(message_user("hi")))
  e <- tryCatch({ complete(router, req); NULL }, error = function(e) e)
  if (is.null(e)) return(list(problems = "a wrong key was accepted", notes = character()))
  if (!inherits(e, "AuthError")) r$problems <- c(r$problems, paste("not AuthError:", err_text(e)))
  if (grepl(fake, conditionMessage(e), fixed = TRUE)) r$problems <- c(r$problems, "the key is repeated in the message")
  r$notes <- c(r$notes, err_text(e))
  r
}

check_image <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  req <- mk_request(bind, list(message_user(list("What animal is in this camera-trap photo? Answer in a few words.",
                                                 image_part(path = photo_path, media_type = "image/jpeg")))))
  done <- complete(router, req)
  t <- response_text(done) %||% ""
  if (!nzchar(t)) r$problems <- c(r$problems, "no answer")
  if (!grepl("badger", tolower(t))) r$notes <- c(r$notes, "the model did not say badger (a model answer, not an lm15 check)")
  r$problems <- c(r$problems, usage_problems(done$usage, "complete"))
  r$notes <- c(r$notes, paste0("answer: ", t), usage_note(done$usage))
  r
}

check_reasoning <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  req <- mk_request(bind, list(message_user("Is 221 prime? Answer yes or no, then the reason in one sentence.")),
                    config_args = c(list(reasoning = do.call(reasoning, bind$reasoning)), if (!is.null(bind$tokens)) list(max_tokens = max(bind$tokens, 4000L))))
  done <- complete(router, req)
  t <- response_text(done) %||% ""
  if (!grepl("no", tolower(t))) r$problems <- c(r$problems, paste0("answer: ", t))
  u <- done$usage
  r$problems <- c(r$problems, usage_problems(u, "complete"))
  thinking <- Filter(function(p) identical(p$type, "thinking"), done$message$parts)
  r$notes <- c(r$notes, paste0("answer: ", t), usage_note(u), sprintf("%d thinking part(s)", length(thinking)))
  for (a in done$adaptations) r$notes <- c(r$notes, sprintf("adapted: %s %s (%s)", a$field, a$action, a$reason %||% ""))
  r
}

prices <- list(
  openai = inference_pricing(input_per_million = 0.25, output_per_million = 2, cache_read_per_million = 0.025),
  anthropic = inference_pricing(input_per_million = 1, output_per_million = 5, cache_read_per_million = 0.1, cache_write_per_million = 1.25)
)
check_cache <- function(router, bind, transport) {
  r <- list(problems = character(), notes = character())
  handbook <- paste(rep(paste("Field station rule", 1:40, ": record the date, the place, the species and the count of every animal seen;",
                             "photograph tracks beside a scale bar; never handle a live animal; log the camera battery level at each visit."),
                        6), collapse = "\n")
  provider <- sub(":.*$", "", bind$model)
  wire <- if (provider == "openai-codex") "openai" else provider  # the same Responses wire and usage block
  cfg <- if (provider == "anthropic") list(cache = cache_config()) else list()
  ask <- function(q) complete(router, mk_request(bind, list(message_user(q)), system = handbook, config_args = cfg))
  first <- ask("In one sentence: what must be logged at each camera visit?")
  Sys.sleep(3)
  second <- ask("In one sentence: what must never be done with a live animal?")
  log <- take_log(transport)
  last <- log[[length(log)]]$body
  if (is.character(last)) {  # an SSE body (Codex streams): the usage of the response.completed event
    events <- regmatches(last, gregexpr("(?m)^data: \\{.*$", last, perl = TRUE))[[1]]
    parsed <- lapply(sub("^data: ", "", events), function(x) tryCatch(jsonlite::fromJSON(x, simplifyVector = FALSE), error = function(e) NULL))
    done_events <- Filter(function(e) identical(e$type, "response.completed"), parsed)
    raw_usage <- if (length(done_events)) done_events[[1]]$response$usage else NULL
  } else raw_usage <- last$usage
  u <- second$usage
  r$notes <- c(r$notes, paste("first", usage_note(first$usage)), paste("second", usage_note(u)),
               paste("provider's own usage on the second call:", jsonlite::toJSON(raw_usage, auto_unbox = TRUE)))
  r$problems <- c(r$problems, usage_problems(first$usage, "first"), usage_problems(u, "second"))
  if (is.null(u$cache_read_tokens) || u$cache_read_tokens == 0) r$notes <- c(r$notes, "the second call read no cache (the provider decides)")
  # MAP-19: input includes the cached tokens.
  if (provider == "anthropic" && !is.null(raw_usage)) {
    expect <- (raw_usage$input_tokens %||% 0) + (raw_usage$cache_read_input_tokens %||% 0) + (raw_usage$cache_creation_input_tokens %||% 0)
    if (!identical(as.numeric(u$input_tokens), as.numeric(expect)))
      r$problems <- c(r$problems, sprintf("input_tokens %s, expected new + cache read + cache write = %s", u$input_tokens, expect))
  }
  if (wire == "openai" && !is.null(raw_usage)) {
    if (!identical(as.numeric(u$input_tokens), as.numeric(raw_usage$input_tokens)))
      r$problems <- c(r$problems, sprintf("input_tokens %s, OpenAI's input_tokens %s", u$input_tokens, raw_usage$input_tokens))
  }
  cost <- estimate_cost(prices[[wire]], u)
  if (is.null(cost)) r$problems <- c(r$problems, "estimate_cost() is NULL with every count known")
  else {
    p <- prices[[wire]]
    cr <- u$cache_read_tokens %||% 0; cw <- u$cache_write_tokens %||% 0
    expect <- ((u$input_tokens - cr - cw) * p$input_per_million + cr * (p$cache_read_per_million %||% p$input_per_million) +
               cw * (p$cache_write_per_million %||% p$input_per_million) + u$output_tokens * p$output_per_million) / 1e6
    if (abs(cost - expect) > 1e-12) r$problems <- c(r$problems, sprintf("estimate_cost() %.8f, expected %.8f", cost, expect))
    r$notes <- c(r$notes, sprintf("estimate_cost: $%.6f (by hand $%.6f)", cost, expect))
  }
  r$log <- log
  r
}

check_judgments <- function(router, bind) {
  r <- list(problems = character(), notes = character())
  review <- judgments(
    ageing = yes_no("Does the note say the wine will improve with age?"),
    style = choice("Which style dominates?", c(fruit = "Fruit-forward", oak = "Oak-driven", other = NA)))
  note <- message_user("Ripe blackberry and cassis, toasty oak; will reward a decade in the cellar.")
  done <- complete(router, request(bind$model, list(note), config = config(response_format = review, probabilities = "required")))
  probs <- response_probabilities(done)
  d <- response_data(done)
  for (k in c("ageing", "style")) {
    p <- unlist(probs[[k]])
    if (!length(p) || abs(sum(p) - 1) > 0.01) r$problems <- c(r$problems, sprintf("%s: probabilities %s", k, paste(p, collapse = ",")))
  }
  r$notes <- c(r$notes, sprintf("ageing %s, style %s; probabilities %s", format(d$ageing), format(d$style),
                                jsonlite::toJSON(probs, auto_unbox = TRUE, digits = 3)), paste("method:", format(response_method(done))))
  boxed <- tryCatch(complete(router, request("typesafe:jev-1.14.0",
      list(message_user(list("Where is the animal?", image_part(path = photo_path, media_type = "image/jpeg")))),
      config = config(response_format = judgments(animal = bounding_box("the animal"))))), error = function(e) e)
  if (inherits(boxed, "error")) r$notes <- c(r$notes, paste("bounding box:", err_text(boxed)))
  else {
    box <- response_data(boxed)$animal
    ok <- all(c("top", "bottom", "left", "right") %in% names(box)) && box$left < box$right && box$top < box$bottom
    if (!ok) r$problems <- c(r$problems, paste("bounding box:", jsonlite::toJSON(box, auto_unbox = TRUE)))
    r$notes <- c(r$notes, paste("bounding box:", jsonlite::toJSON(box, auto_unbox = TRUE)))
  }
  r
}

# ─── Run ─────────────────────────────────────────────────────────────

results <- list()
for (bind in bindings) {
  if (!bind$managed && !nzchar(Sys.getenv(bind$env))) { message(bind$name, ": no ", bind$env, ", skipped"); next }
  transport <- recorder()
  router <- if (bind$managed) new_router(transport = transport, auth = local_auth(), env = character())
            else new_router(transport = transport, api_keys = setNames(list(Sys.getenv(bind$env)), sub(":.*$", "", bind$model)), env = character())
  checks <- list()
  if (bind$judgment) checks$judgments <- function() check_judgments(router, bind)
  else {
    checks$hello <- function() check_hello(router, bind)
    if (bind$order) checks$order <- function() check_order(router, bind, transport)
    if (bind$tools) checks$tools <- function() check_tools(router, bind)
    if (bind$vision) checks$image <- function() check_image(router, bind)
    if (!is.null(bind$reasoning)) checks$reasoning <- function() check_reasoning(router, bind)
    if (bind$cache) checks$cache <- function() check_cache(router, bind, transport)
  }
  checks$models <- function() check_models(router, bind)
  if (!bind$managed) checks[["bad-key"]] <- function() check_bad_key(bind)
  for (name in names(checks)) {
    started <- Sys.time()
    r <- tryCatch(checks[[name]](), error = function(e) list(problems = err_text(e), notes = character(), error = TRUE))
    exchanges <- c(r$log, take_log(transport))
    verdict <- if (isTRUE(r$error)) "error" else if (length(r$problems)) "problem" else "ok"
    receipt <- list(port = paste("lm15-r", as.character(utils::packageVersion("lm15"))), binding = bind$name, model = bind$model,
                    check = name, managed = bind$managed, verdict = verdict, problems = as.list(r$problems), notes = as.list(r$notes),
                    seconds = round(as.numeric(difftime(Sys.time(), started, units = "secs")), 1),
                    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                    exchanges = lapply(exchanges, function(x) { x$sent_text <- NULL; x }))
    writeLines(jsonlite::toJSON(receipt, auto_unbox = TRUE, pretty = TRUE, null = "null", force = TRUE),
               file.path(out_dir, paste0(bind$name, "-", name, ".json")))
    results[[length(results) + 1L]] <- receipt[c("binding", "model", "check", "verdict", "problems", "notes", "seconds")]
    message(sprintf("%-24s %-10s %-7s %s", bind$name, name, verdict, paste(c(r$problems, r$notes)[seq_len(min(2, length(c(r$problems, r$notes))))], collapse = " | ")))
  }
}
writeLines(jsonlite::toJSON(results, auto_unbox = TRUE, pretty = TRUE, null = "null"), file.path(out_dir, "results.json"))
tab <- table(vapply(results, function(x) x$verdict, ""))
message("\n", paste(names(tab), tab, sep = ": ", collapse = ", "))
