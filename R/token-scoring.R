# Judgments by candidate-sequence likelihood (MAP-14 section 4).
#
# A Chat Completions server that scores named tokens (compat token_scoring =
# "logprob_token_ids"; vLLM >= 0.29, receipted 2026-09-17) can deliver a
# distribution over the declared keys of every judgment:
#   1. two /tokenize calls per key plus one per judgment prefill, so the
#      server's chat template is honoured and each key's token path is read
#      in context (terminator included: the paths are prefix-free);
#   2. ONE /v1/completions call carrying every trie node as a prompt (token
#      ids), max_tokens 1, logprob_token_ids = the union of child tokens; raw
#      log-probs sum along each path, then one normalisation over the keys.
# A server that answers 200 without the requested ids dropped the field:
# "required" refuses, "if_available" answers by structured output and says so.

.judgment_prefill <- "Answer:"

.judgment_ask <- function(j) {
  lines <- vapply(j$keys, function(k) {
    label <- j$titles[[k]]; desc <- j$descriptions[[k]]
    has <- function(x) !is.null(x) && nzchar(x)
    tail <- if (has(label) && has(desc)) paste0(": ", label, " - ", desc) else if (has(label) || has(desc)) paste0(": ", if (has(label)) label else desc) else ""
    paste0("- ", k, tail)
  }, "")
  paste0(j$instruction %||% j$name, "\nOptions:\n", paste(lines, collapse = "\n"), "\nAnswer with the option only, spelled exactly as listed.")
}

.with_config <- function(req, cfg) { fields <- unclass(req); fields$config <- cfg; .new_value("Request", fields) }

# The conversation as this wire's messages (no generation settings).
.judgment_base_messages <- function(lm, req) {
  .collecting("note", lm$definition$id, function() .build_payload(lm, .with_config(req, config()), FALSE))$value$messages
}
.judgment_tokenize_wire <- function(lm, model, messages, continue_final) {
  root <- lm; root$base_url <- sub("/v1$", "", lm$base_url)
  .emit(root, "POST", "/tokenize", json_object(model = model, messages = .json_array(messages), add_generation_prompt = FALSE, continue_final_message = continue_final), model = model)
}
.judgment_reply_error <- function(lm, reply, detail) {
  pairs <- .header_pairs(reply$headers)
  stop(lm15_error(paste0("malformed judgment reply: ", detail), code = "provider", provider = lm$definition$id, status = reply$status, rate_limit_headers = .rate_limit_snapshot(pairs)))
}
.judgment_tokens <- function(lm, reply) {
  data <- tryCatch(.json_decode(rawToChar(reply$body)), error = function(e) NULL)
  tokens <- if (.is_object(data)) data$tokens else NULL
  ints <- if (.is_array(tokens) && length(tokens)) lapply(tokens, .json_int) else list(NULL)
  if (any(vapply(ints, function(t) is.null(t) || is.na(t) || t < 0L, logical(1)))) .judgment_reply_error(lm, reply, "tokenize reply carries no non-negative integer token list")
  unlist(ints)
}

# Each key's token path after the prefill, terminator included.
.judgment_paths <- function(prefix, keys) {
  starts <- function(seq, head) length(seq) >= length(head) && identical(seq[seq_along(head)], head)
  lapply(setNames(names(keys), names(keys)), function(key) {
    open <- keys[[key]]$open; closed <- keys[[key]]$closed
    if (!starts(open, prefix) || !starts(closed, open))
      stop(lm15_error(paste0("openai-chat: key '", key, "' does not tokenize as an extension of the prefill in this chat template; candidate-sequence scoring cannot place it (rename the key or use a provider that classifies natively)"), code = "unsupported_feature", provider = "openai-chat", feature = "config.response_format"))
    body <- open[-seq_along(prefix)]; terminator <- closed[length(open) + 1L]
    if (!length(body) || is.na(terminator))
      stop(lm15_error(paste0("openai-chat: key '", key, "' yields no scorable tokens (empty key or no end-of-turn token in the template)"), code = "unsupported_feature", provider = "openai-chat", feature = "config.response_format"))
    c(body, terminator)
  })
}
.node_key <- function(prefix) paste0("node:", paste(prefix, collapse = ","))  # never an empty name (the root)
# Every trie node (a path prefix) and the child tokens that may follow it.
.judgment_nodes <- function(paths) {
  nodes <- list()
  for (seq in paths) for (i in seq_along(seq)) {
    prefix <- seq[seq_len(i - 1L)]; key <- .node_key(prefix)
    node <- nodes[[key]] %||% list(prefix = prefix, children = integer())
    node$children <- union(node$children, seq[[i]])
    nodes[[key]] <- node
  }
  nodes
}

# Harmful omissions are refusals, never silent scoring adaptations.
.validate_judgment_scoring <- function(lm, req) {
  refuse <- function(feature, reason) stop(lm15_error(paste0(lm$definition$id, ": ", reason, "; use generated JSON with probabilities='off' or a separate scoring request"), code = "unsupported_feature", provider = lm$definition$id, feature = feature))
  c <- req$config
  if (length(req$tools)) refuse("tools", "candidate scoring cannot execute tools; the program may depend on their results")
  if (!is.null(c$tool_choice)) refuse("config.tool_choice", "candidate scoring cannot preserve tool/action semantics")
  if (!is.null(c$cache$resource)) refuse("config.cache.resource", "candidate scoring cannot read a stored cache object; omitting it would lose prompt content")
  n <- .json_int(c$extensions$n)
  if (!is.null(n) && n > 1L) refuse("config.extensions.n", "n > 1 has no canonical multiple-response representation")
  for (name in c("store", "user_id", "service_tier")) if (!is.null(c[[name]])) refuse(paste0("config.", name), "measurement endpoints have no established mapping for this privacy, safety or billing control")
  if (!is.null(c$cache)) {
    if (c$cache$mode == "off") refuse("config.cache.mode", "measurement endpoints cannot guarantee cache writes are disabled")
    if (!is.null(c$cache$retention)) refuse("config.cache.retention", "measurement endpoints cannot preserve cache lifetime and billing intent")
  }
  harmless <- c("temperature", "top_p", "top_k", "seed", "frequency_penalty", "presence_penalty")
  for (name in names(c$extensions)) {
    value <- c$extensions[[name]]
    numeric <- !is.logical(value) && (inherits(value, "lm15_json_number") || (is.numeric(value) && length(value) == 1L)) && is.finite(suppressWarnings(as.numeric(unclass(value))))
    if ((name == "n" && numeric && as.numeric(unclass(value)) == 1) || (name %in% harmless && numeric)) next
    refuse(paste0("config.extensions.", name), "unknown measurement extension semantics; dropping it could lose privacy, money or action controls")
  }
}
.judgment_mixed <- function(req) length(.non_judgment_properties(req$config$response_format$schema, request_judgments(req))) > 0L
.judgment_generated_request <- function(req) { cfg <- unclass(req$config); cfg$probabilities <- "off"; .with_config(req, .new_value("Config", cfg)) }

# The offline preflight shared by plan() and complete(): no tokenization,
# credential or transport runs here.
.judgment_adaptations <- function(lm, req, policy = lm$adaptations %||% "note") {
  .validate_judgment_scoring(lm, req)
  .collecting(policy, lm$definition$id, function() {
    mixed <- .judgment_mixed(req)
    if (mixed) .adapt("config.response_format", "client_side", "an additional structured-output call answers ordinary properties, which are never scored")
    cfg <- as_dict(req$config)
    for (name in c("max_tokens", "temperature", "top_p", "top_k", "stop", "seed", "frequency_penalty", "presence_penalty", "reasoning", "logprobs", "cache", "extensions"))
      if (name %in% names(cfg)) .adapt(paste0("config.", name), "dropped", "candidate likelihood measures unmodified next-token probabilities with max_tokens=1 per trie node; this generation setting has no measurement slot (any generated JSON call still uses its usual mapping)", asked = cfg[[name]])
    if (mixed) .build_payload(lm, .judgment_generated_request(req), FALSE)
    .build_payload(lm, .with_config(req, config()), FALSE)
    NULL
  })$records
}

.judgment_scores <- function(lm, reply, n_prompts) {
  data <- tryCatch(.json_decode(rawToChar(reply$body)), error = function(e) NULL)
  choices <- if (.is_object(data)) data$choices else NULL
  indexes <- if (.is_array(choices)) lapply(choices, function(ch) if (.is_object(ch) && !is.logical(ch$index)) .json_int(ch$index) else NULL) else list()
  if (!.is_array(choices) || length(choices) != n_prompts || any(vapply(indexes, is.null, logical(1))) || !setequal(unlist(indexes), seq_len(n_prompts) - 1L))
    .judgment_reply_error(lm, reply, "choices must contain every prompt index exactly once")
  scores <- lapply(choices[order(unlist(indexes))], function(ch) {
    logprobs <- ch$logprobs %||% json_object()
    if (!.is_object(logprobs)) .judgment_reply_error(lm, reply, "logprobs must be an object or null")
    tops <- logprobs$top_logprobs
    top <- if (is.null(tops) || (.is_array(tops) && !length(tops))) json_object()
      else if (.is_array(tops) && length(tops) == 1L && (is.null(tops[[1L]]) || .is_object(tops[[1L]]))) tops[[1L]] %||% json_object()
      else .judgment_reply_error(lm, reply, "expected one top_logprobs object")
    out <- list()
    for (token in names(top)) {
      if (!grepl("^token_id:[0-9]+$", token)) next
      value <- top[[token]]
      number <- if (inherits(value, "lm15_json_number")) suppressWarnings(as.numeric(unclass(value))) else NA_real_
      if (is.na(number) || number > 0) .judgment_reply_error(lm, reply, paste0("invalid log probability for ", token))
      out[[substring(token, 10L)]] <- number
    }
    out
  })
  counts <- data$usage %||% json_object()
  if (!.is_object(counts)) .judgment_reply_error(lm, reply, "usage must be an object or null")
  for (name in c("prompt_tokens_details", "completion_tokens_details")) if (!is.null(counts[[name]]) && !.is_object(counts[[name]])) .judgment_reply_error(lm, reply, paste0(name, " must be an object or null"))
  counts <- tryCatch(.usage_wire(counts, "chat"), error = function(e) .judgment_reply_error(lm, reply, paste0("invalid usage: ", conditionMessage(e))))
  model <- data$model
  if (!is.null(model) && is.null(.nonempty_string(.json_str(model)))) .judgment_reply_error(lm, reply, "model must be a non-empty string")
  list(scores = scores, usage = counts, model = model)
}

.judgment_sum_usage <- function(a, b) {
  fields <- names(.schemas$Usage)
  values <- lapply(fields, function(f) if (!is.null(a[[f]]) && !is.null(b[[f]])) .integer_add(a[[f]], b[[f]]) else NULL)
  .new_value("Usage", setNames(values, fields)[!vapply(values, is.null, logical(1))])
}

# A generated answer is a strict semantic boundary: never repair an incomplete JSON answer.
.judgment_generate <- function(lm, req, unmeasured = FALSE) {
  generated <- .judgment_generated_request(req)
  plain <- .with_config(generated, { cfg <- unclass(generated$config); cfg["response_format"] <- list(NULL); .new_value("Config", cfg) })
  wire <- build_request(lm, generated)
  answer <- if (.client_side_stop(wire$adaptations)) {
    materialize <- stream(lm, generated, on_event = function(e) invisible(NULL))
    fields <- unclass(materialize); fields$message <- message_assistant(lapply(materialize$message$parts, function(p) if (identical(p$type, "data")) text(.data_part_text(p), continuation = p$continuation) else p))
    .new_value("Response", fields)
  } else {
    reply <- .send_wire(lm, wire)
    body <- tryCatch(.json_decode(rawToChar(reply$body)), error = function(e) .judgment_reply_error(lm, reply, "reply is not JSON"))
    choices <- if (.is_object(body)) body$choices else NULL
    if (.is_object(body) && .is_object(body$error)) parse_response(lm, plain, reply$body, headers = reply$headers)
    if (!.is_array(choices) || length(choices) != 1L || !.is_object(choices[[1L]]) || !identical(choices[[1L]]$finish_reason, "stop"))
      .judgment_reply_error(lm, reply, "generated judgment reply needs one complete choice with finish_reason='stop'")
    out <- parse_response(lm, plain, reply$body, headers = reply$headers)
    visible <- .visible_adaptations(lm, wire$adaptations)
    if (length(visible)) out["adaptations"] <- list(visible)
    out
  }
  bad <- function(detail) stop(lm15_error(paste0("malformed generated judgment JSON: ", detail), code = "provider", provider = lm$definition$id))
  if (answer$finish_reason != "stop") bad("generated judgment answer did not finish completely")
  texts <- which(vapply(answer$message$parts, function(p) identical(p$type, "text"), logical(1)))
  if (length(texts) != 1L) bad("generated judgment answer needs one JSON object")
  value <- tryCatch(.json_decode(answer$message$parts[[texts]]$text), error = function(e) bad("the answer is not JSON"))
  if (!.is_object(value)) bad("generated judgment answer needs a JSON object")
  measured <- names(request_judgments(generated))
  for (name in unlist(generated$config$response_format$schema$required))
    if ((unmeasured || !name %in% measured) && !name %in% names(value)) bad(paste0("generated answer is missing required property '", name, "'"))
  parts <- answer$message$parts
  parts[[texts]] <- data_part(value, continuation = parts[[texts]]$continuation)
  fields <- unclass(answer); fields$message <- message_assistant(parts); fields$adaptations <- list()
  list(response = .new_value("Response", fields), records = wire$adaptations)
}

.judgment_complete <- function(lm, request) {
  adaptations <- .judgment_adaptations(lm, request)
  found <- request_judgments(request); calls <- 0L
  base <- .judgment_base_messages(lm, request)
  send <- function(messages, continue_final) { calls <<- calls + 1L; .judgment_tokens(lm, .send_wire(lm, .judgment_tokenize_wire(lm, request$model, messages, continue_final))) }
  turn <- function(j, answer) c(base, list(json_object(role = "user", content = .judgment_ask(j)), json_object(role = "assistant", content = answer)))
  tokenized <- lapply(found, function(j) {
    prefix <- send(turn(j, .judgment_prefill), TRUE)
    keys <- lapply(setNames(j$keys, j$keys), function(k) {
      answer <- paste(.judgment_prefill, k)
      list(open = send(turn(j, answer), TRUE), closed = send(turn(j, answer), FALSE))
    })
    list(j = j, paths = .judgment_paths(prefix, keys), prefix = prefix)
  })
  prompts <- list(); meta <- list(); union_ids <- integer(); nodes_per <- list()
  for (name in names(tokenized)) {
    t <- tokenized[[name]]; nodes <- .judgment_nodes(t$paths); nodes_per[[name]] <- nodes
    for (key in names(nodes)) {
      prompts[[length(prompts) + 1L]] <- c(t$prefix, nodes[[key]]$prefix)
      meta[[length(meta) + 1L]] <- c(name, key)
      union_ids <- union(union_ids, nodes[[key]]$children)
    }
  }
  score_wire <- .emit(lm, "POST", "/completions", json_object(model = request$model, prompt = .json_array(lapply(prompts, function(p) .json_array(as.list(p)))), max_tokens = 1L, temperature = 1.0, logprobs = 0L, return_tokens_as_token_ids = TRUE, logprob_token_ids = .json_array(as.list(sort(union_ids)))), model = request$model)
  reply <- .send_wire(lm, score_wire)
  measured <- .judgment_scores(lm, reply, length(prompts))
  tables <- list()
  for (i in seq_along(meta)) {
    name <- meta[[i]][[1L]]; key <- meta[[i]][[2L]]; got <- measured$scores[[i]]
    children <- nodes_per[[name]][[key]]$children
    if (!all(as.character(children) %in% names(got))) return(.judgment_unmeasured(lm, request, adaptations, measured$usage))
    tables[[name]][[key]] <- got
  }
  value <- json_object(); probabilities <- json_object(); coverage <- json_object()
  for (name in names(tokenized)) {
    t <- tokenized[[name]]; j <- t$j
    raw <- lapply(t$paths, function(seq) sum(vapply(seq_along(seq), function(i) tables[[name]][[.node_key(seq[seq_len(i - 1L)])]][[as.character(seq[[i]])]], double(1))))
    if (!any(is.finite(unlist(raw)))) {
      e <- lm15_error(paste0("judgment '", j$name, "' has zero likelihood for every declared key; cannot normalize"), code = "provider", provider = lm$definition$id, status = reply$status, rate_limit_headers = .rate_limit_snapshot(.header_pairs(reply$headers)))
      stop(e)
    }
    coverage[[name]] <- sum(exp(unlist(raw)))
    dist <- .normalize_logprobs(raw)
    probabilities[[name]] <- .json_object(dist)
    best <- names(dist)[[which.max(unlist(dist))]]
    value[[name]] <- if (j$kind == "boolean") best == "true" else if (j$kind == "ordered") as.integer(best) else best
  }
  out <- response(.nonempty_string(.json_str(measured$model)) %||% request$model, message_assistant(list(data_part(value, probabilities = probabilities, method = "candidate_sequence_likelihood"))), "stop",
    usage = measured$usage, provider_data = json_object(coverage = coverage, judgments = json_object(nodes = length(prompts), tokenize_calls = calls, method = "candidate_sequence_likelihood")))
  if (.judgment_mixed(request)) out <- .judgment_merge(out, .judgment_generate(lm, request)$response)
  visible <- .visible_adaptations(lm, adaptations)
  if (length(visible)) out["adaptations"] <- list(visible)
  out
}

.judgment_merge <- function(measured, generated) {
  original <- .first_data_part(generated); scored <- .first_data_part(measured)
  merged <- original$value
  for (name in names(scored$value)) merged[name] <- list(scored$value[[name]])
  part <- data_part(merged, probabilities = scored$probabilities, method = scored$method, continuation = original$continuation)
  parts <- lapply(generated$message$parts, function(p) if (identical(p$type, "data")) part else p)
  pd <- measured$provider_data %||% json_object()
  pd$scoring_usage <- as_dict(measured$usage)
  pd$generated_response <- as_dict(generated, include_provider_data = TRUE)
  response(measured$model, message_assistant(parts), generated$finish_reason, id = generated$id, usage = .judgment_sum_usage(measured$usage, generated$usage), provider_data = pd)
}

.judgment_unmeasured <- function(lm, request, adaptations, scoring_usage) {
  if (identical(request$config$probabilities, "required"))
    stop(lm15_error(paste0(lm$definition$id, ": config.probabilities='required' but this server ignored logprob_token_ids (vLLM < 0.29?); no distribution can be measured here"), code = "unsupported_feature", provider = lm$definition$id, feature = "config.probabilities"))
  dropped <- .collecting(lm$adaptations %||% "note", lm$definition$id, function() .adapt("config.probabilities", "dropped", "the server accepted the request and returned no log-probs for the requested token ids (logprob_token_ids ignored); answered by structured output instead", asked = request$config$probabilities))$records
  generated <- .judgment_generate(lm, request, unmeasured = TRUE)
  answer <- generated$response
  pd <- answer$provider_data %||% json_object(); pd$scoring_usage <- as_dict(scoring_usage)
  fields <- unclass(answer); fields$usage <- .judgment_sum_usage(scoring_usage, answer$usage); fields$provider_data <- pd
  seen <- vapply(adaptations, function(a) .json_encode(as_dict(a)), "")
  extra <- Filter(function(a) !.json_encode(as_dict(a)) %in% seen, generated$records)
  fields$adaptations <- .visible_adaptations(lm, c(adaptations, dropped, extra))
  .new_value("Response", fields)
}
