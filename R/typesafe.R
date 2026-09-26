# TypeSafe System One (Jev), provider "typesafe" (lm15-contract
# changes/2026-09-17-judgments.md, changes/2026-09-19-jev-state.md).
#
# One POST /v1/systemone per Request: the one user part is Jev's `state`,
# the judgment properties of the json_schema are its `questions` (MAP-14
# section 2), and the answers come back as one data part with a distribution
# per judgment and method "provider_classification" (section 3). Jev
# generates no text: a request without judgments, with tools, with media, a
# system prompt or a conversation is refused before the wire, the native
# place named.

# Config knobs with no home on the systemone wire: dropped with a record.
.typesafe_dropped <- c("max_tokens", "temperature", "top_p", "top_k", "stop", "seed", "frequency_penalty",
  "presence_penalty", "reasoning", "logprobs", "store", "user_id", "service_tier", "cache")

.typesafe_refuse <- function(feature, why) stop(lm15_error(paste0("typesafe: ", why), code = "unsupported_feature", provider = "typesafe", feature = feature))

.typesafe_state <- function(req) {
  for (m in seq_along(req$messages)) for (p in seq_along(req$messages[[m]]$parts)) {
    part <- req$messages[[m]]$parts[[p]]
    if (!part$type %in% c("text", "data"))
      .typesafe_refuse(sprintf("messages[%d].parts[%d]", m - 1L, p - 1L), paste0("a ", part$type, " part has no slot on the systemone wire (MAP-10); Jev reads text or data"))
  }
  if (!is.null(req$system))
    .typesafe_refuse("system", "Jev has no system prompt; put context in the state as a named key (message_user(data_part(list(policy = ..., note = ...)))), or the framing in each question's description")
  if (length(req$messages) != 1L)
    .typesafe_refuse("messages", paste0("Jev judges one state, got ", length(req$messages), " messages; put a transcript in the state as an array or object (message_user(data_part(list(messages = ...)))), where a question can point at a turn with a backtick path"))
  message <- req$messages[[1L]]
  if (message$role != "user") .typesafe_refuse("messages[0].role", paste0("Jev's state is a user message, got role '", message$role, "'"))
  if (length(message$parts) != 1L)
    .typesafe_refuse("messages[0].parts", paste0("Jev's state is one text or data part, got ", length(message$parts), " parts; put several pieces in one data part as named keys"))
  only <- message$parts[[1L]]
  if (only$type == "text") only$text else only$value
}

.typesafe_questions <- function(req) {
  f <- req$config$response_format
  if (!.is_object(f) || !identical(f$type, "json_schema"))
    .typesafe_refuse("config.response_format", "Jev answers declared judgments only; give a json_schema response_format whose properties are enums / booleans / ordered levels (MAP-14), e.g. judgments(...)")
  found <- request_judgments(req)
  extra <- .non_judgment_properties(f$schema, found)
  if (!length(found) || length(extra))
    .typesafe_refuse("config.response_format", paste0(if (length(extra)) paste0("properties ", paste(extra, collapse = ", "), " are free-form") else "no property declares a judgment", "; Jev cannot generate values, only pick among declared keys (MAP-14 section 1)"))
  questions <- json_object()
  for (name in names(found)) {
    j <- found[[name]]; instruction <- j$instruction
    if (is.null(instruction)) {
      .adapt(paste0("config.response_format.schema.properties.", name, ".description"), "defaulted", "a judgment without a description: the property name goes as the instruction (Jev never sees property names)", applied = name)
      instruction <- name
    }
    questions[[name]] <- switch(j$kind,
      boolean = json_object(type = "noul", instructions = instruction),
      choice = {
        if (length(j$keys) > .max_choice_keys) .typesafe_refuse(paste0("config.response_format.schema.properties.", name), paste0("a Jev choice takes at most ", .max_choice_keys, " keys, got ", length(j$keys)))
        json_object(type = "choice", instructions = instruction, criteria = .json_object(setNames(lapply(j$keys, function(k) j$descriptions[[k]]), j$keys)))
      },
      ordered = {
        if (length(j$keys) > .max_ordered_levels) .typesafe_refuse(paste0("config.response_format.schema.properties.", name), paste0("a Jev score takes at most ", .max_ordered_levels, " levels, got ", length(j$keys)))
        json_object(type = "score", instructions = instruction, criteria = .json_array(lapply(j$keys, function(k) j$descriptions[[k]] %||% k)))
      })
  }
  questions
}

.typesafe_payload <- function(lm, req) {
  if (length(req$tools)) .typesafe_refuse("tools", "tools have no slot on the systemone wire")
  c <- req$config
  if (!is.null(c$tool_choice)) .typesafe_refuse("config.tool_choice", "tool_choice has no slot on the systemone wire")
  for (name in .typesafe_dropped) {
    value <- c[[name]]
    if (is.null(value) || (is.list(value) && !inherits(value, "lm15_value") && !length(value))) next
    asked <- if (inherits(value, "lm15_value")) .json_encode(as_dict(value)) else if (is.list(value)) .json_encode(value) else value
    .adapt(paste0("config.", name), "dropped", "no such control on the systemone wire (Jev returns decisions, not samples)", asked = asked)
  }
  questions <- .typesafe_questions(req)
  payload <- json_object(model = req$model)
  payload["state"] <- list(.typesafe_state(req))
  payload$questions <- questions
  for (key in names(c$extensions)) {
    value <- c$extensions[[key]]
    if (key == "n" && !is.null(.json_int(value)) && .json_int(value) > 1L) .typesafe_refuse("config.extensions.n", "n > 1 has no canonical multiple-response representation")
    payload[key] <- list(value)
  }
  payload
}

.typesafe_build <- function(lm, req, stream) {
  if (stream) .typesafe_refuse("stream", "systemone answers in one piece; there is no stream to wrap")
  built <- .collecting(lm$adaptations %||% "note", lm$definition$id, function() .typesafe_payload(lm, req))
  wire <- .emit(lm, "POST", "/v1/systemone", built$value, model = req$model)
  wire$adaptations <- built$records
  wire
}

.typesafe_parse <- function(lm, request, data, headers) {
  pairs <- .header_pairs(headers)
  request_id <- NULL
  for (p in pairs) if (identical(p[[1L]], "x-typesafe-request-id") && nzchar(p[[2L]])) { request_id <- p[[2L]]; break }
  invalid <- function(path, detail) stop(lm15_error(paste0("malformed systemone reply at ", path, ": ", detail), code = "provider", provider = "typesafe", status = 200L, request_id = request_id, rate_limit_headers = .rate_limit_snapshot(pairs)))
  probability <- function(raw, path) {
    if (is.logical(raw) || is.null(raw) || (!is.numeric(raw) && !inherits(raw, "lm15_json_number"))) invalid(path, "expected a finite number in [0, 1]")
    p <- suppressWarnings(as.numeric(unclass(raw)))
    if (length(p) != 1L || !is.finite(p) || p < 0 || p > 1) invalid(path, "expected a finite number in [0, 1]")
    p
  }
  if (!.is_object(data)) invalid("$", "expected an object")
  found <- request_judgments(request)
  answers <- data$answers
  if (!.is_object(answers)) invalid("answers", "expected an object containing every declared judgment")
  if (!setequal(names(answers), names(found)) || length(answers) != length(found)) invalid("answers", "keys must match the declared judgments exactly")
  value <- json_object(); probabilities <- json_object()
  for (name in names(found)) {
    j <- found[[name]]; answer <- answers[[name]]; path <- paste0("answers.", name)
    if (!.is_object(answer)) invalid(path, "expected an answer object")
    expected <- c(boolean = "noul", choice = "choice", ordered = "score")[[j$kind]]
    if (!identical(answer$type, expected)) invalid(paste0(path, ".type"), paste0("expected '", expected, "'"))
    if (j$kind == "boolean") {
      p <- probability(answer$noul, paste0(path, ".noul"))
      value[[name]] <- p >= 0.5
      probabilities[[name]] <- json_object(true = p, false = 1 - p)
      next
    }
    dist <- answer$probabilities
    if (!.is_object(dist) || !setequal(names(dist), j$keys) || length(dist) != length(j$keys)) invalid(paste0(path, ".probabilities"), "expected one probability for every declared key, and no other keys")
    probs <- setNames(lapply(j$keys, function(k) probability(dist[[k]], paste0(path, ".probabilities.", k))), j$keys)
    probabilities[[name]] <- .json_object(probs)
    if (j$kind == "choice") {
      pick <- .json_str(answer$choice)
      if (is.null(pick) || !pick %in% j$keys) invalid(paste0(path, ".choice"), "expected a declared choice key")
      value[[name]] <- pick
    } else value[[name]] <- as.integer(j$keys[[which.max(unlist(probs))]])
  }
  part <- tryCatch(data_part(value, probabilities = if (length(probabilities)) probabilities, method = if (length(probabilities)) "provider_classification"), error = function(e) invalid("answers", conditionMessage(e)))
  counts <- data$usage
  if (is.null(counts)) counts <- json_object()
  if (!.is_object(counts)) invalid("usage", "expected an object or null")
  counts <- tryCatch(usage(input_tokens = counts$input_tokens, output_tokens = counts$output_tokens), error = function(e) invalid("usage", conditionMessage(e)))
  model <- data$model
  if (!is.null(model) && is.null(.nonempty_string(.json_str(model)))) invalid("model", "expected a non-empty string")
  response(model %||% request$model, message_assistant(list(part)), "stop", id = request_id, usage = counts, provider_data = json_object(typesafe = json_object(answers = answers)))
}

.typesafe_error <- function(lm, status, body, headers = list()) {
  text <- if (is.character(body)) body else if (.is_object(body)) .json_encode(body) else ""
  message <- trimws(text); if (nchar(message) > 500L) message <- substr(message, 1L, 500L)
  if (!nzchar(message)) message <- paste("HTTP", status)
  payload <- tryCatch(if (is.character(body)) .json_decode(body) else body, error = function(e) NULL)
  detail <- if (.is_object(payload)) payload$detail else NULL
  code <- NULL
  if (.is_object(detail)) {
    code <- .json_str(detail$error_type)
    if (!is.null(.json_str(detail$message))) message <- detail$message
  } else if (.is_array(detail) && length(detail)) {
    first <- if (.is_object(detail[[1L]])) detail[[1L]] else json_object()
    loc <- paste(vapply(Filter(function(x) !identical(x, "body"), .wire_array(first$loc)), .scalar_text, ""), collapse = ".")
    msg <- .json_str(first$msg) %||% "validation error"
    message <- if (nzchar(loc)) paste0(loc, ": ", msg) else msg
  }
  kind <- if (status == 401L || identical(code, "authentication_error")) "auth"
    else if (status == 429L) "rate_limit"
    else if (status == 400L && grepl("unknown model", message, ignore.case = TRUE)) "unsupported_model"
    else if (status %in% c(400L, 422L)) "invalid_request"
    else if (status >= 500L) "server"
    else .http_code(status)
  pairs <- .header_pairs(headers)
  request_id <- NULL
  for (name in .request_id_headers) { for (p in pairs) if (identical(p[[1L]], name) && nzchar(p[[2L]])) { request_id <- p[[2L]]; break }; if (!is.null(request_id)) break }
  lm15_error(message, code = kind, provider = "typesafe", provider_code = code, status = status, request_id = request_id, retry_after = .retry_after_hint(NULL, pairs, Sys.time()), rate_limit_headers = .rate_limit_snapshot(pairs),
    credential_hint = if (kind == "auth") "export TYPESAFE_API_KEY=... (console.typesafe.ai/keys)")
}
