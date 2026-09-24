.parse_input <- function(value) {
  if (.is_object(value)) return(value)
  if (!is.character(value) || length(value) != 1L || !nzchar(value)) return(json_object())
  parsed <- tryCatch(.json_decode(value), error = function(e) json_object(partial_json = value))
  if (.is_object(parsed)) parsed else json_object(value = parsed)
}
.usage_wire <- function(value, dialect) {
  if (!.is_object(value) || !length(value)) return(usage())
  if (dialect %in% c("chat", "responses")) {
    inp <- .wire_object(value[[if (dialect == "chat") "prompt_tokens_details" else "input_tokens_details"]])
    out <- .wire_object(value[[if (dialect == "chat") "completion_tokens_details" else "output_tokens_details"]])
    return(usage(input_tokens = value[[if (dialect == "chat") "prompt_tokens" else "input_tokens"]], output_tokens = value[[if (dialect == "chat") "completion_tokens" else "output_tokens"]], total_tokens = value$total_tokens, cache_read_tokens = inp$cached_tokens, cache_write_tokens = inp$cache_write_tokens, reasoning_tokens = out$reasoning_tokens, input_audio_tokens = inp$audio_tokens, output_audio_tokens = out$audio_tokens))
  }
  if (dialect == "anthropic") return(usage(input_tokens = value$input_tokens, output_tokens = value$output_tokens, cache_read_tokens = value$cache_read_input_tokens, cache_write_tokens = value$cache_creation_input_tokens, reasoning_tokens = value$output_tokens_details$thinking_tokens))
  modality <- function(entries) {
    counts <- lapply(Filter(function(e) .is_object(e) && identical(e$modality, "AUDIO"), .wire_array(entries)), function(e) .wire_int(e$tokenCount %||% 0L))
    if (length(counts)) sum(unlist(counts)) else NULL
  }
  usage(input_tokens = value$promptTokenCount %||% 0L, output_tokens = value$candidatesTokenCount %||% value$responseTokenCount %||% 0L, total_tokens = value$totalTokenCount, cache_read_tokens = value$cachedContentTokenCount, reasoning_tokens = value$thoughtsTokenCount, input_audio_tokens = modality(value$promptTokensDetails), output_audio_tokens = modality(value$candidatesTokensDetails %||% value$responseTokensDetails))
}
.logprobs_wire <- function(entries, gemini = FALSE) {
  out <- list()
  if (gemini) {
    steps <- .wire_array(entries$topCandidates)
    for (i in seq_along(entries$chosenCandidates %||% list())) {
      item <- entries$chosenCandidates[[i]]
      top <- if (i <= length(steps)) steps[[i]]$candidates %||% list() else list()
      alts <- lapply(top, function(a) top_logprob(.wire_string(a$token), .number(a$logProbability %||% 0, "logprob"), token_id = a$tokenId))
      out[[length(out) + 1L]] <- token_logprob(.wire_string(item$token), .number(item$logProbability %||% 0, "logprob"), token_id = item$tokenId, top = alts)
    }
    return(out)
  }
  for (e in .wire_array(entries)) {
    if (!.is_object(e) || is.null(e$token) || is.null(e$logprob)) next
    top <- lapply(Filter(function(a) .is_object(a) && !is.null(a$token) && !is.null(a$logprob), .wire_array(e$top_logprobs)), function(a) top_logprob(.wire_string(a$token), .number(a$logprob, "logprob"), bytes = a$bytes %||% list()))
    out[[length(out) + 1L]] <- token_logprob(.wire_string(e$token), .number(e$logprob, "logprob"), bytes = e$bytes %||% list(), top = top)
  }
  out
}
.citation_wire <- function(a, source = NULL, dialect = "responses") {
  url <- a$url %||% a$uri
  title <- a$title %||% a$filename %||% a$file_id %||% a$document_title %||% a$source_title
  words <- a$text %||% a$snippet %||% a$cited_text %||% a$quote
  if (is.null(words) && !is.null(source) && !is.null(a$start_index) && !is.null(a$end_index)) {
    start <- .wire_int(a$start_index); end <- .wire_int(a$end_index)
    if (0 <= start && start < end && end <= nchar(source)) words <- substr(source, start + 1L, end)
  }
  nonempty <- function(v) if (!is.null(v) && nzchar(.wire_string(v))) .wire_string(v) else NULL
  url <- nonempty(url); title <- nonempty(title); words <- nonempty(words)
  if (is.null(url) && is.null(title) && is.null(words)) return(NULL)
  citation_part(url = url, title = title, text = words)
}
.finish_wire <- function(raw, dialect, has_tool = FALSE) {
  if (has_tool) return("tool_call")
  word <- .wire_string(raw)
  if (dialect == "chat") return(switch(word, stop = "stop", length = "length", tool_calls = "tool_call", function_call = "tool_call", content_filter = "content_filter", "stop"))
  if (dialect == "anthropic") return(switch(word, max_tokens = "length", model_context_window_exceeded = "length", tool_use = "tool_call", pause_turn = "tool_call", refusal = "content_filter", safety = "content_filter", content_filter = "content_filter", "stop"))
  if (dialect == "gemini") return(if (word == "MAX_TOKENS") "length" else if (word %in% c("SAFETY", "RECITATION", "BLOCKLIST", "PROHIBITED_CONTENT", "SPII")) "content_filter" else "stop")
  "stop"
}
.thought_state <- function(part) {
  value <- part$thoughtSignature %||% part$functionCall$thoughtSignature
  if (is.null(value)) list() else list(continuation_state("gemini", "thought_signature", data = json_object(value = .wire_string(value))))
}

parse_response <- function(lm, request, body, ..., status = 200L, headers = list()) {
  .check_dots(...)
  if (is.raw(body)) body <- rawToChar(body)
  if (status >= 300L) stop(normalize_error(lm, status, body, headers = headers))
  data <- if (is.character(body)) .json_decode(body) else body
  if (!.is_object(data)) .abort("Provider reply must be a JSON object.", "provider", lm$definition$id)
  dialect <- switch(lm$definition$dialect, "openai-chat" = "chat", "openai-responses" = "responses", lm$definition$dialect)
  provider <- lm$definition$id
  if (!is.null(data$error)) stop(normalize_error(lm, 500L, data, headers = headers))
  parts <- list(); unmapped <- list(); logs <- list()
  add <- function(p) if (!is.null(p)) parts[[length(parts) + 1L]] <<- p
  record <- function(path, kind) unmapped[[length(unmapped) + 1L]] <<- json_object(path = path, type = .wire_string(kind, "<missing>"))
  call <- function(id, name, input, index, state = list()) {
    if (is.null(name) || !nzchar(.wire_string(name))) .abort("Provider returned a tool call without a name; lm15 will not guess.", "provider", provider)
    tool_call_part(.wire_string(id, paste0("tool_call_", index)), .wire_string(name), input = .parse_input(input), continuation = state)
  }
  id <- data$id; model <- data$model %||% request$model; finish <- "stop"
  if (dialect == "chat") {
    choices <- .wire_array(data$choices)
    if (length(choices) > 1L) .unsupported(provider, "multiple response choices; select a choice explicitly")
    chosen <- if (length(choices)) .wire_object(choices[[1L]]) else json_object()
    msg <- .wire_object(chosen$message)
    reasoning <- msg$reasoning_content %||% msg$reasoning
    if (!is.null(reasoning) && nzchar(.wire_string(reasoning))) add(thinking(.wire_string(reasoning)))
    content <- msg$content
    if (is.character(content) && nzchar(content)) add(text(content))
    else if (.is_array(content)) for (i in seq_along(content)) {
      b <- content[[i]]
      if (.is_object(b) && identical(b$type, "text")) add(text(.wire_string(b$text))) else record(sprintf("choices[0].message.content[%d]", i - 1L), if (.is_object(b)) b$type else typeof(b))
    } else if (!is.null(content) && !is.character(content)) record("choices[0].message.content", typeof(content))
    if (!is.null(msg$refusal) && nzchar(.wire_string(msg$refusal))) add(refusal(.wire_string(msg$refusal)))
    for (i in seq_along(msg$tool_calls %||% list())) {
      tc <- msg$tool_calls[[i]]
      if (!.is_object(tc) || !(tc$type %||% "function") == "function") { record(sprintf("choices[0].message.tool_calls[%d]", i - 1L), if (.is_object(tc)) tc$type else typeof(tc)); next }
      fn <- .wire_object(tc[["function"]]); add(call(tc$id, fn$name, fn$arguments, length(parts)))
    }
    known <- c("stop", "length", "tool_calls", "function_call", "content_filter")
    if (!is.null(chosen$finish_reason) && !chosen$finish_reason %in% known) record("choices[0].finish_reason", chosen$finish_reason)
    finish <- .finish_wire(chosen$finish_reason, "chat")
    logs <- .logprobs_wire(chosen$logprobs$content)
  }
  if (dialect == "responses") {
    for (i in seq_along(data$output %||% list())) {
      item <- data$output[[i]]
      if (!.is_object(item)) { record(sprintf("output[%d]", i - 1L), typeof(item)); next }
      kind <- item$type %||% ""
      if (kind == "message") for (j in seq_along(item$content %||% list())) {
        b <- item$content[[j]]; k <- if (.is_object(b)) b$type %||% "" else ""
        if (k %in% c("output_text", "text")) {
          words <- .wire_string(b$text); add(text(words)); logs <- c(logs, .logprobs_wire(b$logprobs))
          for (a in .wire_array(b$annotations)) if (.is_object(a)) add(.citation_wire(a, words))
        } else if (k == "refusal") {
          words <- .wire_string(b$refusal %||% b$text); add(if (nzchar(words)) refusal(words) else text(""))
        } else if (k == "output_image") {
          value <- b$b64_json %||% b$image_base64
          if (!is.null(value)) add(image_part(data = value))
        } else if (k == "output_audio") {
          value <- b$audio$data %||% b$b64_json
          if (!is.null(value)) add(audio_part(data = value))
        } else record(sprintf("output[%d].content[%d]", i - 1L, j - 1L), k)
      } else if (kind == "function_call") add(call(item$call_id %||% item$id, item$name, item$arguments, length(parts)))
      else if (kind == "reasoning") {
        summary <- item$summary
        words <- if (.is_array(summary)) paste(vapply(summary, function(v) if (.is_object(v)) .wire_string(v$text) else .wire_string(v), ""), collapse = "\n") else .wire_string(summary %||% item$text)
        state <- json_object()
        for (key in c("id", "encrypted_content")) if (!is.null(item[[key]]) && nzchar(.wire_string(item[[key]]))) state[[key]] <- .wire_string(item[[key]])
        if (nzchar(words) || length(state)) add(thinking(words, continuation = if (length(state)) list(continuation_state("openai", "reasoning_item", data = state)) else list()))
      } else if (!kind %in% c("web_search_call", "file_search_call", "code_interpreter_call", "computer_call", "computer_use_call")) record(sprintf("output[%d]", i - 1L), kind)
    }
    why <- .wire_string(data$incomplete_details$reason)
    if (identical(data$status, "incomplete") && grepl("token", why)) finish <- "length"
    if (grepl("content_filter|safety", why)) finish <- "content_filter"
    if (!length(parts)) add(text(.wire_string(data$output_text)))
  }
  if (dialect == "anthropic") {
    for (i in seq_along(data$content %||% list())) {
      b <- data$content[[i]]
      if (!.is_object(b)) { record(sprintf("content[%d]", i - 1L), typeof(b)); next }
      kind <- b$type %||% ""
      if (kind == "text") { add(text(.wire_string(b$text))); for (a in .wire_array(b$citations)) if (.is_object(a)) add(.citation_wire(a)) }
      else if (kind == "tool_use") add(call(b$id, b$name, .wire_object(b$input), length(parts)))
      else if (kind == "thinking") add(thinking(.wire_string(b$thinking %||% b$text), continuation = if (!is.null(b$signature) && nzchar(.wire_string(b$signature))) list(continuation_state("anthropic", "thinking_signature", data = json_object(signature = b$signature))) else list()))
      else if (kind == "redacted_thinking") add(thinking("", continuation = if (!is.null(b$data)) list(continuation_state("anthropic", "redacted_thinking", data = json_object(data = b$data))) else list()))
      else if (!kind %in% c("server_tool_use", "web_search_tool_result", "code_execution_tool_result")) record(sprintf("content[%d]", i - 1L), kind)
    }
    finish <- .finish_wire(data$stop_reason, "anthropic")
  }
  if (dialect == "gemini") {
    blocked <- .gemini_inband_error(data, provider)
    if (!is.null(blocked)) stop(blocked)
    candidates <- .wire_array(data$candidates); candidate <- if (length(candidates)) .wire_object(candidates[[1L]]) else json_object()
    for (i in seq_along(candidate$content$parts %||% list())) {
      b <- candidate$content$parts[[i]]
      if (!.is_object(b)) { record(sprintf("candidates[0].content.parts[%d]", i - 1L), typeof(b)); next }
      if ("text" %in% names(b)) add(if (isTRUE(b$thought)) thinking(.wire_string(b$text), continuation = .thought_state(b)) else text(.wire_string(b$text), continuation = .thought_state(b)))
      else if (.is_object(b$functionCall)) { fn <- b$functionCall; add(call(fn$id, fn$name, .wire_object(fn$args), length(parts), .thought_state(b))) }
      else if (.is_object(b$inlineData) || .is_object(b$fileData)) {
        inline <- .is_object(b$inlineData); media <- if (inline) b$inlineData else b$fileData
        mime <- .wire_string(media$mimeType, "application/octet-stream")
        value <- if (inline) media$data else media$fileUri
        if (!is.null(value) && nzchar(.wire_string(value))) {
          fields <- list(media_type = mime); fields[[if (inline) "data" else "url"]] <- .wire_string(value)
          add(.new_value(if (startsWith(mime, "image/")) "ImagePart" else if (startsWith(mime, "audio/")) "AudioPart" else "DocumentPart", fields))
        }
      } else if (!any(c("executableCode", "codeExecutionResult") %in% names(b))) record(sprintf("candidates[0].content.parts[%d]", i - 1L), if (length(b)) paste(sort(names(b)), collapse = "+") else "<empty>")
    }
    grounding <- .wire_object(candidate$groundingMetadata); chunks <- .wire_array(grounding$groundingChunks); seen <- character()
    full_text <- paste(vapply(Filter(function(p) p$type == "text", parts), function(p) p$text, ""), collapse = "")
    for (s in .wire_array(grounding$groundingSupports)) {
      segment <- .wire_object(s$segment); words <- segment$text
      if (is.null(words) && !is.null(segment$startIndex) && !is.null(segment$endIndex)) {
        start <- .wire_int(segment$startIndex); end <- .wire_int(segment$endIndex)
        if (0 <= start && start < end && end <= nchar(full_text)) words <- substr(full_text, start + 1L, end)
      }
      for (index in .wire_array(s$groundingChunkIndices)) {
        index <- .wire_int(index) + 1L
        if (index < 1L || index > length(chunks)) next
        chunk <- chunks[[index]]; source <- .wire_object(chunk$web %||% chunk$retrievedContext %||% chunk$googleSearch)
        cited <- .citation_wire(json_object(url = source$uri %||% source$url, title = source$title %||% source$name, text = words))
        if (!is.null(cited)) { key <- as_json(cited); if (!key %in% seen) { add(cited); seen <- c(seen, key) } }
      }
    }
    id <- data$responseId; model <- request$model
    finish <- .finish_wire(candidate$finishReason, "gemini")
    logs <- .logprobs_wire(.wire_object(candidate$logprobsResult), TRUE)
  }
  if (!length(parts)) add(text(""))
  if (any(vapply(parts, function(p) p$type == "tool_call", logical(1)))) finish <- "tool_call"
  if (length(unmapped)) data$`_lm15_unmapped` <- unmapped
  response(model, message_assistant(parts), finish, id = id, usage = .usage_wire(if (dialect == "gemini") data$usageMetadata else data$usage, dialect), logprobs = logs, provider_data = data)
}

response_from_openai_chat <- function(body, ..., model = NULL, choice = NULL) {
  .check_dots(...)
  if (!is.null(choice)) {
    i <- .number(choice, "choice", TRUE) + 1L
    if (i < 1L || i > length(body$choices)) stop("choice is outside the body choices.", call. = FALSE)
    selected <- body; selected$choices <- list(body$choices[[i]])
  } else selected <- body
  lm <- new_lm("openai-chat", api_key = "parse-only", env = character())
  req <- request(model %||% body$model, list(message_user("")))
  out <- parse_response(lm, req, selected)
  # Keep the original multi-choice payload, not the selected projection.
  if (!is.null(choice)) out$provider_data <- body
  out
}
