.ingest_keys <- function(x, allowed, where, provider) {
  if (!.is_object(x)) stop(paste(where, "must be a JSON object."), call. = FALSE)
  extra <- setdiff(names(x), allowed)
  if (length(extra)) .unsupported(provider, paste(where, "key", extra[[1L]]))
}
.ingest_content <- function(value, role, provider) {
  if (is.character(value) && length(value) == 1L && !inherits(value, "lm15_json_number")) return(list(parts = list(text(value)), marked = FALSE))
  if (!.is_array(value)) stop("Message content must be a string or an array.", call. = FALSE)
  parts <- list(); marked <- FALSE
  for (i in seq_along(value)) {
    b <- value[[i]]
    if (!.is_object(b)) stop("Content entries must be objects.", call. = FALSE)
    k <- b$type
    if (!is.null(b$prompt_cache_breakpoint)) {
      if (!identical(b$prompt_cache_breakpoint$mode, "explicit") || !identical(names(b$prompt_cache_breakpoint), "mode") || !identical(k, "text") || i != length(value)) stop("A cache breakpoint must be explicit and on the final text block.", call. = FALSE)
      marked <- TRUE
    }
    if (identical(k, "text")) {
      .ingest_keys(b, c("type", "text", "prompt_cache_breakpoint"), "text block", provider)
      p <- text(b$text)
    } else if (identical(k, "image_url") && role %in% c("user", "tool")) {
      .ingest_keys(b, c("type", "image_url", "prompt_cache_breakpoint"), "image block", provider)
      .ingest_keys(b$image_url, c("url", "detail"), "image_url", provider)
      url <- .string(b$image_url$url, "image URL")
      if (startsWith(url, "data:")) {
        uri <- .ingest_data_uri(url)
        p <- image_part(data = uri$data, media_type = uri$media_type, detail = b$image_url$detail)
      } else {
        ext <- tolower(sub("^.*\\.", "", sub("[?#].*$", "", url)))
        mime <- switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif", webp = "image/webp", bmp = "image/bmp", svg = "image/svg+xml", "image/png")
        p <- image_part(url = url, media_type = mime, detail = b$image_url$detail)
      }
    } else if (identical(k, "input_audio") && role == "user") {
      .ingest_keys(b, c("type", "input_audio"), "audio block", provider)
      .ingest_keys(b$input_audio, c("data", "format"), "input_audio", provider)
      fmt <- b$input_audio$format
      if (is.null(fmt) || !fmt %in% c("wav", "mp3")) stop("Audio input format must be wav or mp3.", call. = FALSE)
      p <- audio_part(data = b$input_audio$data, media_type = if (fmt == "mp3") "audio/mpeg" else "audio/wav")
    } else if (identical(k, "file") && role == "user") {
      .ingest_keys(b, c("type", "file"), "file block", provider)
      .ingest_keys(b$file, c("file_id", "file_data", "filename"), "file", provider)
      if (!is.null(b$file$filename)) .unsupported(provider, "file filename in canonical content")
      if (sum(!vapply(b$file[c("file_id", "file_data")], is.null, logical(1))) != 1L) stop("File needs exactly one of file_id or file_data.", call. = FALSE)
      if (!is.null(b$file$file_id)) p <- document_part(file_id = b$file$file_id)
      else { uri <- .ingest_data_uri(b$file$file_data); p <- document_part(data = uri$data, media_type = uri$media_type) }
    } else if (identical(k, "refusal") && role == "assistant") {
      .ingest_keys(b, c("type", "refusal"), "refusal block", provider); p <- refusal(b$refusal)
    } else .unsupported(provider, paste("content block", .wire_string(k), "in", role, "message"))
    parts[[length(parts) + 1L]] <- p
  }
  list(parts = parts, marked = marked)
}
.ingest_data_uri <- function(url) {
  .string(url, "data URI")
  match <- regmatches(url, regexec("^data:([^,;]+);base64,(.+)$", url))[[1L]]
  if (length(match) != 3L) stop("Expected data:<media-type>;base64,<data>.", call. = FALSE)
  list(media_type = match[[2L]], data = match[[3L]])
}

request_from_openai_chat <- function(body, ..., compat = NULL, provider = "openai-chat") {
  .check_dots(...)
  b <- .decode_body(body); provider <- canonical_provider(provider)
  d <- .definition(provider)
  if (d$dialect != "openai-chat") .unsupported(provider, "Chat Completions ingest on a different dialect")
  preset <- compat %||% d$compat %||% "openai"
  if (is.character(preset)) {
    preset <- gsub("-", "_", preset, fixed = TRUE)
    aliases <- c(openai_chat = "openai", lm_studio = "lmstudio")
    if (preset %in% names(aliases)) preset <- unname(aliases[[preset]])
    preset <- .provider_tables()$chat[[preset]]
    if (is.null(preset)) stop("Unknown Chat compatibility preset.", call. = FALSE)
  }
  policy <- .compat(list(definition = d, compat = preset), request(b$model, list(message_user(""))))
  passthrough <- c("seed", "logit_bias", "presence_penalty", "frequency_penalty", "metadata", "verbosity", "moderation", "provider")
  controls <- c("model", "messages", "tools", "functions", "tool_choice", "function_call", "parallel_tool_calls", "max_completion_tokens", "max_tokens", "temperature", "top_p", "top_k", "stop", "logprobs", "top_logprobs", "response_format", "service_tier", "store", "user", "safety_identifier", "user_id", "reasoning_effort", "reasoning", "thinking", "enable_thinking", "chat_template_kwargs", "reasoning_format", "prompt_cache_key", "prompt_cache_retention", "prompt_cache_options", "stream", "stream_options")
  .ingest_keys(b, c(controls, passthrough), "request", provider)
  if (!.is_array(b$messages)) stop("messages must be an array.", call. = FALSE)
  if (!is.null(b$stream)) .coerce_field(b$stream, "bool", "stream", FALSE)
  if (!is.null(b$stream_options) && !.is_object(b$stream_options)) stop("stream_options must be an object.", call. = FALSE)
  if (!is.null(b$tools) && !.is_array(b$tools)) stop("tools must be an array.", call. = FALSE)
  messages <- list(); system <- NULL; pending <- list(); boundary <- NULL; stable <- FALSE
  flush <- function() {
    if (length(pending)) { messages[[length(messages) + 1L]] <<- message("tool", pending); pending <<- list() }
  }
  mark <- function(is_system) {
    if (stable || !is.null(boundary)) stop("Only one cache breakpoint can be represented.", call. = FALSE)
    if (is_system) stable <<- TRUE else boundary <<- length(messages)
  }
  for (i in seq_along(b$messages)) {
    row <- b$messages[[i]]; .ingest_keys(row, c("role", "content", "name", "tool_calls", "refusal", "reasoning_content", "audio", "function_call", "annotations", "provider_specific_fields", "thinking_blocks", "images", "tool_call_id"), "message", provider)
    role <- .string(row$role, "message role")
    if (!is.null(row$name) && role != "tool") .unsupported(provider, "per-message participant name")
    if (role != "tool") flush()
    if (role %in% c("system", "developer", "user")) {
      .ingest_keys(row, c("role", "content", if (role == "user") "name"), "prompt message", provider)
      value <- .ingest_content(row$content, if (role == "user") "user" else "system", provider)
      is_system <- i == 1L && role %in% c("system", "developer")
      if (value$marked) mark(is_system)
      if (is_system) system <- if (length(value$parts) == 1L && value$parts[[1L]]$type == "text") value$parts[[1L]]$text else value$parts
      else messages[[length(messages) + 1L]] <- message(if (role == "user") "user" else "developer", value$parts)
    } else if (role == "tool") {
      .ingest_keys(row, c("role", "content", "name", "tool_call_id"), "tool message", provider)
      value <- .ingest_content(row$content, "tool", provider)
      if (value$marked) stop("Tool rows cannot carry cache breakpoints.", call. = FALSE)
      pending[[length(pending) + 1L]] <- tool_result_part(row$tool_call_id, value$parts, name = row$name)
    } else if (role == "assistant") {
      .ingest_keys(row, c("role", "content", "name", "tool_calls", "refusal", "reasoning_content", "audio", "function_call", "annotations", "provider_specific_fields", "thinking_blocks", "images"), "assistant message", provider)
      if (!is.null(row$audio) || !is.null(row$function_call)) .unsupported(provider, "legacy function call or assistant audio reference")
      for (key in c("provider_specific_fields", "thinking_blocks", "images")) {
        v <- row[[key]]
        if (!is.null(v) && length(v) && !(.is_object(v) && all(vapply(v, function(x) is.null(x) || (is.list(x) && !length(x)), logical(1))))) .unsupported(provider, paste("client-only field", key))
      }
      parts <- list()
      if (!is.null(row$reasoning_content)) parts <- list(thinking(row$reasoning_content))
      if (!is.null(row$content)) {
        value <- .ingest_content(row$content, "assistant", provider)
        if (value$marked) stop("Assistant rows cannot carry cache breakpoints.", call. = FALSE)
        parts <- c(parts, value$parts)
      }
      if (!is.null(row$refusal)) parts <- c(parts, list(refusal(row$refusal)))
      if (!is.null(row$tool_calls) && !.is_array(row$tool_calls)) stop("tool_calls must be an array.", call. = FALSE)
      for (tc in .wire_array(row$tool_calls)) {
        .ingest_keys(tc, c("id", "type", "function"), "tool call", provider)
        if (!identical(tc$type %||% "function", "function")) .unsupported(provider, "non-function call")
        fn <- tc[["function"]]; .ingest_keys(fn, c("name", "arguments"), "tool call function", provider)
        args <- fn$arguments %||% "{}"
        if (is.character(args)) args <- .json_decode(if (nzchar(args)) args else "{}")
        if (!.is_object(args)) stop("Tool arguments must encode a JSON object.", call. = FALSE)
        parts <- c(parts, list(tool_call_part(tc$id, fn$name, input = args)))
      }
      if (!is.null(row$annotations)) {
        if (!.is_array(row$annotations)) stop("annotations must be an array.", call. = FALSE)
        for (a in row$annotations) {
          .ingest_keys(a, c("type", "url_citation"), "annotation", provider)
          if (!identical(a$type, "url_citation")) .unsupported(provider, "non-URL annotation")
          .ingest_keys(a$url_citation, c("url", "title", "start_index", "end_index"), "URL citation", provider)
          cited <- .citation_wire(a$url_citation, if (is.character(row$content)) row$content else NULL)
          if (!is.null(cited)) parts <- c(parts, list(cited))
        }
      }
      if (!length(parts)) parts <- list(text(""))
      messages[[length(messages) + 1L]] <- message_assistant(parts)
    } else .unsupported(provider, paste("message role", role))
  }
  flush()
  # The deprecated functions / function_call shape is a spelling of tools /
  # tool_choice, and is translated (MAP-13: a spelling change is never a refusal).
  if (!is.null(b$functions) && !is.null(b$tools)) stop("functions and tools cannot both be given.", call. = FALSE)
  if (!is.null(b$function_call) && !is.null(b$tool_choice)) stop("function_call and tool_choice cannot both be given.", call. = FALSE)
  if (!is.null(b$functions)) {
    if (!.is_array(b$functions)) stop("functions must be an array.", call. = FALSE)
    b$tools <- lapply(b$functions, function(fn) json_object(type = "function", "function" = fn))
  }
  if (!is.null(b$function_call)) {
    fc <- b$function_call
    b$tool_choice <- if (is.character(fc) && fc %in% c("none", "auto")) fc
      else if (.is_object(fc) && !is.null(fc$name)) json_object(type = "function", "function" = json_object(name = fc$name))
      else stop("function_call must be 'none', 'auto', or an object with a name.", call. = FALSE)
  }
  tools <- list()
  for (entry in .wire_array(b$tools)) {
    if (identical(entry$type, "function")) {
      .ingest_keys(entry, c("type", "function"), "tool", provider)
      fn <- entry[["function"]]; .ingest_keys(fn, c("name", "description", "parameters", "strict"), "function", provider)
      if (!is.null(fn$strict)) { .coerce_field(fn$strict, "bool", "function strict", FALSE); if (fn$strict) .unsupported(provider, "strict function schema") }
      tools[[length(tools) + 1L]] <- function_tool(fn$name, description = fn$description, parameters = fn$parameters %||% json_object(type = "object", properties = json_object()))
    } else if (policy$builtin_tools == "groq" && !is.null(entry$type) && entry$type %in% c("browser_search", "code_interpreter")) {
      cfg <- entry[setdiff(names(entry), "type")]
      tools[[length(tools) + 1L]] <- builtin_tool(if (entry$type == "browser_search") "web_search" else "code_execution", config = .json_object(cfg))
    } else .unsupported(provider, "tool type")
  }
  fields <- list()
  for (key in c("temperature", "top_p", "top_k", "stop", "service_tier", "store")) if (key %in% names(b)) fields[key] <- list(b[[key]])
  if (!is.null(b$max_tokens) && !is.null(b$max_completion_tokens) && .number(b$max_tokens, "max_tokens", TRUE) != .number(b$max_completion_tokens, "max_completion_tokens", TRUE)) stop("Token limits disagree.", call. = FALSE)
  fields$max_tokens <- b$max_completion_tokens %||% b$max_tokens
  if (!is.null(b$logprobs)) .coerce_field(b$logprobs, "bool", "logprobs", FALSE)
  if (isTRUE(b$logprobs)) fields$logprobs <- b$top_logprobs %||% 0L
  else if (!is.null(b$top_logprobs)) stop("top_logprobs requires logprobs = TRUE.", call. = FALSE)
  user_keys <- intersect(names(b), c("user", "user_id", "safety_identifier"))
  if (length(user_keys) > 1L) stop("End-user identifiers disagree.", call. = FALSE)
  if ("user_id" %in% user_keys && policy$user_field != "user_id") .unsupported(provider, "foreign user_id spelling")
  if (length(user_keys)) fields$user_id <- b[[user_keys[[1L]]]]
  if (!is.null(b$response_format)) {
    f <- b$response_format
    if (identical(f$type, "text")) .ingest_keys(f, "type", "response_format", provider)
    else if (identical(f$type, "json_object")) { .ingest_keys(f, "type", "response_format", provider); fields$response_format <- f }
    else if (identical(f$type, "json_schema")) {
      .ingest_keys(f, c("type", "json_schema"), "response_format", provider)
      s <- f$json_schema; .ingest_keys(s, c("name", "schema", "strict"), "response schema", provider)
      out <- json_object(type = "json_schema", schema = s$schema)
      if (!is.null(s$name) && s$name != "response") out$name <- s$name
      if (!is.null(s$strict)) out$strict <- s$strict
      fields$response_format <- out
    } else stop("Unknown response_format type.", call. = FALSE)
  }
  tc <- b$tool_choice; mode <- NULL; allowed <- list()
  if (is.character(tc)) mode <- tc
  else if (!is.null(tc)) {
    .ingest_keys(tc, c("type", "function", "allowed_tools"), "tool_choice", provider)
    if (identical(tc$type, "function")) { .ingest_keys(tc[["function"]], "name", "forced tool", provider); mode <- "required"; allowed <- list(tc[["function"]]$name) }
    else if (identical(tc$type, "allowed_tools")) {
      .ingest_keys(tc$allowed_tools, c("mode", "tools"), "allowed_tools", provider)
      mode <- tc$allowed_tools$mode
      allowed <- lapply(tc$allowed_tools$tools, function(t) {
        .ingest_keys(t, c("type", "function"), "allowed tool", provider)
        if (!identical(t$type, "function")) .unsupported(provider, "non-function tool choice")
        .ingest_keys(t[["function"]], "name", "allowed function", provider); t[["function"]]$name
      })
      if (!length(allowed)) stop("Allowed tools cannot be empty.", call. = FALSE)
    } else .unsupported(provider, "tool_choice form")
  }
  if (!is.null(mode) || !is.null(b$parallel_tool_calls)) fields$tool_choice <- tool_choice(mode = mode %||% "auto", allowed = allowed, parallel = b$parallel_tool_calls)
  reason_keys <- intersect(names(b), c("reasoning_effort", "reasoning", "thinking", "enable_thinking", "chat_template_kwargs", "reasoning_format"))
  spellings <- switch(policy$thinking_format, reasoning_effort = "reasoning_effort", openrouter = "reasoning", deepseek = c("thinking", "reasoning_effort"), kimi = c("thinking", "reasoning_effort"), qwen = "enable_thinking", qwen_chat_template = "chat_template_kwargs", character())
  if (policy$builtin_tools == "groq") spellings <- c(spellings, "reasoning_format")
  if (any(!reason_keys %in% spellings)) .unsupported(provider, "another server's reasoning spelling")
  effort <- b$reasoning_effort; off <- identical(effort, "none")
  if (off) effort <- NULL
  if (!is.null(b$thinking)) {
    .ingest_keys(b$thinking, "type", "thinking", provider)
    if (identical(b$thinking$type, "disabled")) { if (!is.null(effort)) stop("Thinking disable conflicts with effort.", call. = FALSE); off <- TRUE }
    else if (!identical(b$thinking$type, "enabled")) stop("Unknown thinking type.", call. = FALSE)
    else if (is.null(effort)) .unsupported(provider, "thinking enabled without an effort level")
  }
  if (!is.null(b$reasoning)) {
    .ingest_keys(b$reasoning, c("effort", "enabled"), "reasoning", provider)
    if (identical(b$reasoning$enabled, FALSE)) off <- TRUE else effort <- b$reasoning$effort
    if (!off && is.null(effort)) stop("reasoning needs effort or enabled = FALSE.", call. = FALSE)
  }
  flag <- b$enable_thinking
  if (!is.null(b$chat_template_kwargs)) { .ingest_keys(b$chat_template_kwargs, c("enable_thinking", "preserve_thinking"), "chat_template_kwargs", provider); flag <- b$chat_template_kwargs$enable_thinking }
  if (!is.null(flag)) { .coerce_field(flag, "bool", "enable_thinking", FALSE); if (flag) .unsupported(provider, "thinking enabled without a canonical effort level"); off <- TRUE }
  # seed and the two penalties are canonical Config fields (promoted 2026-09-14); the rest passes verbatim.
  for (name in c("seed", "frequency_penalty", "presence_penalty")) if (!is.null(b[[name]])) fields[name] <- list(b[[name]])
  summary <- NULL; extensions <- .json_object(b[setdiff(intersect(names(b), passthrough), c("seed", "frequency_penalty", "presence_penalty"))])
  if (!is.null(b$reasoning_format)) {
    if (!identical(b$reasoning_format, "parsed")) .unsupported(provider, "reasoning_format value")
    if (is.null(effort)) extensions$reasoning_format <- "parsed" else summary <- "auto"
  }
  if (off) fields$reasoning <- reasoning("off") else if (!is.null(effort)) fields$reasoning <- reasoning(effort, summary = summary)
  cache_keys <- intersect(names(b), c("prompt_cache_key", "prompt_cache_retention", "prompt_cache_options"))
  if (length(cache_keys) || stable || !is.null(boundary)) {
    if (!policy$cache_control %in% c("openai", "openai_implicit")) .unsupported(provider, "OpenAI cache control on this preset")
    if ((stable || !is.null(boundary)) && policy$cache_control != "openai") .unsupported(provider, "explicit breakpoint on an implicit-only cache")
    retention <- if (!is.null(b$prompt_cache_retention)) { if (b$prompt_cache_retention != "24h") .unsupported(provider, "cache retention value"); "long" } else NULL
    explicit <- FALSE
    if (!is.null(b$prompt_cache_options)) {
      .ingest_keys(b$prompt_cache_options, "mode", "prompt_cache_options", provider)
      if (!identical(b$prompt_cache_options$mode, "explicit")) .unsupported(provider, "cache mode spelling")
      explicit <- TRUE
    }
    fields$cache <- cache_config(mode = if (explicit && !stable && is.null(boundary)) "off" else "auto", retention = retention, key = b$prompt_cache_key, prefix = if (stable) "stable" else NULL, prefix_until_index = boundary)
  }
  fields$extensions <- extensions
  request(b$model, messages, system = system, tools = tools, config = .new_value("Config", fields))
}
