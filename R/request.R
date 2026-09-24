.compat_defaults <- list(
  chat = list(instruction_role = "system", max_tokens_field = "max_completion_tokens", stream_usage = "include", tool_result_name = "omit", assistant_after_tool_result = "omit", thinking_format = "reasoning_effort", thinking_replay = "as_text", assistant_reasoning_content = "omit", strict_tools = "omit", builtin_tools = "reject", tool_result_media = "reject", cache_control = "openai", user_field = "user", forced_tool_choice = "send", json_schema = "send"),
  responses = list(developer_role = "developer", max_output_tokens_field = "max_output_tokens", reasoning_format = "responses_reasoning", tool_result_name = "omit", strict_tools = "omit", cache_control = "openai", commentary_phase = "omit", edit_image_field = "array", builtin_tools = "openai", tool_result_media = "native"),
  anthropic = list(thinking_format = "anthropic", thinking_replay = "signed", cache_control = "anthropic", structured_output = "send", parallel_tool_calls = "send", sampling_params = "send", tool_result_media = "native")
)
.compat <- function(lm, request) {
  key <- switch(lm$definition$dialect, "openai-chat" = "chat", "openai-responses" = "responses", anthropic = "anthropic", NULL)
  if (is.null(key)) return(list())
  out <- lm$compat
  if (key == "chat") for (entry in out$model_overrides %||% list()) {
    if (startsWith(request$model, entry[[1L]])) { out[names(entry[[2L]])] <- entry[[2L]]; break }
  }
  if (key == "responses") {
    e <- request$config$extensions
    override <- e$openai_responses_compat %||% e$openai_compat %||% e$compat$openai_responses %||% e$compat$openai
    if (.is_object(override)) out[names(override)] <- override
  }
  for (name in names(.compat_defaults[[key]])) if (is.null(out[[name]]) || identical(out[[name]], "auto")) out[[name]] <- .compat_defaults[[key]][[name]]
  out
}
.media_kinds <- c("image", "audio", "video", "document", "binary")
.parts_text <- function(parts, provider) {
  if (any(vapply(parts, function(p) p$type %in% .media_kinds, logical(1)))) .unsupported(provider, "media in a text-only field")
  bits <- lapply(parts, function(p) switch(p$type, text = p$text, thinking = if (nzchar(p$text)) p$text else NULL, citation = paste(Filter(Negate(is.null), list(p$title, p$url, p$text)), collapse = " \u2014 "), refusal = p$text, .unsupported(provider, paste("part", p$type, "in a text-only field"))))
  paste(Filter(Negate(is.null), bits), collapse = "\n")
}
.media_base64 <- function(p) if (!is.null(p$data)) p$data else .base64_encode(media_bytes(p))
.media_uri <- function(p) paste0("data:", p$media_type, ";base64,", .media_base64(p))
.native_block <- function(p, dialect, provider, compat, call_names = list()) {
  k <- p$type
  if (k == "tool_result") {
    media <- Filter(function(v) v$type %in% .media_kinds, p$content)
    allowed <- if (dialect == "gemini") c("image", "document") else switch(compat$tool_result_media, native = c("image", "document"), images = "image", character())
    if (any(!vapply(media, function(v) v$type, "") %in% allowed)) .unsupported(provider, "tool-result media")
    if (dialect == "gemini") {
      name <- p$name %||% call_names[[p$id]]
      if (is.null(name)) .unsupported(provider, "a tool result without an explicit or preceding function name")
      words <- Filter(function(v) !v$type %in% .media_kinds, p$content)
      out <- json_object(name = name, response = if (p$is_error) json_object(error = .parts_text(words, provider)) else if (!length(words)) json_object() else json_object(result = .parts_text(words, provider)), id = p$id)
      if (length(media)) out$parts <- lapply(media, .native_block, dialect = dialect, provider = provider, compat = compat)
      return(json_object(functionResponse = out))
    }
    if (dialect == "anthropic") {
      blocks <- lapply(p$content, .native_block, dialect = dialect, provider = provider, compat = compat)
      content <- if (length(blocks) == 1L && blocks[[1L]]$type == "text") blocks[[1L]]$text else blocks
      out <- json_object(type = "tool_result", tool_use_id = p$id, content = content)
      if (p$is_error) out$is_error <- TRUE
      return(out)
    }
    if (!length(media)) return(paste0(if (p$is_error) "[error] " else "", .parts_text(p$content, provider)))
    blocks <- lapply(p$content, .native_block, dialect = dialect, provider = provider, compat = compat)
    if (p$is_error) {
      i <- which(vapply(blocks, function(b) b$type %in% c("text", "input_text"), logical(1)))
      if (length(i)) blocks[[i[[1L]]]]$text <- paste0("[error] ", blocks[[i[[1L]]]]$text)
      else blocks <- c(list(json_object(type = if (dialect == "chat") "text" else "input_text", text = "[error]")), blocks)
    }
    return(blocks)
  }
  if (k == "tool_call") {
    if (dialect == "anthropic") return(json_object(type = "tool_use", id = p$id, name = p$name, input = p$input))
    if (dialect == "gemini") {
      out <- json_object(functionCall = json_object(name = p$name, args = p$input, id = p$id))
      state <- continuation_data(p, "gemini", "thought_signature")
      if (!is.null(state$value)) out$thoughtSignature <- state$value
      return(out)
    }
    .unsupported(provider, "tool call inside a content block")
  }
  if (k %in% c("text", "thinking", "citation", "refusal")) {
    if (k == "thinking" && dialect == "anthropic") {
      redacted <- continuation_data(p, "anthropic", "redacted_thinking")
      if (!is.null(redacted)) return(.json_object(c(list(type = "redacted_thinking"), unclass(redacted))))
      signature <- continuation_data(p, "anthropic", "thinking_signature")
      if (!is.null(signature$signature)) return(json_object(type = "thinking", thinking = p$text, signature = signature$signature))
      if (compat$thinking_replay == "unsigned" && nzchar(p$text)) return(json_object(type = "thinking", thinking = p$text))
    }
    words <- .parts_text(list(p), provider)
    if (dialect == "gemini") {
      out <- json_object(text = words)
      state <- continuation_data(p, "gemini", "thought_signature")
      if (!is.null(state$value)) {
        out$thoughtSignature <- state$value
        if (k == "thinking") out$thought <- TRUE
      }
      return(out)
    }
    return(json_object(type = if (dialect == "responses") "input_text" else "text", text = words))
  }
  if (dialect == "chat") {
    if (k != "image" || !is.null(p$file_id)) .unsupported(provider, paste(k, "content or file-id image on Chat Completions"))
    out <- json_object(url = p$url %||% .media_uri(p))
    if (!is.null(p$detail)) out$detail <- p$detail
    return(json_object(type = "image_url", image_url = out))
  }
  if (dialect == "anthropic") {
    if (!k %in% c("image", "document")) .unsupported(provider, paste(k, "content"))
    source <- if (!is.null(p$url)) json_object(type = "url", url = p$url) else if (!is.null(p$file_id)) json_object(type = "file", file_id = p$file_id) else json_object(type = "base64", media_type = p$media_type, data = .media_base64(p))
    return(json_object(type = k, source = source))
  }
  if (dialect == "gemini") {
    uri <- p$url %||% p$file_id
    if (!is.null(uri)) return(json_object(fileData = json_object(mimeType = p$media_type, fileUri = uri)))
    return(json_object(inlineData = json_object(mimeType = p$media_type, data = .media_base64(p))))
  }
  if (k == "image") {
    if (!is.null(p$file_id)) return(json_object(type = "input_image", file_id = p$file_id))
    out <- json_object(type = "input_image", image_url = p$url %||% .media_uri(p))
    if (!is.null(p$detail)) out$detail <- p$detail
    return(out)
  }
  if (k == "audio") {
    if (!is.null(p$url)) return(json_object(type = "input_audio", audio_url = p$url))
    if (!is.null(p$file_id)) return(json_object(type = "input_audio", file_id = p$file_id))
    format <- sub("^[^/]+/", "", p$media_type); if (format == "mpeg") format <- "mp3"
    return(json_object(type = "input_audio", audio = .media_base64(p), format = format))
  }
  if (k %in% c("document", "binary")) {
    if (!is.null(p$url)) return(json_object(type = "input_file", file_url = p$url))
    if (!is.null(p$file_id)) return(json_object(type = "input_file", file_id = p$file_id))
    ext <- sub("\\+.*$", "", sub("^[^/]+/", "", p$media_type))
    return(json_object(type = "input_file", filename = paste0("file.", ext), file_data = .media_uri(p)))
  }
  if (k == "video") {
    if (!is.null(p$url)) return(json_object(type = "input_video", video_url = p$url))
    if (!is.null(p$file_id)) return(json_object(type = "input_video", file_id = p$file_id))
    return(json_object(type = "input_video", video_data = .media_uri(p)))
  }
  .unsupported(provider, paste("part", k))
}

.builtin <- function(tool, dialect, compat, provider) {
  maps <- list(responses = c(web_search = "web_search_preview", code_execution = "code_interpreter", file_search = "file_search", computer_use = "computer_use_preview"), anthropic = c(web_search = "web_search_20250305", code_execution = "code_execution_20250522"), gemini = c(web_search = "googleSearch", code_execution = "codeExecution"), chat = c(web_search = "browser_search", code_execution = "code_interpreter"))
  map <- maps[[dialect]]
  if (dialect == "chat" && (compat$builtin_tools != "groq" || !tool$name %in% names(map))) .unsupported(provider, "builtin tool")
  name <- if (dialect == "responses" && compat$builtin_tools == "verbatim") tool$name else if (tool$name %in% names(map)) unname(map[[tool$name]]) else tool$name
  if (dialect == "gemini") return(.json_object(setNames(list(tool$config %||% json_object()), name)))
  out <- json_object(type = name)
  if (dialect == "anthropic") out$name <- tool$name
  if (!is.null(tool$config)) out[names(tool$config)] <- tool$config
  out
}
.tool_wire <- function(tool, dialect, compat, provider) {
  if (tool$type == "builtin") return(.builtin(tool, dialect, compat, provider))
  if (dialect == "anthropic") return(json_object(name = tool$name, description = tool$description, input_schema = tool$parameters))
  out <- json_object(name = tool$name, description = tool$description, parameters = tool$parameters)
  if (dialect %in% c("chat", "responses") && compat$strict_tools == "include") out$strict <- FALSE
  if (dialect == "chat") return(json_object(type = "function", "function" = out))
  if (dialect == "responses") out <- .json_object(c(list(type = "function"), out))
  out
}
.tool_choice_wire <- function(req, dialect, compat, provider) {
  tc <- req$config$tool_choice
  if (is.null(tc)) return(NULL)
  mode <- tc$mode; names <- unlist(tc$allowed, use.names = FALSE)
  tools <- setNames(req$tools, vapply(req$tools, function(t) t$name, ""))
  if (dialect %in% c("chat", "gemini") && any(vapply(tools[names], function(t) !is.null(t) && t$type == "builtin", logical(1)))) .unsupported(provider, "named builtin tool choice")
  if (dialect == "gemini") {
    if (identical(tc$parallel, FALSE))
      .adapt("config.tool_choice.parallel", "dropped", "GenerateContent has no parallel-tool-calls knob and may return several calls (OpenAI and Anthropic carry it)", asked = FALSE)
    out <- json_object(mode = switch(mode, none = "NONE", required = "ANY", auto = if (length(names)) "VALIDATED" else "AUTO"))
    if (length(names)) out$allowedFunctionNames <- as.list(names)
    return(json_object(functionCallingConfig = out))
  }
  if (dialect == "anthropic") {
    out <- json_object(type = switch(mode, required = "any", mode))
    # A proper-subset allowlist was narrowed to the tools sent (see .build_payload): any/auto over them.
    if (length(names) == 1L && mode == "required") out <- json_object(type = "tool", name = names[[1L]])
    if (identical(tc$parallel, FALSE) && mode != "none") {
      if (compat$parallel_tool_calls == "reject")
        .adapt("config.tool_choice.parallel", "dropped", "this server accepts disable_parallel_tool_use and does not apply it (guide--anthropic-api.md); the model may return several calls", asked = FALSE)
      else out$disable_parallel_tool_use <- TRUE
    } else if (!is.null(tc$parallel) && compat$parallel_tool_calls == "reject")
      .adapt("config.tool_choice.parallel", "dropped", "this server accepts disable_parallel_tool_use and does not apply it (guide--anthropic-api.md); the model may return several calls", asked = tc$parallel)
    return(out)
  }
  if (!length(names)) return(mode)
  entries <- lapply(unname(tools[names]), function(t) {
    if (t$type == "builtin") return(json_object(type = .builtin(t, dialect, compat, provider)$type))
    if (dialect == "chat") json_object(type = "function", "function" = json_object(name = t$name)) else json_object(type = "function", name = t$name)
  })
  if (length(entries) == 1L && mode == "required") return(entries[[1L]])
  if (dialect == "chat") json_object(type = "allowed_tools", allowed_tools = json_object(mode = mode, tools = entries)) else json_object(type = "allowed_tools", mode = mode, tools = entries)
}

.effort_budgets <- c(minimal = 1024L, low = 2048L, medium = 8192L, high = 16384L, xhigh = 24576L, max = 32768L)
.adaptive_class <- function(model) any(vapply(c("sonnet-5", "opus-5", "sonnet-4-6", "opus-4-6", "opus-4-7", "opus-4-8", "fable", "mythos", "haiku-5"), function(s) grepl(s, tolower(model), fixed = TRUE), logical(1)))
.cache_options_class <- function(model) {
  m <- regmatches(tolower(model), regexec("^gpt-([0-9]+)\\.([0-9]+)", tolower(model)))[[1L]]
  length(m) == 3L && (as.integer(m[[2L]]) > 5L || (as.integer(m[[2L]]) == 5L && as.integer(m[[3L]]) >= 6L))
}

.reasoning_wire <- function(req, dialect, compat, provider, payload, visible_max_tokens = NULL) {
  r <- req$config$reasoning
  if (is.null(r)) return(payload)
  off <- r$effort == "off"
  detail <- !is.null(r$summary) && r$summary %in% c("concise", "detailed")
  if (dialect == "gemini") {
    level <- grepl("^(models/)?gemini-3", tolower(req$model))
    if (off && level) {
      .adapt("config.reasoning.effort", "substituted", paste0(req$model, " cannot disable thinking (the Gemini 3 class honours no off switch); the lowest level was sent and the thinking spend is visible in usage"), asked = "off", applied = "minimal")
      r$effort <- "minimal"; off <- FALSE
    }
    t <- json_object()
    if (off) t$thinkingBudget <- 0L else {
      if (detail) {
        .adapt("config.reasoning.summary", "substituted", "GenerateContent has includeThoughts only, no detail levels; 'auto' shows the thoughts", asked = r$summary, applied = "auto")
        r$summary <- "auto"
      }
      if (!is.null(r$summary)) t$includeThoughts <- TRUE
      if (!is.null(r$thinking_budget)) t$thinkingBudget <- r$thinking_budget
      else if (level) {
        effort <- r$effort
        if (effort %in% c("xhigh", "max")) {
          .adapt("config.reasoning.effort", "clamped", "the Gemini 3 class has thinkingLevel minimal|low|medium|high; 'high' is the ceiling", asked = effort, applied = "high")
          effort <- "high"
        }
        t$thinkingLevel <- effort
      } else t$thinkingBudget <- unname(.effort_budgets[[r$effort]])
    }
    payload$generationConfig$thinkingConfig <- t
    return(payload)
  }
  if (dialect == "anthropic") {
    fmt <- compat$thinking_format
    adaptive <- fmt != "anthropic" || .adaptive_class(req$model)
    if (off) { if (fmt != "anthropic") payload$thinking <- json_object(type = "disabled"); return(payload) }
    if (!is.null(compat$reasoning_efforts) && !r$effort %in% unlist(compat$reasoning_efforts)) {
      nearest <- .nearest_effort(r$effort, compat$reasoning_efforts)
      .adapt("config.reasoning.effort", "clamped", paste0("this server has no '", r$effort, "' level (it accepts ", paste(unlist(compat$reasoning_efforts), collapse = ", "), ") and would have accepted the word silently"), asked = r$effort, applied = nearest)
      r$effort <- nearest
    }
    if (detail) .adapt("config.reasoning.summary", "substituted", "the Messages API has no summary detail levels; it returns thinking blocks whenever thinking runs, which is 'auto'", asked = r$summary, applied = "auto")
    if (adaptive) {
      if (!is.null(r$thinking_budget))
        .adapt("config.reasoning.thinking_budget", "dropped", if (fmt == "deepseek") "this server ignores budget_tokens; effort is the dial" else if (fmt == "adaptive") "this server accepts budget_tokens without translating it; effort is the dial (protocols--messages.md)" else paste0(req$model, " takes thinking.type 'adaptive' with output_config.effort; budget_tokens is rejected by the API (live 2026-09-02)"), asked = r$thinking_budget)
      if (fmt == "anthropic" && r$effort == "minimal") {
        .adapt("config.reasoning.effort", "clamped", "this model class has no 'minimal' level (output_config.effort is low|medium|high|xhigh|max); 'low' is the floor", asked = "minimal", applied = "low")
        r$effort <- "low"
      }
      if (fmt != "effort") payload$thinking <- json_object(type = if (fmt == "deepseek") "enabled" else "adaptive")
      payload$output_config <- json_object(effort = r$effort)
    } else {
      budget <- r$thinking_budget %||% unname(.effort_budgets[[r$effort]])
      payload$thinking <- json_object(type = "enabled", budget_tokens = budget)
      payload$max_tokens <- budget + visible_max_tokens
    }
    return(payload)
  }
  fmt <- if (dialect == "chat") compat$thinking_format else compat$reasoning_format
  if (dialect == "chat" && fmt == "none") {
    .adapt("config.reasoning", "dropped", "this server has no reasoning dial on its wire (compat thinking_format='none'); the model reasons at its own default; pass the server's own knob through extensions", asked = json_object(effort = r$effort))
    return(payload)
  }
  word <- if (off) "none" else r$effort
  if (!off) {
    if (!is.null(r$thinking_budget))
      .adapt("config.reasoning.thinking_budget", "dropped", if (dialect == "chat") "the Chat Completions wire has no thinking token budget; effort carries the intent" else "this wire has no thinking token budget; effort carries the intent (Anthropic's manual class and Gemini take a budget)", asked = r$thinking_budget)
    if (detail && !(dialect == "responses" && fmt == "responses_reasoning")) {
      .adapt("config.reasoning.summary", "substituted", if (dialect == "chat") "the Chat Completions wire has no summary detail levels; 'auto' is what it shows" else "this wire has no summary detail levels; 'auto' is what it shows", asked = r$summary, applied = "auto")
      if (dialect == "chat") r$summary <- "auto"
    }
    if (dialect == "chat" && !is.null(compat$reasoning_efforts) && !word %in% unlist(compat$reasoning_efforts)) {
      nearest <- .nearest_effort(word, compat$reasoning_efforts)
      .adapt("config.reasoning.effort", "clamped", paste0("this server has no '", word, "' level (it accepts ", paste(unlist(compat$reasoning_efforts), collapse = ", "), ") and would have accepted the word silently"), asked = word, applied = nearest)
      word <- nearest
    }
  }
  if (fmt == "responses_reasoning") { payload$reasoning <- json_object(effort = word); if (!off && !is.null(r$summary)) payload$reasoning$summary <- r$summary }
  if (fmt == "reasoning_effort" || (fmt == "kimi" && !off)) payload$reasoning_effort <- word
  if (fmt == "openrouter") payload$reasoning <- if (off) json_object(enabled = FALSE) else json_object(effort = word)
  if (fmt == "deepseek" || (fmt == "kimi" && off)) {
    payload$thinking <- json_object(type = if (off) "disabled" else "enabled")
    if (!off) payload$reasoning_effort <- word
  }
  if (fmt %in% c("qwen", "zai")) payload$enable_thinking <- !off
  if (fmt == "qwen_chat_template") { payload$chat_template_kwargs <- json_object(enable_thinking = !off); if (!off) payload$chat_template_kwargs$preserve_thinking <- TRUE }
  if (!off && dialect == "chat" && compat$builtin_tools == "groq" && identical(r$summary, "auto")) payload$reasoning_format <- "parsed"
  payload
}

# The Messages API requires max_tokens; when the caller set none, the class
# default is used and recorded (MAP-13 defaulted). Output ceilings by class.
.anthropic_default_max_tokens <- function(model) {
  lowered <- tolower(model)
  for (pair in list(c("claude-3-haiku", 4096), c("claude-3-opus", 4096), c("claude-3-sonnet", 4096), c("claude-3-5-", 8192), c("claude-3.5-", 8192)))
    if (grepl(pair[[1L]], lowered, fixed = TRUE)) return(as.integer(pair[[2L]]))
  16384L
}

# xAI's own adaptations before the Chat Completions build (Python XaiLM._payload).
.xai_prepare <- function(req, provider) {
  r <- req$config$reasoning
  if (!is.null(r) && r$effort == "off") {
    .adapt("config.reasoning.effort", "substituted", "Grok reasoning models have no off switch and api.x.ai ignores disable fields (158 reasoning tokens on an explicit off, live 2026-09-01); the lowest level was sent", asked = "off", applied = "low")
    r$effort <- "low"; req$config$reasoning <- r
  }
  if (!is.null(req$config$logprobs)) {
    .adapt("config.logprobs", "dropped", "grok-4.20 and newer ignore logprobs/top_logprobs (docs.x.ai, live 2026-09-01); Response.logprobs will be absent (OpenAI and Gemini carry them)", asked = req$config$logprobs)
    req$config$logprobs <- NULL
  }
  tc <- req$config$tool_choice
  names <- if (is.null(tc)) character() else unlist(tc$allowed, use.names = FALSE)
  if (length(names) && !(length(names) == 1L && tc$mode == "required")) {
    kept <- Filter(function(t) t$name %in% names, req$tools)
    .adapt("config.tool_choice.allowed", "client_side", "api.x.ai ignores tool_choice allowlists (live 2026-09-02); only the allowed tools were sent, which is what the allowlist means", asked = as.list(names), applied = lapply(kept, function(t) t$name))
    req$tools <- kept; tc$allowed <- list(); req$config$tool_choice <- tc
  }
  if (!is.null(tc) && tc$mode == "required" && !is.null(req$config$response_format))
    stop(lm15_error("xai: a forced tool (mode='required') cannot be combined with response_format \u2014 api.x.ai returns JSON text and drops the call (verified live 2026-09-02)", code = "unsupported_feature", provider = provider, feature = "config.tool_choice.mode"))
  req
}

# A server that ignores every tool_choice but auto (Z.AI): "none" and an
# allowlist have a client-side form; "required" cannot be forced (refused).
.forced_choice_prepare <- function(req, provider) {
  tc <- req$config$tool_choice
  names <- unlist(tc$allowed, use.names = FALSE)
  if (tc$mode == "required")
    stop(lm15_error(paste0(provider, ": tool_choice mode='required' is silently ignored by this server (only 'auto' is honoured) and a forced call cannot be reproduced client-side"), code = "unsupported_feature", provider = provider, feature = "config.tool_choice.mode"))
  if (tc$mode == "none") {
    .adapt("config.tool_choice.mode", "client_side", "this server ignores tool_choice='none'; no tools were sent, which is the same outcome", asked = "none", applied = "no tools sent")
    req$tools <- list()
  } else {
    kept <- Filter(function(t) t$type == "function" && t$name %in% names, req$tools)
    .adapt("config.tool_choice.allowed", "client_side", "this server ignores tool_choice allowlists; only the allowed tools were sent, which is what the allowlist means", asked = as.list(names), applied = lapply(kept, function(t) t$name))
    req$tools <- kept
  }
  tc$mode <- "auto"; tc$allowed <- list(); req$config$tool_choice <- tc
  req
}

build_request <- function(lm, request, ..., stream = FALSE) {
  .check_dots(...)
  pair <- .route(lm, request); lm <- pair$lm; req <- pair$request
  .require_surface(lm, if (stream) "stream" else "complete")
  built <- .collecting(lm$adaptations %||% "note", lm$definition$id, function() .build_payload(lm, req, stream))
  body <- built$value
  d <- lm$definition$dialect
  endpoint <- switch(d, "openai-chat" = "/chat/completions", "openai-responses" = "/responses", anthropic = "/messages", gemini = paste0("/", .path_id(if (startsWith(req$model, "models/")) req$model else paste0("models/", req$model), TRUE), if (stream) ":streamGenerateContent" else ":generateContent"))
  wire <- .emit(lm, "POST", endpoint, body, params = if (d == "gemini" && stream) list(alt = "sse") else list(), model = req$model, stream = stream)
  wire$adaptations <- built$records
  wire
}

.build_payload <- function(lm, req, streaming = FALSE) {
  dialect <- switch(lm$definition$dialect, "openai-chat" = "chat", "openai-responses" = "responses", lm$definition$dialect)
  provider <- lm$definition$id; c <- req$config; compat <- .compat(lm, req)
  if (dialect == "anthropic" && !is.null(compat$model_prefixes) && !any(vapply(compat$model_prefixes, function(p) startsWith(req$model, p), logical(1)))) .abort("This model would be silently substituted by this endpoint.", "unsupported_model", provider)
  if (provider == "xai") { req <- .xai_prepare(req, provider); c <- req$config }
  if (dialect == "chat" && compat$forced_tool_choice == "reject" && !is.null(c$tool_choice) && (c$tool_choice$mode != "auto" || length(c$tool_choice$allowed))) {
    req <- .forced_choice_prepare(req, provider); c <- req$config
  }
  # Anthropic cannot restrict to a subset of the declared tools: only the allowed ones are sent.
  if (dialect == "anthropic" && !is.null(c$tool_choice) && c$tool_choice$mode != "none" && length(c$tool_choice$allowed)) {
    names <- unlist(c$tool_choice$allowed, use.names = FALSE)
    declared <- vapply(req$tools, function(t) t$name, "")
    if (!(length(names) == 1L && c$tool_choice$mode == "required") && !setequal(names, declared)) {
      kept <- Filter(function(t) t$name %in% names, req$tools)
      .adapt("config.tool_choice.allowed", "client_side", "the Messages API cannot restrict to a subset of the declared tools; only the allowed tools were sent, which is what the allowlist means", asked = as.list(names), applied = lapply(kept, function(t) t$name))
      req$tools <- kept
    }
  }
  cache <- c$cache; cache_on <- !is.null(cache) && cache$mode != "off"
  resource <- if (cache_on) cache$resource else NULL
  if (!is.null(resource) && dialect != "gemini") .unsupported(provider, "stored cache resource")
  cache_wire <- compat$cache_control %||% "none"
  if (dialect == "anthropic" && !is.null(cache)) {
    if (!is.null(cache$key)) .adapt("config.cache.key", "dropped", "the Messages API has no cache affinity key (OpenAI's prompt_cache_key); marks on blocks are its mechanism", asked = cache$key)
    if (identical(cache$retention, "long") && cache_wire != "anthropic") .adapt("config.cache.retention", "dropped", "this server caches implicitly and has no cache-control TTL", asked = "long")
  }
  if (dialect == "gemini" && cache_on) {
    if (!is.null(cache$key)) .adapt("config.cache.key", "dropped", "GenerateContent has no cache affinity key; implicit caching applies, and a stored cache (lm.cache(prefix), cache.resource) is the explicit tier", asked = cache$key)
    if (!is.null(cache$retention) && cache$retention != "short") .adapt("config.cache.retention", "dropped", "GenerateContent takes no lifetime in-request; it belongs to the stored cache (cache_create(..., ttl_seconds=...) / cache_update)", asked = cache$retention)
  }
  if (dialect %in% c("chat", "responses") && !is.null(cache) && !cache_wire %in% c("openai", "openai_implicit")) {
    if (!is.null(cache$key)) .adapt("config.cache.key", "dropped", "this server has no cache affinity field; implicit caching still applies", asked = cache$key)
    if (identical(cache$retention, "long")) .adapt("config.cache.retention", "dropped", "this server has no in-request cache lifetime knob; implicit caching still applies", asked = "long")
  }
  mark_at <- if (cache_on && !is.null(cache$prefix_until_index)) min(cache$prefix_until_index, length(req$messages) - 1L) else NULL
  if (!is.null(mark_at) && dialect %in% c("chat", "responses") && cache_wire == "openai") {
    # The wire marks a text block of a user/developer message only: a mark asked
    # elsewhere walks back to the nearest eligible message, else it is dropped.
    asked <- mark_at; mark_at <- NULL
    for (index in seq(asked, 0L)) {
      m <- req$messages[[index + 1L]]
      if (m$role %in% c("assistant", "tool") || !length(m$parts) || m$parts[[length(m$parts)]]$type != "text") next
      if (index != asked) .adapt("config.cache.prefix_until_index", "substituted", paste0("message ", asked, " is a ", req$messages[[asked + 1L]]$role, " message or does not end with text; the Responses wire marks text blocks of user/developer messages only, so the mark moved to the nearest eligible message before it"), asked = asked, applied = index)
      mark_at <- index
      break
    }
    if (is.null(mark_at)) .adapt("config.cache.prefix_until_index", "dropped", paste0("no user/developer message ending with text at or before message ", asked, "; the Responses wire marks text blocks only (implicit caching still applies)"), asked = asked)
  }
  if (cache_on && dialect == "anthropic" && identical(cache$prefix, "history")) mark_at <- length(req$messages) - 1L
  stable <- cache_on && identical(cache$prefix, "stable")
  marker <- json_object(type = "ephemeral")
  if (cache_on && identical(cache$retention, "long")) marker$ttl <- "1h"
  if (!cache_wire %in% c("anthropic", "openai")) { mark_at <- NULL; stable <- FALSE }
  system <- if (is.character(req$system)) req$system else if (!is.null(req$system)) .parts_text(req$system, provider) else NULL
  rows <- list(); append_row <- function(row) rows[[length(rows) + 1L]] <<- row
  if (!is.null(system) && dialect == "chat") append_row(json_object(role = compat$instruction_role, content = if (stable) list(json_object(type = "text", text = system, prompt_cache_breakpoint = json_object(mode = "explicit"))) else system))
  if (!is.null(system) && dialect == "responses" && stable) append_row(json_object(role = compat$developer_role, content = list(json_object(type = "input_text", text = system, prompt_cache_breakpoint = json_object(mode = "explicit")))))
  call_names <- list()
  for (i in seq_along(req$messages)) {
    msg <- req$messages[[i]]
    # Name lookup uses only preceding calls, never a future transcript row.
    at_mark <- !is.null(mark_at) && i - 1L == mark_at
    if (!is.null(resource) && !is.null(cache$prefix_until_index) && i - 1L <= min(cache$prefix_until_index, length(req$messages) - 1L)) {
      for (p in msg$parts) if (p$type == "tool_call") call_names[[p$id]] <- p$name
      next
    }
    if (msg$role == "tool" && dialect %in% c("chat", "responses")) {
      for (p in msg$parts) {
        content <- .native_block(p, dialect, provider, compat)
        row <- if (dialect == "chat") json_object(role = "tool", tool_call_id = p$id, content = content) else json_object(type = "function_call_output", call_id = p$id, output = content)
        if (compat$tool_result_name == "include" && !is.null(p$name)) row$name <- p$name
        append_row(row)
      }
      next
    }
    if (msg$role == "assistant" && dialect %in% c("chat", "responses")) {
      words <- list(); calls <- list(); reasoning <- character()
      for (p in msg$parts) {
        if (p$type %in% .media_kinds || p$type == "citation") .unsupported(provider, paste(p$type, "in assistant replay"))
        if (p$type == "tool_call") {
          calls[[length(calls) + 1L]] <- if (dialect == "chat") json_object(id = p$id, type = "function", "function" = json_object(name = p$name, arguments = .json_encode(p$input))) else json_object(type = "function_call", call_id = p$id, name = p$name, arguments = .json_encode(p$input))
          call_names[[p$id]] <- p$name; next
        }
        if (p$type == "thinking") {
          state <- continuation_data(p, "openai", "reasoning_item")
          if (dialect == "responses" && !is.null(state) && length(state)) {
            row <- json_object(type = "reasoning", summary = if (nzchar(p$text)) list(json_object(type = "summary_text", text = p$text)) else list())
            for (name in intersect(names(state), c("id", "encrypted_content"))) row[name] <- list(state[[name]])
            append_row(row); next
          }
          if (dialect == "chat" && compat$thinking_replay == "native") { reasoning <- c(reasoning, p$text); next }
          if (dialect == "chat" && compat$thinking_replay == "omit") .unsupported(provider, "dropping assistant thinking replay")
          if (!nzchar(p$text)) next
        }
        words[[length(words) + 1L]] <- if (dialect == "chat") p$text else if (p$type == "refusal") json_object(type = "refusal", refusal = p$text) else json_object(type = "output_text", text = p$text)
      }
      if (dialect == "chat") {
        row <- json_object(role = "assistant", content = if (length(words)) paste(unlist(words), collapse = "\n") else NULL)
        if (length(calls)) row$tool_calls <- calls
        if (length(reasoning) || compat$assistant_reasoning_content == "include_empty") row$reasoning_content <- paste(reasoning, collapse = "\n")
        append_row(row)
      } else {
        if (length(words)) { row <- json_object(role = "assistant", content = words); if (compat$commentary_phase == "tag" && length(calls)) row$phase <- "commentary"; append_row(row) }
        for (row in calls) append_row(row)
      }
      next
    }
    blocks <- lapply(msg$parts, .native_block, dialect = dialect, provider = provider, compat = compat, call_names = call_names)
    if (msg$role == "developer" && dialect %in% c("anthropic", "gemini")) blocks <- list(if (dialect == "anthropic") json_object(type = "text", text = paste0("[developer]\n", .parts_text(msg$parts, provider))) else json_object(text = paste0("[developer]\n", .parts_text(msg$parts, provider))))
    if (at_mark && length(blocks)) {
      last <- length(blocks)
      if (dialect == "anthropic") blocks[[last]]$cache_control <- marker
      else blocks[[last]]$prompt_cache_breakpoint <- json_object(mode = "explicit")
    }
    role <- switch(dialect, gemini = if (msg$role == "assistant") "model" else "user", anthropic = if (msg$role == "assistant") "assistant" else "user", chat = if (msg$role == "developer") compat$instruction_role else msg$role, responses = if (msg$role == "developer") compat$developer_role else msg$role)
    if (dialect == "chat" && length(blocks) == 1L && blocks[[1L]]$type == "text" && !at_mark) blocks <- blocks[[1L]]$text
    append_row(if (dialect == "gemini") json_object(role = role, parts = blocks) else json_object(role = role, content = blocks))
    for (p in msg$parts) if (p$type == "tool_call") call_names[[p$id]] <- p$name
  }
  if (dialect == "gemini" && !length(rows)) stop("Stored-cache request requires a suffix message.", call. = FALSE)
  visible_max_tokens <- NULL
  if (dialect == "anthropic") {
    visible_max_tokens <- c$max_tokens
    if (is.null(visible_max_tokens)) {
      visible_max_tokens <- .anthropic_default_max_tokens(req$model)
      .adapt("config.max_tokens", "defaulted", "the Messages API requires max_tokens and none was set; the class default was used", applied = visible_max_tokens)
    }
  }
  payload <- switch(dialect, chat = json_object(model = req$model, messages = rows), responses = json_object(model = req$model, input = rows, stream = streaming), anthropic = json_object(model = req$model, messages = rows, stream = streaming, max_tokens = visible_max_tokens), gemini = json_object(contents = rows))
  if (dialect == "chat" && streaming) { payload$stream <- TRUE; if (compat$stream_usage == "include") payload$stream_options <- json_object(include_usage = TRUE) }
  if (!is.null(system)) {
    if (dialect == "responses" && !stable) payload$instructions <- system
    if (dialect == "anthropic") payload$system <- if (cache_on && cache_wire == "anthropic") list(json_object(type = "text", text = system, cache_control = marker)) else system
    if (dialect == "gemini" && is.null(resource)) payload$systemInstruction <- json_object(parts = list(json_object(text = system)))
  }
  if (!is.null(resource)) payload$cachedContent <- if (startsWith(resource, "cachedContents/")) resource else paste0("cachedContents/", resource)
  fields <- switch(dialect,
    chat = c(max_tokens = compat$max_tokens_field, temperature = "temperature", top_p = "top_p", seed = "seed", frequency_penalty = "frequency_penalty", presence_penalty = "presence_penalty", stop = "stop", service_tier = "service_tier", user_id = compat$user_field, store = "store"),
    responses = c(max_tokens = compat$max_output_tokens_field, temperature = "temperature", top_p = "top_p", service_tier = "service_tier", user_id = "safety_identifier", store = "store"),
    anthropic = c(temperature = "temperature", top_p = "top_p", top_k = "top_k", stop = "stop_sequences", service_tier = "service_tier"),
    gemini = c(temperature = "temperature", max_tokens = "maxOutputTokens", top_p = "topP", top_k = "topK", seed = "seed", frequency_penalty = "frequencyPenalty", presence_penalty = "presencePenalty", stop = "stopSequences"))
  if (dialect == "gemini") payload$generationConfig <- json_object()
  if (!is.null(c$top_k) && dialect %in% c("chat", "responses"))
    .adapt("config.top_k", "dropped", if (dialect == "chat") "the Chat Completions wire has no top_k (Anthropic and Gemini carry it; servers that accept it take it through extensions)" else "the Responses wire has no top_k (Anthropic and Gemini carry it)", asked = c$top_k)
  if (dialect == "responses" && length(c$stop))
    .adapt("config.stop", "client_side", "the Responses wire has no stop field; the reply is streamed and the connection closed at the first stop sequence (whether the provider then stops generating, and billing, is its own behaviour); the usage report rides only the final frame, so it is not reported when the cut happens (never estimated)", asked = as.list(unlist(c$stop)), applied = as.list(unlist(c$stop)))
  for (name in c("seed", "frequency_penalty", "presence_penalty")) if (!is.null(c[[name]]) && dialect %in% c("responses", "anthropic"))
    .adapt(paste0("config.", name), "dropped", if (dialect == "responses") paste0("the Responses wire has no ", name, " field (the Chat Completions dialect carries it)") else paste0("the Messages API has no ", name, " field"), asked = c[[name]])
  if (dialect == "anthropic") {
    if (compat$sampling_params == "reject") {
      for (name in c("temperature", "top_p", "top_k")) if (!is.null(c[[name]])) {
        .adapt(paste0("config.", name), "dropped", "this server ignores sampling parameters (the model's sampling is fixed)", asked = c[[name]])
        fields <- fields[names(fields) != name]
      }
    } else if (!is.null(c$temperature) && c$temperature > 1) {
      .adapt("config.temperature", "clamped", "the Messages API accepts temperature in [0, 1]; the canonical range is [0, 2]", asked = c$temperature, applied = .json_number("1.0"))
      c$temperature <- .json_number("1.0")
    }
    if (isFALSE(c$store)) .adapt("config.store", "satisfied", "the Messages API has no stored-response object to opt out of; nothing retrievable is kept", asked = FALSE)
    if (isTRUE(c$store)) .adapt("config.store", "dropped", "the Messages API has no stored-response object to opt into (OpenAI and Gemini carry `store`)", asked = TRUE)
    if (!is.null(c$logprobs)) .adapt("config.logprobs", "dropped", "the Messages API does not expose token log probabilities (OpenAI and Gemini carry them); Response.logprobs will be absent", asked = c$logprobs)
  }
  for (key in names(fields)) {
    v <- c[[key]]
    if (.empty(v)) next
    if (dialect == "gemini") {
      if (is.double(v) && length(v) == 1L && v == trunc(v)) v <- .json_number(sprintf("%.0f", v))
      payload$generationConfig[[fields[[key]]]] <- v
    } else payload[[fields[[key]]]] <- v
  }
  if (dialect == "gemini") {
    if (!is.null(c$user_id)) .adapt("config.user_id", "dropped", "GenerateContent has no end-user attribution field (OpenAI and Anthropic carry it)", asked = c$user_id)
    if (!is.null(c$store)) payload$store <- c$store
    if (!is.null(c$service_tier)) payload$serviceTier <- c$service_tier
  }
  if (dialect == "anthropic" && !is.null(c$user_id)) payload$metadata <- json_object(user_id = c$user_id)
  if (length(req$tools) && is.null(resource)) {
    wire <- lapply(req$tools, .tool_wire, dialect = dialect, compat = compat, provider = provider)
    if (dialect == "gemini") {
      fn <- vapply(req$tools, function(t) t$type == "function", logical(1))
      wire <- c(if (any(fn)) list(json_object(functionDeclarations = wire[fn])) else list(), wire[!fn])
    }
    payload$tools <- wire
  }
  tc <- .tool_choice_wire(req, dialect, compat, provider)
  if (!is.null(tc)) {
    if (!is.null(resource)) .unsupported(provider, "per-request tool choice alongside a stored cache")
    payload[[if (dialect == "gemini") "toolConfig" else "tool_choice"]] <- tc
  }
  if (dialect %in% c("chat", "responses") && !is.null(c$tool_choice$parallel)) payload$parallel_tool_calls <- c$tool_choice$parallel
  payload <- .reasoning_wire(req, dialect, compat, provider, payload, visible_max_tokens)
  f <- c$response_format
  if (!is.null(f)) {
    schema <- identical(f$type, "json_schema")
    if (dialect == "anthropic") {
      if (compat$structured_output == "reject")
        .adapt("config.response_format", "dropped", "this server accepts output_config.format and does not apply it; describe the shape in the prompt", asked = f)
      else {
        if (!schema) stop(lm15_error("anthropic: response_format json_object is not supported \u2014 the Messages API has no any-JSON mode; give a json_schema (objects need additionalProperties: false)", code = "unsupported_feature", provider = provider, feature = "config.response_format"))
        payload$output_config <- payload$output_config %||% json_object()
        payload$output_config$format <- json_object(type = "json_schema", schema = f$schema)
      }
    } else if (dialect == "gemini") {
      payload$generationConfig$responseMimeType <- "application/json"
      contains <- function(v) (.is_object(v) && "additionalProperties" %in% names(v)) || (is.list(v) && any(vapply(v, contains, logical(1))))
      if (schema) payload$generationConfig[[if (contains(f$schema)) "responseJsonSchema" else "responseSchema"]] <- f$schema
    } else {
      if (dialect == "chat" && schema && compat$json_schema == "reject") {
        .adapt("config.response_format", "dropped", paste0("this server accepts response_format type '", f$type, "' and does not apply it; use {'type': 'json_object'} and describe the shape in the prompt"), asked = f)
        f <- NULL
      }
    }
    if (!is.null(f) && !dialect %in% c("anthropic", "gemini")) {
      inner <- if (schema) json_object(name = f$name %||% "response", schema = f$schema) else json_object(type = "json_object")
      if (schema && !is.null(f$strict)) inner$strict <- f$strict
      if (dialect == "responses") { if (schema) inner$type <- "json_schema"; payload$text <- json_object(format = inner) }
      else payload$response_format <- if (schema) json_object(type = "json_schema", json_schema = inner) else inner
    }
  }
  if (!is.null(c$logprobs)) {
    if (dialect == "responses") { payload$top_logprobs <- c$logprobs; payload$include <- list("message.output_text.logprobs") }
    if (dialect == "chat") { payload$logprobs <- TRUE; if (c$logprobs > 0) payload$top_logprobs <- c$logprobs }
    if (dialect == "gemini") { payload$generationConfig$responseLogprobs <- TRUE; if (c$logprobs > 0) payload$generationConfig$logprobs <- c$logprobs }
  }
  if (!is.null(cache) && dialect %in% c("chat", "responses") && cache_wire %in% c("openai", "openai_implicit")) {
    if (cache_on) {
      if (!is.null(cache$key)) payload$prompt_cache_key <- cache$key
      if (identical(cache$retention, "long")) payload$prompt_cache_retention <- "24h"
    }
    if (cache_wire == "openai" && .cache_options_class(req$model) && (!cache_on || !is.null(mark_at) || (stable && !is.null(system)))) payload$prompt_cache_options <- json_object(mode = "explicit")
  }
  if (!is.null(compat$routing)) payload$provider <- compat$routing
  ext <- c$extensions %||% json_object()
  reserved <- switch(dialect, chat = c("prompt_caching", "cache", "compat", "openai_compat", "openai_chat_compat"), responses = c("prompt_caching", "cache", "compat", "openai_compat", "openai_responses_compat"), gemini = c("prompt_caching", "output"), "prompt_caching")
  if (dialect == "gemini" && !is.null(ext$output) && ext$output %in% c("image", "audio")) payload$generationConfig$responseModalities <- list(toupper(ext$output))
  if (dialect == "gemini" && !length(payload$generationConfig)) payload$generationConfig <- NULL
  passthrough <- setdiff(names(ext), reserved)
  payload[passthrough] <- ext[passthrough]
  if (!is.null(lm$definition$access$system_prefix)) {
    prefix <- lm$definition$access$system_prefix
    if (dialect == "anthropic") payload$system <- c(list(json_object(type = "text", text = prefix)), if (is.character(payload$system)) list(json_object(type = "text", text = payload$system)) else payload$system %||% list())
    if (dialect == "responses") payload$instructions <- payload$instructions %||% prefix
  }
  if (lm$definition$access$backend == "chatgpt-codex") {
    # Reject impossible explicit intent, rather than silently strip the cap.
    if (!is.null(c$max_tokens) || isTRUE(c$store)) .unsupported(provider, "max_tokens or store = TRUE on Codex")
    payload$store <- FALSE; payload$stream <- TRUE
  }
  payload
}
