# R's streaming surface is callback-based: the HTTP read remains in this
# process and callbacks run on R's thread, never on a background R thread.
# INV-056: no size limit by default. A provider sends whole objects as one
# line (OpenAI Responses repeats the full response, system prompt included,
# in response.completed; Gemini sends a 4K image as one 29.7 MB line), a
# non-streamed reply has no limit either, and a stream is accumulated into
# the whole reply anyway. The limits here are opt-in caps. Splitting is
# linear: each chunk is searched once, and an unfinished line keeps its
# pieces until its newline arrives, then is joined once.
.new_sse <- function(on_frame, max_line_bytes = Inf, max_event_bytes = Inf) {
  pieces <- list(); pending <- 0; data <- character(); event <- NULL; event_bytes <- 0
  line <- function(bytes) {
    if (length(bytes) > max_line_bytes) .abort("SSE line exceeds the configured limit.", "transport")
    event_bytes <<- event_bytes + length(bytes)
    if (event_bytes > max_event_bytes) .abort("SSE event exceeds the configured limit.", "transport")
    n <- length(bytes)
    if (n && bytes[[n]] == as.raw(13L)) bytes <- bytes[-n]
    if (!length(bytes)) {
      if (length(data)) on_frame(paste(data, collapse = "\n"), event)
      data <<- character(); event <<- NULL; event_bytes <<- 0; return(invisible(NULL))
    }
    value <- rawToChar(bytes)
    if (startsWith(value, "data:")) data <<- c(data, sub("^[[:space:]]+", "", substring(value, 6L)))
    else if (startsWith(value, "event:")) event <<- trimws(substring(value, 7L))
  }
  take <- function(tail) {
    if (!length(pieces)) return(tail)
    out <- do.call(c, c(pieces, list(tail)))
    pieces <<- list(); pending <<- 0
    out
  }
  feed <- function(chunk) {
    if (!length(chunk)) return(invisible(NULL))
    start <- 1L
    for (end in which(chunk == as.raw(10L))) {
      line(take(if (end > start) chunk[start:(end - 1L)] else raw()))
      start <- end + 1L
    }
    if (start <= length(chunk)) {
      pieces[[length(pieces) + 1L]] <<- chunk[start:length(chunk)]
      pending <<- pending + (length(chunk) - start + 1L)
      if (pending > max_line_bytes) .abort("SSE line exceeds the configured limit.", "transport")
    }
    invisible(NULL)
  }
  finish <- function() {
    if (pending > 0) line(take(raw()))
    if (length(data)) on_frame(paste(data, collapse = "\n"), event)
    pieces <<- list(); pending <<- 0; data <<- character(); invisible(NULL)
  }
  list(feed = feed, finish = finish)
}

.parse_stream_frame <- function(lm, req, data, event = NULL) {
  if (!nzchar(data)) return(list())
  if (data == "[DONE]") return(list(stream_end_event()))
  p <- .json_decode(data)
  if (!.is_object(p)) return(list())
  dialect <- switch(lm$definition$dialect, "openai-chat" = "chat", "openai-responses" = "responses", lm$definition$dialect)
  result <- list(); add <- function(e) result[[length(result) + 1L]] <<- e
  delta <- function(d) add(stream_delta_event(d))
  index <- function(x) .wire_int(x %||% 0L)
  if (!is.null(p$error) || identical(p$type, "error") || identical(p$type, "response.error")) {
    error <- normalize_error(lm, 500L, p)
    return(list(stream_error_event(error_detail(error$code, message = error$message, provider_code = error$provider_code))))
  }
  if (dialect == "chat") {
    choices <- .wire_array(p$choices)
    if (length(choices) > 1L) .unsupported(lm$definition$id, "multiple streamed choices")
    chosen <- if (length(choices)) .wire_object(choices[[1L]]) else json_object()
    d <- .wire_object(chosen$delta)
    thinking <- d$reasoning_content %||% d$reasoning
    if (!is.null(thinking) && nzchar(.wire_string(thinking))) delta(thinking_delta(.wire_string(thinking)))
    if (is.character(d$content) && nzchar(d$content)) delta(text_delta(d$content, logprobs = .logprobs_wire(chosen$logprobs$content)))
    for (tc in .wire_array(d$tool_calls)) {
      if (!.is_object(tc)) next
      fn <- .wire_object(tc[["function"]])
      delta(tool_call_delta(.wire_string(fn$arguments), part_index = index(tc$index), id = tc$id, name = fn$name))
    }
    reasoning_usage <- .compat(lm, req)$reasoning_usage %||% "in_completion"
    if (!is.null(chosen$finish_reason)) add(stream_end_event(finish_reason = .finish_wire(chosen$finish_reason, "chat"), usage = if (.is_object(p$usage)) .usage_wire(p$usage, "chat", reasoning_usage) else NULL, provider_data = p))
    else if (.is_object(p$usage)) add(stream_end_event(usage = .usage_wire(p$usage, "chat", reasoning_usage), provider_data = p))
  }
  if (dialect == "responses") {
    type <- p$type %||% ""
    i <- index(p$output_index)
    if (type == "response.created") add(stream_start_event(id = p$response$id, model = p$response$model %||% req$model))
    if (type %in% c("response.output_text.delta", "response.refusal.delta")) delta(text_delta(.wire_string(p$delta), part_index = i, logprobs = .logprobs_wire(p$logprobs)))
    if (type %in% c("response.reasoning_summary_text.delta", "response.reasoning_text.delta")) delta(thinking_delta(.wire_string(p$delta), part_index = i))
    if (type == "response.output_text.annotation.added" && .is_object(p$annotation)) {
      cited <- .citation_wire(p$annotation)
      if (!is.null(cited)) delta(citation_delta(text = cited$text, url = cited$url, title = cited$title, part_index = i))
    }
    if (type == "response.output_audio.delta") delta(audio_delta(data = .wire_string(p$delta), part_index = i, media_type = "audio/wav"))
    if (type %in% c("response.output_image.delta", "response.image.delta")) delta(image_delta(data = .wire_string(p$delta), part_index = i, media_type = "image/png"))
    item <- .wire_object(p$item)
    if (type %in% c("response.output_item.added", "response.output_item.done") && identical(item$type, "reasoning")) {
      if (type == "response.output_item.added") delta(thinking_delta("", part_index = i)) else {
        state <- json_object()
        for (key in c("id", "encrypted_content")) if (!is.null(item[[key]]) && nzchar(.wire_string(item[[key]]))) state[[key]] <- item[[key]]
        if (length(state)) delta(continuation_delta("openai", "reasoning_item", data = state, part_index = i))
      }
    }
    if (type == "response.output_item.added" && identical(item$type, "function_call")) delta(tool_call_delta(.wire_string(item$arguments), part_index = i, id = item$call_id %||% item$id, name = item$name))
    if (type == "response.function_call_arguments.delta") delta(tool_call_delta(.wire_string(p$delta), part_index = i, id = p$call_id %||% p$id, name = p$name))
    if (type %in% c("response.completed", "response.incomplete", "response.failed")) {
      response <- .wire_object(p$response)
      if (!is.null(response$error)) stop(normalize_error(lm, 500L, response))
      has_tool <- any(vapply(.wire_array(response$output), function(t) .is_object(t) && identical(t$type, "function_call"), logical(1)))
      # The finish reason reads status and incomplete_details as the complete
      # path does (INV-051): length for the output limit, content_filter.
      why <- .wire_string(response$incomplete_details$reason)
      reason <- if (has_tool) "tool_call" else if (identical(response$status, "incomplete") && grepl("token", why)) "length" else if (grepl("content_filter|safety", why)) "content_filter" else "stop"
      add(stream_end_event(finish_reason = reason, usage = .usage_wire(response$usage, "responses"), provider_data = response))
    }
  }
  if (dialect == "anthropic") {
    type <- p$type %||% ""; i <- index(p$index)
    if (type == "message_start") add(stream_start_event(id = p$message$id, model = p$message$model %||% req$model))
    if (type == "content_block_start") {
      b <- .wire_object(p$content_block)
      if (identical(b$type, "tool_use")) delta(tool_call_delta(if (.is_object(b$input) && length(b$input)) .json_encode(b$input) else "", part_index = i, id = b$id, name = b$name))
      if (identical(b$type, "redacted_thinking") && !is.null(b$data)) {
        delta(thinking_delta("", part_index = i)); delta(continuation_delta("anthropic", "redacted_thinking", data = json_object(data = b$data), part_index = i))
      }
    }
    if (type == "content_block_delta") {
      d <- .wire_object(p$delta)
      if (identical(d$type, "text_delta")) delta(text_delta(.wire_string(d$text), part_index = i))
      if (identical(d$type, "thinking_delta")) delta(thinking_delta(.wire_string(d$thinking), part_index = i))
      if (identical(d$type, "input_json_delta")) delta(tool_call_delta(.wire_string(d$partial_json), part_index = i))
      if (identical(d$type, "signature_delta") && !is.null(d$signature)) delta(continuation_delta("anthropic", "thinking_signature", data = json_object(signature = d$signature), part_index = i))
      if (!is.null(d$type) && d$type %in% c("citation_delta", "citations_delta")) {
        cited <- .citation_wire(.wire_object(d$citation %||% d))
        if (!is.null(cited)) delta(citation_delta(text = cited$text, url = cited$url, title = cited$title, part_index = i))
      }
    }
    if (type == "message_delta") add(stream_end_event(finish_reason = if (!is.null(p$delta$stop_reason)) .finish_wire(p$delta$stop_reason, "anthropic") else NULL, usage = if (.is_object(p$usage) && length(p$usage)) .usage_wire(p$usage, "anthropic") else NULL, provider_data = p))
    if (type == "message_stop") add(stream_end_event())
  }
  if (dialect == "gemini") {
    blocked <- .gemini_inband_error(p, lm$definition$id)
    if (!is.null(blocked)) return(list(stream_error_event(error_detail(blocked$code, message = blocked$message, provider_code = "inband_finish_reason"))))
    candidates <- .wire_array(p$candidates); chosen <- if (length(candidates)) .wire_object(candidates[[1L]]) else json_object()
    logs <- .logprobs_wire(.wire_object(chosen$logprobsResult), TRUE); tool <- FALSE
    for (i in seq_along(chosen$content$parts %||% list())) {
      b <- chosen$content$parts[[i]]; if (!.is_object(b)) next
      if ("text" %in% names(b)) {
        if (isTRUE(b$thought)) delta(thinking_delta(.wire_string(b$text), part_index = i - 1L))
        else { delta(text_delta(.wire_string(b$text), part_index = i - 1L, logprobs = logs)); logs <- list() }
      } else if (.is_object(b$functionCall)) {
        fn <- b$functionCall; tool <- TRUE
        delta(tool_call_delta(.json_encode(fn$args %||% json_object()), part_index = i - 1L, id = fn$id, name = fn$name))
      } else if (.is_object(b$inlineData)) {
        media <- b$inlineData; mime <- .wire_string(media$mimeType)
        if (startsWith(mime, "image/")) delta(image_delta(data = media$data, media_type = mime, part_index = i - 1L))
        else if (startsWith(mime, "audio/")) delta(audio_delta(data = media$data, media_type = mime, part_index = i - 1L))
        else .unsupported(lm$definition$id, "non-streamable media in provider stream")
      }
      for (s in .thought_state(b)) delta(continuation_delta(s$provider, s$kind, data = s$data, part_index = i - 1L))
    }
    if (!is.null(chosen$finishReason)) add(stream_end_event(finish_reason = .finish_wire(chosen$finishReason, "gemini", tool), usage = .usage_wire(p$usageMetadata, "gemini"), provider_data = p))
    else if (!length(result) && !is.null(p$usageMetadata)) add(stream_end_event(finish_reason = "stop", usage = .usage_wire(p$usageMetadata, "gemini"), provider_data = p))
  }
  result
}

# ─── Stream readers (MAP-20) ─────────────────────────────────────────
#
# .parse_stream_frame maps ONE frame on its own and holds no state. A reader
# reads one stream: it may hold what that stream needs and nothing across
# streams. Every path that turns frames into canonical events goes through
# .new_stream_reader (the stream, replay_stream and the vet shim).

.new_stream_reader <- function(lm, req) {
  dialect <- lm$definition$dialect
  if (identical(dialect, "openai-responses")) return(.responses_stream_reader(lm, req))
  if (identical(dialect, "gemini")) return(.gemini_stream_reader(lm, req))
  reader <- function(data, event = NULL) .parse_stream_frame(lm, req, data, event)
  # MAP-22: a server that returns the stop sequence it stopped at.
  if (.stop_returned_in_text(lm, req)) return(.stop_cutting_reader(reader, req$config$stop))
  reader
}
# MAP-22: text is cut at the first stop sequence; text after the cut is
# dropped; every other event (usage, the end) passes, because the server has
# already stopped.
.stop_cutting_reader <- function(inner, stop) {
  cutter <- .new_stop_cutter(stop)
  is_text <- function(e) identical(e$type, "delta") && identical(e$delta$type, "text")
  function(data, event = NULL) {
    out <- list()
    for (e in inner(data, event)) {
      if (cutter$is_cut()) { if (!is_text(e)) out[[length(out) + 1L]] <- e; next }
      if (identical(e$type, "end") || identical(e$type, "error")) { out <- c(out, cutter$flush(), list(e)); next }
      out <- c(out, cutter$feed(e))
    }
    out
  }
}

# What one stream's fragments delivered, per part slot and channel. A final
# copy is the provider restating a value it may also have sent in fragments;
# remainder() is what the fragments did not deliver: the whole value when none
# came, the rest when they are its beginning, nothing when they are all of it.
# Fragments that are not its beginning contradict it and the stream is
# refused: lm15 does not choose.
.final_copies <- function() {
  seen <- list()
  key <- function(index, channel) paste0(sprintf("%.0f", index), ":", channel)
  saw <- function(index, channel, text) {
    if (nzchar(text)) { k <- key(index, channel); seen[[k]] <<- paste0(seen[[k]] %||% "", text) }
    invisible(NULL)
  }
  remainder <- function(index, channel, final, json_value = FALSE) {
    k <- key(index, channel); got <- seen[[k]] %||% ""
    if (startsWith(final, got)) { seen[[k]] <<- final; return(substring(final, nchar(got) + 1L)) }
    # The same JSON value, members in the same order: the text differs
    # (whitespace, escapes), the arguments do not.
    if (json_value && isTRUE(tryCatch(identical(.json_encode(.json_decode(got)), .json_encode(.json_decode(final))), error = function(e) FALSE))) return("")
    .abort(sprintf("the streamed %s of part %.0f is not the beginning of the provider's final copy of it (%d characters streamed, %d in the final copy); lm15 does not choose between them (MAP-20)",
                   channel, index, nchar(got), nchar(final)), "stream_assembly", part_index = index)
  }
  list(saw = saw, remainder = remainder)
}

.arguments_text <- function(x) if (is.null(x)) "" else if (is.character(x)) x else .json_encode(x)

# The text a message item's fragments add up to: its output_text and refusal
# contents in order (both stream as text into the item's slot).
.message_final_text <- function(item) {
  out <- character()
  for (c in .wire_array(item$content)) {
    if (!.is_object(c)) next
    if (identical(c$type, "output_text") || identical(c$type, "text")) out <- c(out, .wire_string(c$text))
    else if (identical(c$type, "refusal")) out <- c(out, .wire_string(c$refusal %||% c$text))
  }
  paste(out, collapse = "")
}

# One OpenAI Responses stream (MAP-20). response.output_item.done is each
# item's final copy; at it the reader emits what the item's fragments did not
# deliver: a function call's arguments (the Codex backend sends parallel
# calls' arguments only there), a message's text, a reasoning item's summary.
# A summary's parts are joined with a newline, as the complete path joins them
# (INV-051); the newline goes before the first fragment of each later part.
.responses_stream_reader <- function(lm, req) {
  copies <- .final_copies(); summary_part <- list(); named <- character()
  slot <- function(i) sprintf("%.0f", i)
  complete <- function(i, item) {
    if (identical(item$type, "function_call")) {
      rest <- copies$remainder(i, "arguments", .arguments_text(item$arguments), json_value = TRUE)
      if (!nzchar(rest) && slot(i) %in% named) return(list())
      if (!is.null(item$name) && nzchar(.wire_string(item$name))) named <<- union(named, slot(i))
      return(list(stream_delta_event(tool_call_delta(rest, part_index = i, id = item$call_id %||% item$id, name = item$name))))
    }
    if (identical(item$type, "message")) {
      rest <- copies$remainder(i, "text", .message_final_text(item))
      return(if (nzchar(rest)) list(stream_delta_event(text_delta(rest, part_index = i))) else list())
    }
    if (identical(item$type, "reasoning") && .is_array(item$summary)) {
      final <- paste(vapply(item$summary, function(v) if (.is_object(v)) .wire_string(v$text) else .wire_string(v), ""), collapse = "\n")
      rest <- copies$remainder(i, "summary", final)
      return(if (nzchar(rest)) list(stream_delta_event(thinking_delta(rest, part_index = i))) else list())
    }
    list()
  }
  function(data, event = NULL) {
    events <- .parse_stream_frame(lm, req, data, event)
    if (!nzchar(data) || identical(data, "[DONE]")) return(events)
    p <- .json_decode(data)
    if (!.is_object(p)) return(events)
    type <- p$type %||% ""; i <- .wire_int(p$output_index %||% 0L)
    if (type == "response.reasoning_summary_text.delta") {
      part <- .wire_int(p$summary_index %||% 0L); at <- summary_part[[slot(i)]] %||% 0L
      prefix <- ""
      if (part > at) { summary_part[[slot(i)]] <<- part; prefix <- strrep("\n", part - at) }
      for (k in seq_along(events)) {
        e <- events[[k]]
        if (identical(e$type, "delta") && identical(e$delta$type, "thinking")) {
          text <- paste0(prefix, e$delta$text)
          events[[k]] <- stream_delta_event(thinking_delta(text, part_index = i))
          copies$saw(i, "summary", text)
        }
      }
    } else if (type %in% c("response.output_text.delta", "response.refusal.delta")) {
      copies$saw(i, "text", .wire_string(p$delta))
    } else if (type %in% c("response.output_item.added", "response.function_call_arguments.delta")) {
      for (e in events) if (identical(e$type, "delta") && identical(e$delta$type, "tool_call")) {
        copies$saw(i, "arguments", e$delta$input)
        if (!is.null(e$delta$name)) named <<- union(named, slot(i))
      }
    } else if (type == "response.output_item.done" && .is_object(p$item)) {
      events <- c(complete(i, p$item), events)
    }
    events
  }
}

# One Gemini stream: which slot each chunk's parts belong to. A chunk's parts
# are numbered from 0 in every chunk. The first part of a chunk continues the
# previous chunk's last part when both are text (or both thought text): that
# is how a long text arrives. Every other part is a new part with a slot of
# its own, in arrival order. A function call is never a continuation:
# gemini-2.5-pro sends each parallel call in a chunk of its own
# (cases/gemini/streaming_parallel_tool_calls_chunked.json).
.gemini_stream_reader <- function(lm, req) {
  last_slot <- 0L; last_kind <- ""; next_slot <- 0L
  kind_of <- function(b) {
    if (!.is_object(b)) return("")
    if ("text" %in% names(b)) return(if (isTRUE(b$thought)) "thought" else "text")
    if (.is_object(b$functionCall)) return("call")
    if (.is_object(b$inlineData)) return("media")
    ""
  }
  function(data, event = NULL) {
    events <- .parse_stream_frame(lm, req, data, event)
    if (!nzchar(data)) return(events)
    p <- tryCatch(.json_decode(data), error = function(e) NULL)
    if (!.is_object(p)) return(events)
    candidates <- .wire_array(p$candidates)
    parts <- if (length(candidates) && .is_object(candidates[[1L]])) .wire_array(.wire_object(candidates[[1L]]$content)$parts) else list()
    slots <- list()
    for (pos in seq_along(parts)) {
      kind <- kind_of(parts[[pos]])
      if (!nzchar(kind)) next
      if (pos == 1L && kind %in% c("text", "thought") && identical(last_kind, kind)) s <- last_slot
      else { s <- next_slot; next_slot <<- next_slot + 1L }
      slots[[as.character(pos - 1L)]] <- s
      last_slot <<- s; last_kind <<- kind
    }
    for (k in seq_along(events)) {
      e <- events[[k]]
      if (!identical(e$type, "delta") || is.null(e$delta$part_index)) next
      s <- slots[[sprintf("%.0f", e$delta$part_index)]]
      if (is.null(s) || s == e$delta$part_index) next
      d <- e$delta; d$part_index <- s
      events[[k]] <- stream_delta_event(d)
    }
    events
  }
}

.new_accumulator <- function(req) {
  slots <- list(); states <- list(); id <- NULL; model <- req$model; finish <- NULL; counts <- usage(); provider_data <- NULL; logs <- list(); adaptations <- list(); scores_complete <- TRUE
  push <- function(event) {
    if (event$type == "start") { id <<- event$id %||% id; model <<- event$model %||% model; if (length(event$adaptations)) adaptations <<- event$adaptations; return(invisible(NULL)) }
    if (event$type == "end") { finish <<- event$finish_reason %||% finish; counts <<- event$usage %||% counts; provider_data <<- event$provider_data %||% provider_data; return(invisible(NULL)) }
    if (event$type != "delta") return(invisible(NULL))
    d <- event$delta
    if (d$type == "continuation" && is.null(d$part_index)) { states[[length(states) + 1L]] <<- continuation_state(d$provider, d$kind, data = d$data); return(invisible(NULL)) }
    key <- sprintf("%.0f", d$part_index); s <- slots[[key]] %||% list()
    k <- d$type
    if (k %in% c("text", "thinking")) s[[k]] <- paste0(s[[k]] %||% "", d$text)
    if (k == "text" && length(d$logprobs)) logs <<- c(logs, d$logprobs)
    if (k == "text" && identical(d$logprobs_complete, FALSE)) scores_complete <<- FALSE
    if (k == "tool_call") {
      s$tool_input <- paste0(s$tool_input %||% "", d$input)
      s$tool_id <- d$id %||% s$tool_id; s$tool_name <- d$name %||% s$tool_name
    }
    if (k == "image") s$image <- d
    if (k == "audio") {
      # Decode independent chunks at materialization; padding between chunks
      # must not cause all later audio to disappear.
      s$audio <- c(s$audio %||% list(), list(d$data %||% "")); s$audio_type <- d$media_type %||% s$audio_type
      if (!is.null(d$url) || !is.null(d$file_id)) .unsupported("stream", "addressed audio delta assembly")
    }
    if (k == "citation") s$citations <- c(s$citations %||% list(), list(citation_part(text = d$text, title = d$title, url = d$url)))
    if (k == "continuation") s$continuation <- c(s$continuation %||% list(), list(continuation_state(d$provider, d$kind, data = d$data)))
    slots[[key]] <<- s; invisible(NULL)
  }
  materialize <- function() {
    parts <- list(); unnamed <- numeric()
    add <- function(p, state) { p$continuation <- state; parts[[length(parts) + 1L]] <<- p }
    for (key in names(slots)[order(as.numeric(names(slots)))]) {
      s <- slots[[key]]; state <- s$continuation %||% list(); before <- length(parts)
      if (!is.null(s$thinking)) add(thinking(s$thinking), state)
      if (!is.null(s$text)) add(text(s$text), state)
      if (!is.null(s$image) && any(!vapply(s$image[c("data", "url", "file_id")], is.null, logical(1)))) add(image_part(data = s$image$data, url = s$image$url, file_id = s$image$file_id, media_type = s$image$media_type %||% "image/png"), state)
      if (!is.null(s$audio)) {
        bytes <- lapply(s$audio, function(v) if (!nzchar(v)) raw() else jsonlite::base64_dec(paste0(v, strrep("=", (4L - nchar(v) %% 4L) %% 4L))))
        bytes <- if (length(bytes)) do.call(c, bytes) else raw()
        mime <- s$audio_type
        if (is.null(mime) || mime %in% c("audio/pcm", "audio/pcm16")) { bytes <- .pcm_wav(bytes); mime <- "audio/wav" }
        add(audio_part(data = .base64_encode(bytes), media_type = mime), state)
      }
      for (p in s$citations %||% list()) add(p, state)
      if (!is.null(s$tool_input)) {
        if (is.null(s$tool_name)) unnamed <- c(unnamed, as.numeric(key))
        else add(tool_call_part(s$tool_id %||% paste0("tool_call_", key), s$tool_name, input = .parse_input(s$tool_input)), state)
      }
      if (length(parts) == before && is.null(s$tool_input)) add(text(""), state)
    }
    if (!length(parts)) parts <- list(text(""))
    has_tool <- any(vapply(parts, function(p) p$type == "tool_call", logical(1)))
    reason <- finish %||% if (has_tool) "tool_call" else "stop"
    if (has_tool && reason == "stop") reason <- "tool_call"
    parts <- .replace_text_with_data(parts, request_judgments(req))  # MAP-14 section 3
    msg <- message_assistant(parts); msg$continuation <- states
    out <- response(model, msg, reason, id = id, usage = counts, logprobs = logs, logprobs_complete = scores_complete, provider_data = provider_data, adaptations = adaptations)
    if (length(unnamed)) .abort("Tool call arrived without a name; no tool name was invented.", "stream_assembly", partial = out, part_index = unnamed[[1L]])
    out
  }
  list(push = push, response = materialize)
}
.pcm_wav <- function(bytes) {
  little <- function(x, size) writeBin(as.integer(x), raw(), size = size, endian = "little")
  c(charToRaw("RIFF"), little(36 + length(bytes), 4L), charToRaw("WAVEfmt "), little(16L, 4L), little(1L, 2L), little(1L, 2L), little(24000L, 4L), little(48000L, 4L), little(2L, 2L), little(16L, 2L), charToRaw("data"), little(length(bytes), 4L), bytes)
}

.new_stream <- function(lm, req, on_event, adaptations = list(), stop = NULL, handshake = function() NULL) {
  acc <- .new_accumulator(req); started <- FALSE; ended <- FALSE; terminal <- NULL; rank <- -1L
  cutter <- .new_stop_cutter(stop); cut <- FALSE
  emit <- function(e) {
    acc$push(e)
    on_event(e)
  }
  # MAP-13: the adaptations are known before the first byte and ride the first event.
  start <- function(e = stream_start_event(model = req$model)) {
    started <<- TRUE
    if (length(adaptations)) e["adaptations"] <- list(adaptations)
    emit(e)
  }
  consume <- function(e) {
    if (e$type == "start") { if (!started) start(e); return(invisible(NULL)) }
    if (e$type == "error") {
      # The HTTP driver's handshake evidence rides the event (never status 200).
      block <- .handshake_block(handshake())
      if (!is.null(block) && is.null(e$error$http_response)) {
        detail <- unclass(e$error); detail$http_response <- block
        e <- stream_error_event(.new_value("ErrorDetail", detail))
      }
      for (x in cutter$flush()) emit(x); on_event(e)
      h <- e$error$http_response
      .abort(e$error$message, e$error$code, lm$definition$id, provider_code = e$error$provider_code, request_id = h$request_id, retry_after = h$retry_after, rate_limit_headers = h$rate_limit_headers)
    }
    if (e$type == "end") {
      for (x in cutter$flush()) emit(x)
      ended <<- TRUE
      fields <- if (is.null(terminal)) list() else unclass(terminal)
      if (!is.null(e$finish_reason)) fields$finish_reason <- e$finish_reason
      if (!is.null(e$usage)) fields$usage <- e$usage
      incoming <- if (!is.null(e$usage)) 2L else if (!is.null(e$finish_reason)) 1L else 0L
      if (!is.null(e$provider_data) && incoming >= rank) { fields$provider_data <- e$provider_data; rank <<- incoming }
      terminal <<- .new_value("StreamEndEvent", fields)
      return(invisible(NULL))
    }
    if (!started) start()
    if (!cutter$active) return(emit(e))
    for (x in cutter$feed(e)) emit(x)
    if (cutter$is_cut()) {
      # Close the source at the cut: no usage is reported (never estimated).
      cut <<- TRUE; ended <<- TRUE
      terminal <<- stream_end_event(finish_reason = "stop")
      .stream_cut()
    }
  }
  reader <- .new_stream_reader(lm, req)
  sse <- .new_sse(function(data, event) for (e in reader(data, event)) consume(e))
  finish <- function() {
    if (!cut) sse$finish()
    if (!ended) {
      partial <- tryCatch(acc$response(), StreamAssemblyError = function(e) e$partial)
      .abort("Stream ended without a final end event; this is not a completed answer.", "stream_assembly", partial = partial)
    }
    if (!started) start()
    emit(terminal)
    acc$response()
  }
  list(feed = sse$feed, finish = finish, push = consume)
}

.stream_impl <- function(lm, request, on_event, ...) {
  .check_dots(...)
  if (!is.function(on_event)) stop("on_event must be a function receiving each canonical event.", call. = FALSE)
  pair <- .route(lm, request); lm <- pair$lm; request <- pair$request
  if (.uses_live_completion(lm, request)) return(.stream_live_completion(lm, request, on_event))
  wire <- build_request(lm, request, stream = TRUE)
  stop_at <- if (.client_side_stop(wire$adaptations)) request$config$stop else NULL
  handshake <- NULL
  source <- .new_stream(lm, request, function(event) on_event(.redact_error_event(event, wire)), adaptations = .visible_adaptations(lm, wire$adaptations), stop = stop_at, handshake = function() handshake)
  args <- list(wire, on_chunk = source$feed)
  if ("on_response" %in% names(formals(lm$transport))) args$on_response <- function(status, headers) handshake <<- headers
  tryCatch({
    result <- tryCatch(do.call(lm$transport, args), lm15_stream_cut = function(e) NULL)
    if (!is.null(result) && result$status >= 300L) stop(normalize_error(lm, result$status, rawToChar(result$body), headers = result$headers))
    source$finish()
  }, LM15Error = function(e) stop(.redact_wire_condition(e, wire)))
}
replay_stream <- function(lm, request, body, ...) {
  .check_dots(...)
  events <- list()
  source <- .new_stream(lm, request, function(e) events[[length(events) + 1L]] <<- e)
  source$feed(if (is.raw(body)) body else charToRaw(enc2utf8(body)))
  list(events = { response <- source$finish(); events }, response = response)
}
materialize_response <- function(events, request, ...) {
  .check_dots(...)
  acc <- .new_accumulator(request); ended <- FALSE
  for (e in events) {
    e <- validate(e)
    if (ended) .abort("An event followed the end event.", "stream_assembly", partial = acc$response())
    if (e$type == "error") {
      h <- e$error$http_response
      .abort(e$error$message, e$error$code, provider_code = e$error$provider_code, request_id = h$request_id, retry_after = h$retry_after, rate_limit_headers = h$rate_limit_headers)
    }
    acc$push(e)
    if (e$type == "end") ended <- TRUE
  }
  if (!ended) .abort("Stream has no end event.", "stream_assembly", partial = tryCatch(acc$response(), StreamAssemblyError = function(e) e$partial))
  acc$response()
}
