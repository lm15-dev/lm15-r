.uses_live_completion <- function(lm, request) {
  if (lm$definition$access$backend == "chatgpt-codex" || !lm$definition$dialect %in% c("openai-responses", "gemini")) return(FALSE)
  mode <- tolower(request$config$extensions$transport %||% "")
  model <- tolower(request$model)
  mode %in% c("live", "websocket", "ws") || grepl("-live", model, fixed = TRUE) ||
    (lm$definition$dialect == "gemini" && endsWith(model, "live")) ||
    (lm$definition$dialect == "openai-responses" && grepl("realtime", model, fixed = TRUE))
}
.live_request_config <- function(request) {
  fields <- as_dict(request$config)
  if (length(setdiff(names(fields), "extensions"))) .unsupported("live", "ordinary chat settings on a realtime request; use explicit live session extensions")
  ext <- request$config$extensions %||% json_object()
  ext[c("transport", "prompt_caching", "output")] <- NULL
  live_config(request$model, system = request$system, tools = request$tools, extensions = if (length(ext)) ext else NULL)
}
.wav_pcm <- function(bytes) {
  if (length(bytes) < 12L || rawToChar(bytes[1:4]) != "RIFF" || rawToChar(bytes[9:12]) != "WAVE") stop("Expected a RIFF WAVE file.", call. = FALSE)
  number <- function(offset, size) {
    if (offset + size - 1 > length(bytes)) stop("Truncated WAVE header.", call. = FALSE)
    sum(as.numeric(bytes[seq.int(offset, length.out = size)]) * 256^(seq_len(size) - 1L))
  }
  offset <- 13L; format <- NULL; data <- NULL
  while (offset + 7L <= length(bytes)) {
    tag <- rawToChar(bytes[offset + 0:3]); size <- number(offset + 4L, 4L); start <- offset + 8L
    if (start + size - 1 > length(bytes)) stop("Truncated WAVE chunk.", call. = FALSE)
    if (tag == "fmt ") {
      if (size < 16L) stop("Truncated WAVE format.", call. = FALSE)
      format <- list(encoding = number(start, 2L), channels = number(start + 2L, 2L), rate = number(start + 4L, 4L), bits = number(start + 14L, 2L))
    }
    if (tag == "data") data <- if (size) bytes[seq.int(start, length.out = size)] else raw()
    offset <- start + size + size %% 2L
  }
  if (is.null(format) || is.null(data) || format$encoding != 1L || format$bits != 16L || format$channels != 1L || format$rate <= 0) .unsupported("live", "non-mono/non-PCM16 WAVE input")
  list(data = .base64_encode(data), media_type = paste0("audio/pcm;rate=", format$rate))
}

build_live_completion <- function(lm, request, ...) {
  .check_dots(...); request <- validate(request); dialect <- .live_dialect(lm)
  config <- .live_request_config(request)
  setup <- live_setup_frames(lm, config)
  frames <- list(); add <- function(frame) frames[[length(frames) + 1L]] <<- frame
  provider <- lm$definition$id
  if (dialect == "openai-responses") {
    for (message in request$messages) {
      if (message$role == "tool") {
        for (part in message$parts) add(json_object(type = "conversation.item.create", item = json_object(type = "function_call_output", call_id = part$id, output = paste0(if (part$is_error) "[error] " else "", .parts_text(part$content, provider)))))
      } else {
        parts <- Filter(function(part) !part$type %in% c("tool_call", "tool_result"), message$parts)
        if (length(parts)) add(json_object(type = "conversation.item.create", item = json_object(type = "message", role = message$role, content = lapply(parts, .native_block, dialect = "responses", provider = provider, compat = list()))))
        for (part in Filter(function(p) p$type == "tool_call", message$parts)) add(json_object(type = "conversation.item.create", item = json_object(type = "function_call", call_id = part$id, name = part$name, arguments = .json_encode(part$input))))
      }
    }
    frame <- json_object(type = "response.create")
    if (identical(request$config$extensions$output, "audio")) frame$response <- json_object(output_modalities = list("audio"))
    add(frame)
  } else {
    output <- request$config$extensions$output
    native <- .live_audio_native(request$model)
    if (native || identical(output, "audio")) {
      setup[[1L]]$setup$generationConfig$responseModalities <- list("AUDIO")
      if (!identical(output, "audio")) setup[[1L]]$setup$outputAudioTranscription <- json_object()
    } else if (identical(output, "image")) setup[[1L]]$setup$generationConfig$responseModalities <- list("IMAGE")
    messages <- request$messages
    if (native && length(messages) == 1L && messages[[1L]]$role == "user") {
      text <- list(); media <- list(); parts <- list()
      for (part in messages[[1L]]$parts) {
        if (part$type == "text") text <- c(text, list(json_object(realtimeInput = json_object(text = part$text))))
        else if (part$type %in% c("audio", "video")) {
          if (is.null(part$data) && is.null(part$path)) .unsupported(provider, "URL/file-id media on realtime input")
          value <- list(data = .media_base64(part), media_type = part$media_type)
          if (part$type == "audio" && grepl("wav|wave", part$media_type)) value <- .wav_pcm(media_bytes(part))
          input <- json_object(); input[[part$type]] <- json_object(mimeType = value$media_type, data = value$data)
          media <- c(media, list(json_object(realtimeInput = input)))
        } else parts <- c(parts, list(.native_block(part, "gemini", provider, list())))
      }
      if (length(media)) {
        add(json_object(realtimeInput = json_object(activityStart = json_object())))
        setup[[1L]]$setup$realtimeInputConfig$automaticActivityDetection$disabled <- TRUE
      }
      if (length(parts)) add(json_object(clientContent = json_object(turns = list(json_object(role = "user", parts = parts)), turnComplete = FALSE)))
      frames <- c(frames, text, media)
      if (length(media)) add(json_object(realtimeInput = json_object(activityEnd = json_object())))
    } else if (length(messages) == 1L && messages[[1L]]$role == "user" && all(vapply(messages[[1L]]$parts, function(p) p$type == "text", logical(1)))) {
      add(json_object(realtimeInput = json_object(text = .parts_text(messages[[1L]]$parts, provider))))
    } else {
      history <- request; history$config <- config()
      add(json_object(clientContent = json_object(turns = .build_payload(lm, history)$contents, turnComplete = TRUE)))
    }
  }
  list(config = config, setup_frames = setup, client_frames = frames)
}

.new_live_completion_decoder <- function(lm, request) {
  dialect <- lm$definition$dialect; calls <- list(); indices <- list(); next_index <- 0L; saw_tool <- FALSE
  function(frame) {
    data <- tryCatch(.decode_body(frame), error = function(e) NULL)
    if (!.is_object(data)) return(list())
    events <- list(); add <- function(e) events[[length(events) + 1L]] <<- e
    delta <- function(d) add(stream_delta_event(d))
    if (!is.null(data$error)) {
      for (event in .live_error(lm, data)) add(stream_error_event(event$error))
      return(events)
    }
    if (dialect == "gemini") {
      server <- .wire_object(data$serverContent)
      parts <- c(lapply(.wire_array(data$toolCall$functionCalls), function(fc) json_object(functionCall = fc)), .wire_array(server$modelTurn$parts))
      envelope <- json_object(candidates = list(json_object(content = json_object(parts = parts))))
      for (event in .parse_stream_frame(lm, request, .json_encode(envelope))) {
        if (event$type == "delta" && event$delta$type == "tool_call") saw_tool <<- TRUE
        add(event)
      }
      if (nzchar(.wire_string(server$outputTranscription$text))) delta(text_delta(server$outputTranscription$text))
      if (isTRUE(server$interrupted)) .abort("Realtime completion was interrupted.", "stream_assembly")
      if (isTRUE(server$turnComplete) || (saw_tool && length(data$toolCall$functionCalls %||% list()))) add(stream_end_event(finish_reason = if (saw_tool) "tool_call" else "stop", usage = .usage_wire(data$usageMetadata %||% server$usageMetadata, "gemini"), provider_data = data))
      return(events)
    }
    type <- data$type %||% ""
    if (type %in% c("response.output_text.delta", "response.text.delta", "response.output_audio_transcript.delta", "response.audio_transcript.delta")) delta(text_delta(.wire_string(data$delta %||% data$text)))
    if (type == "response.output_audio.delta") delta(audio_delta(data = data$delta, media_type = "audio/wav"))
    item <- .wire_object(data$item)
    tool_item <- type %in% c("response.output_item.added", "response.output_item.done") && identical(item$type, "function_call")
    if (tool_item || type %in% c("response.function_call_arguments.delta", "response.function_call_arguments.done")) {
      keys <- unique(unlist(Filter(Negate(is.null), list(item$id, data$item_id, item$call_id, data$call_id, data$id)), use.names = FALSE))
      if (!length(keys)) keys <- paste0("output:", .wire_int(data$output_index %||% 0L))
      previous <- unique(unlist(indices[keys], use.names = FALSE))
      if (length(previous) > 1L) .abort("Conflicting realtime tool-call identifiers.", "stream_assembly")
      index <- if (length(previous)) previous[[1L]] else next_index
      if (!length(previous)) next_index <<- next_index + 1L
      for (alias in keys) indices[[alias]] <<- index
      key <- as.character(index)
      state <- calls[[key]] %||% list(input = "", id = NULL, name = NULL)
      state$id <- item$call_id %||% data$call_id %||% state$id
      state$name <- item$name %||% data$name %||% state$name
      fragment <- if (type == "response.function_call_arguments.delta") .wire_string(data$delta) else ""
      full <- if (tool_item) item$arguments else if (endsWith(type, ".done")) data$arguments else NULL
      if (!is.null(full)) {
        full <- .wire_string(full)
        if (!startsWith(full, state$input)) .abort("Conflicting final realtime tool arguments.", "stream_assembly")
        fragment <- substring(full, nchar(state$input) + 1L)
      }
      state$input <- paste0(state$input, fragment); calls[[key]] <<- state; saw_tool <<- TRUE
      delta(tool_call_delta(fragment, part_index = index, id = state$id, name = state$name))
    }
    if (type %in% c("response.done", "response.completed")) {
      response <- .wire_object(data$response)
      if ((response$status %||% "") %in% c("failed", "cancelled", "canceled")) .abort("Realtime response did not complete successfully.", "stream_assembly")
      u <- .wire_object(response$usage)
      u$input_tokens_details <- u$input_token_details %||% u$input_tokens_details
      u$output_tokens_details <- u$output_token_details %||% u$output_tokens_details
      add(stream_end_event(finish_reason = if (saw_tool) "tool_call" else "stop", usage = .usage_wire(u, "responses"), provider_data = response))
    }
    events
  }
}
.stream_live_completion <- function(lm, request, on_event) {
  built <- build_live_completion(lm, request)
  wire <- live_connection_request(lm, built$config)
  socket <- .open_live_socket(wire, lm$live_connect %||% .websocket_connection, 30, 1024L, 32 * 1024^2)
  on.exit(tryCatch(socket$close(), error = function(e) warning("Live connection cleanup failed; the request will not be replayed.", call. = FALSE)), add = TRUE)
  source <- .new_stream(lm, request, on_event); decode <- .new_live_completion_decoder(lm, request)
  for (frame in built$setup_frames) socket$send(.json_encode(frame))
  if (lm$definition$dialect == "gemini") {
    deadline <- proc.time()[["elapsed"]] + 30
    repeat {
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Live setup timed out.", "timeout")
      frame <- socket$receive(remaining)
      if (is.null(frame)) return(source$finish())
      data <- tryCatch(.decode_body(frame), error = function(e) json_object())
      if (!is.null(data$error)) .abort("Provider rejected live setup.", "invalid_request", lm$definition$id)
      if ("setupComplete" %in% names(data)) break
    }
  }
  for (frame in built$client_frames) socket$send(.json_encode(frame))
  repeat {
    frame <- socket$receive(120)
    if (is.null(frame)) return(source$finish())
    events <- decode(frame)
    for (event in events) {
      if (event$type == "error") {
        error <- .redact_wire_condition(lm15_error(event$error$message, code = event$error$code, provider_code = event$error$provider_code), wire)
        event <- stream_error_event(error_detail(error$code, message = error$message, provider_code = error$provider_code))
      }
      source$push(event)
    }
    if (any(vapply(events, function(e) e$type == "end", logical(1)))) return(source$finish())
  }
}
