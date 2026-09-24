.live_dialect <- function(lm) {
  .require_surface(lm, "live")
  dialect <- lm$definition$dialect
  if (!dialect %in% c("gemini", "openai-responses")) .unsupported(lm$definition$id, "live sessions")
  dialect
}
.live_audio_native <- function(model) grepl("native-audio|live-preview", model, ignore.case = TRUE)
.live_audio_format <- function(fmt) {
  if (fmt$encoding == "pcm16") json_object(type = "audio/pcm", rate = fmt$sample_rate)
  else json_object(type = paste0("audio/", fmt$encoding))
}

live_setup_frames <- function(lm, config, ...) {
  .check_dots(...); config <- validate(config); dialect <- .live_dialect(lm)
  if (!inherits(config, "lm15_LiveConfig")) stop("Expected live_config().", call. = FALSE)
  if (any(vapply(config$tools, function(tool) tool$type == "builtin", logical(1)))) .unsupported(lm$definition$id, "builtin tools in live sessions")
  instructions <- if (is.character(config$system)) config$system else if (!is.null(config$system)) .parts_text(config$system, lm$definition$id) else NULL
  tools <- lapply(Filter(function(t) t$type == "function", config$tools), function(t) json_object(name = t$name, description = t$description, parameters = t$parameters))
  if (dialect == "gemini") {
    setup <- json_object(model = if (startsWith(config$model, "models/")) config$model else paste0("models/", config$model))
    if (!is.null(instructions)) setup$systemInstruction <- json_object(parts = list(json_object(text = instructions)))
    if (length(tools)) setup$tools <- list(json_object(functionDeclarations = tools))
    generation <- json_object()
    if (!is.null(config$output_format) || .live_audio_native(config$model)) generation$responseModalities <- list("AUDIO")
    if (!is.null(config$voice)) generation$speechConfig <- json_object(voiceConfig = json_object(prebuiltVoiceConfig = json_object(voiceName = config$voice)))
    if (length(generation)) setup$generationConfig <- generation
    if (!is.null(config$extensions)) setup[names(config$extensions)] <- config$extensions
    if (.live_audio_native(config$model)) setup$outputAudioTranscription <- json_object()
    return(list(json_object(setup = setup)))
  }
  session <- json_object(type = "realtime")
  if (!is.null(instructions)) session$instructions <- instructions
  audio <- json_object()
  if (!is.null(config$output_format) || !is.null(config$voice)) {
    session$output_modalities <- list("audio"); output <- json_object()
    if (!is.null(config$output_format)) output$format <- .live_audio_format(config$output_format)
    if (!is.null(config$voice)) output$voice <- config$voice
    audio$output <- output
  } else session$output_modalities <- list("text")
  if (!is.null(config$input_format)) audio$input <- json_object(format = .live_audio_format(config$input_format), turn_detection = NULL)
  if (length(audio)) session$audio <- audio
  if (length(config$tools)) session$tools <- lapply(tools, function(t) .json_object(c(list(type = "function"), unclass(t))))
  if (!is.null(config$extensions)) session[names(config$extensions)] <- config$extensions
  list(json_object(type = "session.update", session = session))
}

live_encode <- function(lm, config, event, ...) {
  .check_dots(...); event <- validate(event); config <- validate(config); dialect <- .live_dialect(lm)
  if (!inherits(event, "lm15_LiveClientEvent")) stop("Expected a live client event.", call. = FALSE)
  k <- event$type; provider <- lm$definition$id
  block <- function(p) .native_block(p, if (dialect == "gemini") "gemini" else "responses", provider, list())
  if (dialect == "gemini") {
    if (k == "audio") return(list(json_object(realtimeInput = json_object(audio = json_object(mimeType = event$media_type, data = event$data)))))
    if (k == "image") return(list(json_object(realtimeInput = json_object(video = json_object(mimeType = event$media_type, data = event$data)))))
    if (k == "end_audio") return(list(json_object(realtimeInput = json_object(audioStreamEnd = TRUE))))
    if (k == "interrupt") return(list(json_object(clientContent = json_object(turnComplete = TRUE))))
    if (k == "tool_result") return(list(json_object(toolResponse = json_object(functionResponses = list(json_object(id = event$id, response = json_object(output = list(json_object(text = .parts_text(event$content, provider))))))))))
    if (k == "text" && .live_audio_native(config$model)) return(list(json_object(realtimeInput = json_object(text = event$text))))
    parts <- if (k == "text") list(json_object(text = event$text)) else lapply(event$parts, block)
    return(list(json_object(clientContent = json_object(turns = list(json_object(role = "user", parts = parts)), turnComplete = if (k == "text") TRUE else event$turn_complete))))
  }
  if (k == "audio") return(list(json_object(type = "input_audio_buffer.append", audio = event$data)))
  if (k == "end_audio") return(list(json_object(type = "input_audio_buffer.commit"), json_object(type = "response.create")))
  if (k == "interrupt") return(list(json_object(type = "response.cancel")))
  if (k == "tool_result") item <- json_object(type = "function_call_output", call_id = event$id, output = .parts_text(event$content, provider))
  else {
    parts <- switch(k, text = list(json_object(type = "input_text", text = event$text)), image = list(json_object(type = "input_image", image_url = paste0("data:", event$media_type, ";base64,", event$data))), turn = lapply(event$parts, block))
    item <- json_object(type = "message", role = "user", content = parts)
  }
  frames <- list(json_object(type = "conversation.item.create", item = item))
  if (k != "turn" || event$turn_complete) frames <- c(frames, list(json_object(type = "response.create")))
  frames
}

.live_error <- function(lm, payload) {
  e <- .wire_object(payload$error)
  pc <- .wire_string(if (lm$definition$dialect == "gemini") e$status %||% e$code else e$code %||% e$type %||% payload$code %||% payload$error_type, "provider")
  if (pc == "response_cancel_not_active") return(list())
  message <- .wire_string(e$message %||% payload$message)
  map <- .error_type_maps[[if (lm$definition$dialect == "gemini") "gemini" else "openai"]]
  code <- if (pc %in% names(map)) unname(map[[pc]]) else "provider"
  if (.pinned_model_not_found(pc, message)) code <- "unsupported_model"  # MAP-15
  list(live_server_error_event(error_detail(code, message = message, provider_code = pc)))
}

live_decode <- function(lm, frame, ...) {
  .check_dots(...); dialect <- .live_dialect(lm)
  p <- tryCatch(.decode_body(frame), error = function(e) NULL)
  if (!.is_object(p)) return(list())
  if (!is.null(p$error) || (p$type %||% "") %in% c("error", "response.error")) return(.live_error(lm, p))
  events <- list(); add <- function(e) events[[length(events) + 1L]] <<- e
  if (dialect == "gemini") {
    call <- function(fc) live_server_tool_call_event(.wire_string(fc$id, "fc_0"), .wire_string(fc$name, "tool"), input = .wire_object(fc$args))
    for (fc in .wire_array(p$toolCall$functionCalls)) if (.is_object(fc)) add(call(fc))
    server <- p$serverContent
    if (!.is_object(server)) return(events)
    for (part in .wire_array(server$modelTurn$parts)) {
      if (!.is_object(part)) next
      if ("text" %in% names(part)) add(live_server_text_event(.wire_string(part$text)))
      else if (.is_object(part$inlineData) && startsWith(.wire_string(part$inlineData$mimeType), "audio/")) add(live_server_audio_event(part$inlineData$data, media_type = part$inlineData$mimeType))
      else if (.is_object(part$functionCall)) add(call(part$functionCall))
    }
    if (nzchar(.wire_string(server$outputTranscription$text))) add(live_server_text_event(server$outputTranscription$text))
    u <- p$usageMetadata %||% server$usageMetadata
    if (.is_object(u) && !isTRUE(server$turnComplete)) add(live_server_usage_event(usage = .usage_wire(u, "gemini")))
    if (isTRUE(server$interrupted)) add(live_server_interrupted_event())
    if (isTRUE(server$turnComplete)) add(live_server_turn_end_event(usage = .usage_wire(u, "gemini")))
    return(events)
  }
  type <- p$type %||% ""
  value <- .wire_string(p$delta %||% p$text)
  if (type %in% c("response.output_text.delta", "response.text.delta", "response.output_audio_transcript.delta", "response.audio_transcript.delta") && nzchar(value)) add(live_server_text_event(value))
  if (type == "response.output_audio.delta" && nzchar(value)) add(live_server_audio_event(value))
  if (type == "response.function_call_arguments.delta" && nzchar(value)) add(live_server_tool_call_delta_event(value, id = p$call_id %||% p$id, name = p$name))
  if (type == "response.output_item.done" && identical(p$item$type, "function_call")) {
    id <- .wire_string(p$item$call_id %||% p$item$id)
    if (nzchar(id)) add(live_server_tool_call_event(id, .wire_string(p$item$name, "tool"), input = .parse_input(p$item$arguments)))
  }
  if (type %in% c("response.done", "response.completed")) {
    response <- .wire_object(p$response); u <- response$usage
    if (.is_object(u)) {
      u$input_tokens_details <- u$input_token_details %||% u$input_tokens_details
      u$output_tokens_details <- u$output_token_details %||% u$output_tokens_details
    }
    tokens <- if (.is_object(u)) .usage_wire(u, "responses") else NULL
    if (identical(response$status, "cancelled")) {
      if (!is.null(tokens)) add(live_server_usage_event(usage = tokens))
      add(live_server_interrupted_event())
    } else if (any(vapply(.wire_array(response$output), function(i) .is_object(i) && identical(i$type, "function_call"), logical(1)))) {
      if (!is.null(tokens)) add(live_server_usage_event(usage = tokens))
    } else add(live_server_turn_end_event(usage = tokens %||% usage()))
  }
  if (type %in% c("response.cancelled", "response.canceled")) add(live_server_interrupted_event())
  events
}
