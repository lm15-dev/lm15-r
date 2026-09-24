response_to_events <- function(response, ..., on_event = NULL) {
  .check_dots(...); response <- validate(response)
  if (!inherits(response, "lm15_Response")) stop("Expected response().", call. = FALSE)
  if (!is.null(on_event) && !is.function(on_event)) stop("on_event must be a function.", call. = FALSE)
  # Validate the whole conversion before emitting anything: fields without a
  # delta representation are errors, not permission to lose response content.
  for (part in response$message$parts) {
    if (!part$type %in% c("text", "thinking", "tool_call", "image", "audio", "citation")) .unsupported("stream", paste("response part", part$type))
    if (part$type == "image" && (!is.null(part$path) || !is.null(part$detail))) .unsupported("stream", "image path/detail in response-to-events conversion")
    if (part$type == "audio" && is.null(part$data)) .unsupported("stream", "non-inline audio in response-to-events conversion")
  }
  if (length(response$logprobs) && !any(vapply(response$message$parts, function(p) p$type == "text", logical(1)))) .unsupported("stream", "log probabilities without a text part")
  events <- list(); add <- function(event) {
    if (is.null(on_event)) events[[length(events) + 1L]] <<- event else on_event(event)
  }
  add(stream_start_event(id = response$id, model = response$model)); logs <- response$logprobs
  for (i in seq_along(response$message$parts)) {
    part <- response$message$parts[[i]]; index <- i - 1L
    delta <- switch(part$type,
      text = { out <- text_delta(part$text, part_index = index, logprobs = logs); logs <- list(); out },
      thinking = thinking_delta(part$text, part_index = index),
      tool_call = tool_call_delta(.json_encode(part$input), part_index = index, id = part$id, name = part$name),
      image = image_delta(data = part$data, url = part$url, file_id = part$file_id, media_type = part$media_type, part_index = index),
      audio = audio_delta(data = part$data, media_type = part$media_type, part_index = index),
      citation = citation_delta(text = part$text, title = part$title, url = part$url, part_index = index))
    add(stream_delta_event(delta))
    for (state in part$continuation) add(stream_delta_event(continuation_delta(state$provider, state$kind, data = state$data, part_index = index)))
  }
  for (state in response$message$continuation) add(stream_delta_event(continuation_delta(state$provider, state$kind, data = state$data)))
  add(stream_end_event(finish_reason = response$finish_reason, usage = response$usage, provider_data = response$provider_data))
  if (is.null(on_event)) events else invisible(response)
}

complete_from_openai_chat <- function(lm, body, ..., streaming = isTRUE(body$stream), on_event = function(event) invisible(NULL)) {
  .check_dots(...); .coerce_field(streaming, "bool", "streaming", FALSE)
  if (inherits(lm, "lm15_router")) {
    routed <- route_openai_chat(lm, body); lm <- routed$lm; request <- routed$request
  } else {
    provider <- if (identical(lm$definition$dialect, "openai-chat")) lm$definition$id else "openai-chat"
    request <- request_from_openai_chat(body, provider = provider)
  }
  if (streaming) stream(lm, request, on_event) else complete(lm, request)
}
