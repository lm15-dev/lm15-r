generation_build <- function(lm, request, ...) {
  .check_dots(...); r <- validate(request)
  image <- inherits(r, "lm15_ImageGenerationRequest")
  speech <- inherits(r, "lm15_SpeechGenerationRequest")
  if (!image && !speech) stop("Expected an image or speech generation request.", call. = FALSE)
  .require_surface(lm, if (image) "images" else "speech")
  dialect <- lm$definition$dialect; provider <- lm$definition$id
  if (dialect == "gemini") {
    ext <- r$extensions %||% json_object()
    if (speech) {
      if (!is.null(r$format)) .unsupported(provider, "speech format selection")
      g <- json_object(responseModalities = list("AUDIO"))
      if (!is.null(r$voice)) g$speechConfig <- json_object(voiceConfig = json_object(prebuiltVoiceConfig = json_object(voiceName = r$voice)))
      if (is.null(ext$generationConfig)) ext$generationConfig <- g
    } else if (!is.null(r$size)) {
      g <- ext$generationConfig %||% json_object(); g$imageConfig <- g$imageConfig %||% json_object()
      g$imageConfig$aspectRatio <- g$imageConfig$aspectRatio %||% r$size
      ext$generationConfig <- g
    }
    req <- get("request", mode = "function")(r$model, list(message_user(c(list(text(r$prompt)), if (image) r$images else list()))), config = config(extensions = ext))
    return(build_request(lm, req))
  }
  if (speech) {
    payload <- json_object(model = r$model, input = r$prompt)
    if (!is.null(r$voice)) payload$voice <- r$voice
    if (!is.null(r$format)) payload$response_format <- r$format
    if (!is.null(r$extensions)) payload[names(r$extensions)] <- r$extensions
    return(.emit(lm, "POST", "/audio/speech", payload))
  }
  payload <- json_object(model = r$model, prompt = r$prompt)
  if (!is.null(r$size)) {
    if (provider == "xai") .unsupported(provider, "image size")
    payload$size <- r$size
  }
  if (!is.null(r$extensions)) payload[names(r$extensions)] <- r$extensions
  if (!length(r$images)) return(.emit(lm, "POST", "/images/generations", payload))
  if (provider == "xai") {
    if (length(r$images) != 1L) .unsupported(provider, "more than one edit image")
    p <- r$images[[1L]]
    payload$image <- if (!is.null(p$file_id)) json_object(file_id = p$file_id) else json_object(url = p$url %||% .media_uri(p))
    return(.emit(lm, "POST", "/images/edits", payload))
  }
  compat <- .compat(lm, get("request", mode = "function")(r$model, list(message_user(r$prompt))))
  files <- lapply(seq_along(r$images), function(i) {
    p <- r$images[[i]]
    if (is.null(p$data) && is.null(p$path)) .unsupported(provider, "URL/file-id image edits")
    list(name = if (compat$edit_image_field == "indexed") paste0("image[", i - 1L, "]") else "image[]", filename = paste0("image-", i - 1L), media_type = p$media_type, data = media_bytes(p))
  })
  form <- .multipart(payload, files)
  .emit(lm, "POST", "/images/edits", body = form$body, headers = list("content-type" = form$content_type))
}

generation_parse <- function(lm, request, body, ..., headers = list()) {
  .check_dots(...)
  image <- inherits(request, "lm15_ImageGenerationRequest")
  dialect <- lm$definition$dialect
  if (dialect == "gemini") {
    req <- get("request", mode = "function")(request$model, list(message_user(request$prompt)))
    result <- parse_response(lm, req, body)
    if (!image) {
      audio <- Filter(function(p) p$type == "audio", result$message$parts)
      if (!length(audio)) .abort("Provider returned no audio.", provider = lm$definition$id)
      return(speech_generation_response(audio[[1L]], id = result$id, model = result$model, usage = result$usage, provider_data = result$provider_data))
    }
    images <- Filter(function(p) p$type == "image", result$message$parts)
    texts <- Filter(function(p) p$type == "text", result$message$parts)
    words <- paste(vapply(texts, function(p) p$text, ""), collapse = "")
    return(image_generation_response(images, text = if (nzchar(words)) words else NULL, id = result$id, model = result$model, usage = result$usage, provider_data = result$provider_data))
  }
  if (!image) {
    i <- match("content-type", tolower(names(headers)))
    mime <- if (!is.na(i)) headers[[i]] else NULL
    if (is.null(mime)) .abort("Speech response has no content-type.", provider = lm$definition$id)
    return(speech_generation_response(audio_part(data = .base64_encode(body), media_type = mime), provider_data = json_object(content_type = mime)))
  }
  data <- if (is.raw(body)) .json_decode(rawToChar(body)) else if (is.character(body)) .json_decode(body) else body
  images <- list()
  for (item in .wire_array(data$data)) {
    if (!.is_object(item)) next
    mime <- if (lm$definition$id == "xai") item$mime_type else if (!is.null(data$output_format)) paste0("image/", data$output_format) else NULL
    if (is.null(item$b64_json) && is.null(item$url)) next
    images[[length(images) + 1L]] <- image_part(data = item$b64_json, url = if (is.null(item$b64_json)) item$url else NULL, media_type = mime %||% "application/octet-stream")
  }
  image_generation_response(images, usage = .usage_wire(data$usage, "responses"), provider_data = data)
}
image_generate <- function(lm, request, ...) {
  .check_dots(...); reply <- .send_wire(lm, generation_build(lm, request))
  generation_parse(lm, request, reply$body, headers = reply$headers)
}
speech_generate <- function(lm, request, ...) {
  .check_dots(...); reply <- .send_wire(lm, generation_build(lm, request))
  generation_parse(lm, request, reply$body, headers = reply$headers)
}
