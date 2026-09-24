.browser_resource_build <- function(lm, input) {
  args <- input$args %||% json_object(); surface <- input$surface; action <- input$action
  read <- function(value, kind) if (!is.null(value)) from_dict(value, kind) else NULL
  limit <- .wire_int(args$limit %||% 20L)
  switch(surface,
    models = list(build_models_request(lm)),
    image = list(generation_build(lm, read(args$request, "image_generation_request"))),
    speech = list(generation_build(lm, read(args$request, "speech_generation_request"))),
    files = list(file_op_build(lm, action, upload_request = read(args$request, "file_upload_request"), file_id = args$id, limit = limit, cursor = args$cursor)),
    cache = list(cache_op_build(lm, action, prefix = read(args$prefix, "request"), id = args$id, limit = limit, cursor = args$cursor, ttl_seconds = .wire_int(args$ttl_seconds), label = args$label)),
    batch = batch_op_build(lm, action, request = read(args$request, "batch_request"), id = args$id, limit = limit, upload_body = args$upload_body, status_body = args$status_body),
    video = video_op_build(lm, action, request = read(args$request, "video_generation_request"), id = args$id, limit = limit, status_body = args$status_body, model = args$model),
    stop("Unknown browser resource surface.", call. = FALSE))
}
.browser_resource_parse <- function(state, replies) {
  lm <- state$lm; input <- state$input; args <- input$args %||% json_object(); action <- input$action
  for (reply in replies) if (.wire_int(reply$status) >= 300L) stop(normalize_error(lm, .wire_int(reply$status), rawToChar(jsonlite::base64_dec(reply$body_b64)), headers = reply$headers %||% list()))
  bytes <- lapply(replies, function(reply) jsonlite::base64_dec(reply$body_b64))
  body <- if (length(bytes)) bytes[[1L]] else raw()
  headers <- if (length(replies)) replies[[1L]]$headers else list()
  data <- function() .decode_body(body)
  value <- switch(input$surface,
    models = lapply(parse_models_response(lm, body), as_dict),
    image = as_dict(generation_parse(lm, from_dict(args$request, "image_generation_request"), body, headers = headers)),
    speech = as_dict(generation_parse(lm, from_dict(args$request, "speech_generation_request"), body, headers = headers)),
    files = if (action == "delete") NULL else if (action == "download") json_object(bytes_b64 = .base64_encode(body)) else as_dict(file_op_parse(lm, body, page = action == "list")),
    cache = if (action == "delete") NULL else if (action == "list") as_dict(cache_page(items = lapply(data()$cachedContents %||% list(), .parse_cache), next_cursor = data()$nextPageToken)) else as_dict(.parse_cache(data())),
    batch = {
      if (action == "upload") if (length(body)) data() else NULL
      else {
        kind <- switch(action, list = "list", result_fetches = "entries", "job")
        result <- batch_op_parse(lm, kind, body, status_body = args$status_body, fetched = bytes)
        if (kind == "job") as_dict(result) else lapply(result, as_dict)
      }
    },
    video = {
      kind <- switch(action, list = "list", result_fetch = "part", "job")
      fetched <- if (length(bytes)) list(body = body, headers = headers) else NULL
      result <- video_op_parse(lm, kind, body, id = args$id, status_body = args$status_body, fetched = fetched)
      if (kind == "list") lapply(result, as_dict) else as_dict(result)
    })
  json_object(value = if (isTRUE(state$json_only)) .json_encode(value) else value)
}
