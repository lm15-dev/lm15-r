live_connection_request <- function(lm, config, ...) {
  .check_dots(...); dialect <- .live_dialect(lm); config <- validate(config)
  if (!inherits(config, "lm15_LiveConfig")) stop("Expected live_config().", call. = FALSE)
  if (dialect == "gemini") {
    credential <- .credential_value(lm$definition$id, lm$credential)
    if (!credential$kind %in% c("api_key", "bearer_token")) .unsupported(lm$definition$id, "this live credential kind")
    root <- sub("^(https?://[^/]+).*$", "\\1", lm$base_url)
    url <- paste0(root, "/ws/google.ai.generativelanguage.v1beta.GenerativeService.BidiGenerateContent")
    headers <- list()
    if (credential$kind == "api_key") url <- paste0(url, "?key=", .path_id(credential$value)) else headers$authorization <- paste("Bearer", credential$value)
  } else {
    wire <- .emit(lm, "GET", "/realtime", params = list(model = config$model))
    url <- wire$url; headers <- wire$headers; headers[["content-type"]] <- NULL
  }
  structure(list(method = "GET", url = sub("^http", "ws", url), headers = headers, body = raw()), class = "lm15_wire_request")
}
.open_live_socket <- function(wire, connect, timeout, max_queue, max_frame_bytes) {
  socket <- tryCatch(connect(wire$url, wire$headers, timeout, max_queue, max_frame_bytes), error = function(e) {
    if (inherits(e, "LM15Error")) stop(.redact_wire_condition(e, wire))
    .abort("Cannot open live connection; secret diagnostics are suppressed.", "transport")
  })
  if (!is.list(socket) || any(!vapply(socket[c("send", "receive", "close")], is.function, logical(1)))) stop("connect must return send, receive, and close functions.", call. = FALSE)
  socket
}
