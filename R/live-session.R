websocket_connect <- function(url, headers = list(), timeout = 30, max_queue = 1024L, max_frame_bytes = 32 * 1024^2, ..., ca_bundle = NULL) {
  .check_dots(...); .string(url, "url")
  .number(timeout, "timeout"); .number(max_frame_bytes, "max_frame_bytes", TRUE)
  if (timeout <= 0 || max_frame_bytes <= 0) stop("WebSocket limits must be positive.", call. = FALSE)
  if (!is.null(ca_bundle)) .string(ca_bundle, "ca_bundle")
  if (!is.list(headers) || (length(headers) && (is.null(names(headers)) || any(!nzchar(names(headers))) || anyDuplicated(names(headers))))) stop("WebSocket headers must be a uniquely named list.", call. = FALSE)
  header_lines <- vapply(names(headers), function(name) paste0(name, ": ", .string(headers[[name]], "header value")), "")
  socket <- tryCatch(.Call(C_lm15_ws_open, url, unname(header_lines), timeout, ca_bundle), error = function(e) {
    detail <- conditionMessage(e)
    code <- if (grepl("libcurl lacks WebSocket", detail, fixed = TRUE)) "not_configured" else if (grepl("browser WebSocket bridge", detail, fixed = TRUE)) "unsupported_feature" else "transport"
    .abort(paste("Verified WebSocket handshake failed:", detail), code)
  })
  if (inherits(socket, "lm15_ws_failure")) {
    code <- if (socket$status >= 300L) .http_code(socket$status) else if (socket$curl_code == 28L) "timeout" else "transport"
    .abort("Verified WebSocket handshake failed; server diagnostics are suppressed.", code, status = if (socket$status > 0L) socket$status else NULL)
  }
  closed <- FALSE; chunks <- list(); bytes <- 0
  close <- function() { if (!closed) { closed <<- TRUE; .Call(C_lm15_ws_close, socket) }; invisible(NULL) }
  send <- function(frame) {
    if (closed) .abort("Cannot send on a closed live connection.", "transport")
    payload <- charToRaw(enc2utf8(frame)); offset <- 0
    if (length(payload) > max_frame_bytes) .abort("Outgoing WebSocket frame exceeds the configured limit.", "transport")
    deadline <- proc.time()[["elapsed"]] + timeout
    repeat {
      sent <- tryCatch(.Call(C_lm15_ws_send, socket, payload, offset), error = function(e) { close(); .abort("WebSocket send failed; no replay was attempted.", "transport") })
      offset <- offset + sent
      if (offset == length(payload)) return(invisible(NULL))
      if (proc.time()[["elapsed"]] >= deadline) { close(); .abort("WebSocket send timed out.", "timeout") }
      if (sent == 0) Sys.sleep(0.01)
    }
  }
  receive <- function(timeout) {
    if (closed) return(NULL)
    deadline <- proc.time()[["elapsed"]] + .number(timeout, "timeout")
    repeat {
      chunk <- tryCatch(.Call(C_lm15_ws_recv, socket, max_frame_bytes), error = function(e) { close(); .abort("WebSocket receive failed or exceeded its frame limit.", "transport") })
      if (identical(chunk, FALSE)) { close(); return(NULL) }
      if (!is.null(chunk)) {
        bytes <<- bytes + length(chunk)
        if (bytes > max_frame_bytes) { close(); .abort("WebSocket message exceeds the configured limit.", "transport") }
        complete <- isTRUE(attr(chunk, "complete")); attributes(chunk) <- NULL
        chunks[[length(chunks) + 1L]] <<- chunk
        if (complete) {
          frame <- if (length(chunks)) do.call(c, chunks) else raw()
          chunks <<- list(); bytes <<- 0
          return(frame)
        }
      }
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Waiting for a WebSocket frame timed out.", "timeout")
      if (is.null(chunk)) Sys.sleep(min(remaining, 0.01))
    }
  }
  list(send = send, receive = receive, close = close)
}
.websocket_connection <- websocket_connect

live <- function(lm, config, ..., connect = lm$live_connect %||% .websocket_connection, timeout = 30, max_queue = 1024L, max_frame_bytes = 32 * 1024^2, max_turn_bytes = 128 * 1024^2) {
  .check_dots(...); pair <- .route(lm, config); lm <- pair$lm; config <- pair$request; dialect <- .live_dialect(lm)
  if (.number(timeout, "timeout") <= 0) stop("Live timeout must be positive.", call. = FALSE)
  for (value in list(max_queue, max_frame_bytes, max_turn_bytes)) if (.number(value, "live limit", TRUE) <= 0) stop("Live limits must be positive integers.", call. = FALSE)
  setup <- live_setup_frames(lm, config)
  wire <- live_connection_request(lm, config)
  socket <- .open_live_socket(wire, connect, timeout, max_queue, max_frame_bytes)
  closed <- FALSE; pending <- list(); success <- FALSE
  close_session <- function() { if (!closed) { closed <<- TRUE; socket$close() }; invisible(NULL) }
  on.exit(if (!success) close_session(), add = TRUE)
  for (frame in setup) socket$send(.json_encode(frame))
  if (dialect == "gemini") {
    deadline <- proc.time()[["elapsed"]] + timeout
    repeat {
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Live setup timed out.", "timeout")
      raw <- socket$receive(remaining)
      if (is.null(raw)) .abort("Live connection closed before setup completed.", "transport")
      p <- tryCatch(.decode_body(raw), error = function(e) json_object())
      if (.is_object(p) && "setupComplete" %in% names(p)) break
      if (.is_object(p) && !is.null(p$error)) .abort("Provider rejected live setup.", "invalid_request", lm$definition$id)
    }
  }
  send <- function(event) {
    if (closed) .abort("Live session is closed.", "transport")
    frames <- live_encode(lm, config, event)
    tryCatch(for (frame in frames) socket$send(.json_encode(frame)), error = function(e) { close_session(); .abort("Live send failed; the session was closed to prevent an ambiguous replay.", "transport") })
    invisible(NULL)
  }
  next_event <- function(wait = timeout) {
    deadline <- proc.time()[["elapsed"]] + wait
    repeat {
      if (length(pending)) { out <- pending[[1L]]; pending <<- pending[-1L]; return(out) }
      if (closed) return(NULL)
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Waiting for a live event timed out.", "timeout")
      raw <- socket$receive(remaining)
      if (is.null(raw)) { close_session(); return(NULL) }
      pending <<- live_decode(lm, raw)
      if (length(pending) > max_queue) { close_session(); .abort("Live event queue exceeded its configured limit.", "transport") }
    }
  }
  turn <- function(content, on_event = function(event) NULL, wait = timeout) {
    send(if (inherits(content, "lm15_LiveClientEvent")) content else live_client_turn_event(.normalize_parts(content)))
    deadline <- proc.time()[["elapsed"]] + wait; events <- list(); collected_bytes <- 0
    repeat {
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Live turn timed out.", "timeout")
      event <- next_event(remaining)
      if (is.null(event)) .abort("Live session closed before the turn finished.", "transport")
      collected_bytes <- collected_bytes + as.double(utils::object.size(event))
      if (length(events) >= max_queue || collected_bytes > max_turn_bytes) { close_session(); .abort("Collected live turn exceeded its limit; consume next_event() for longer turns.", "transport") }
      events[[length(events) + 1L]] <- event; on_event(event)
      if (event$type %in% c("turn_end", "interrupted", "error", "tool_call")) return(events)
    }
  }
  success <- TRUE
  structure(list(send = send, next_event = next_event, turn = turn, interrupt = function() send(live_client_interrupt_event()), close = close_session), class = "lm15_live_session")
}
print.lm15_live_session <- function(x, ...) { cat("<lm15 live session>\n"); invisible(x) }
str.lm15_live_session <- function(object, ...) { print.lm15_live_session(object); invisible(NULL) }
