# The browser owns asynchronous fetch and cancellation. webR evaluates only
# pure codecs here, so no curl connection or blocking JavaScript call is used.
.browser_sessions <- new.env(parent = emptyenv())

browser_dispatch <- function(action, id, payload = "{}", ...) {
  .check_dots(...)
  .string(action, "action"); .string(id, "session id")
  response <- tryCatch({
    input <- .json_decode(payload)
    if (is.character(input$args)) input$args <- .json_decode(input$args)
    if (action %in% c("prepare", "resource_prepare", "live_prepare")) {
      if (exists(id, .browser_sessions, inherits = FALSE)) stop("Browser request id is already active.", call. = FALSE)
      credential <- if (.is_object(input$api_key)) from_dict(input$api_key, "credential") else input$api_key
      lm <- new_lm(input$provider, api_key = credential, base_url = input$base_url, compat = input$compat,
        settings = input$settings %||% list(), account_id = input$account_id,
        env = character(), transport = function(...) .abort("Browser networking is owned by the JavaScript bridge.", "transport"))
      if (action == "live_prepare") {
        config <- if (is.character(input$config)) from_json(input$config, "live_config") else from_dict(input$config, "live_config")
        frames <- live_setup_frames(lm, config)
        wire <- live_connection_request(lm, config)
        state <- new.env(parent = emptyenv()); state$lm <- lm; state$config <- config; state$wire <- wire; state$events <- list(); state$json_only <- isTRUE(input$json_only)
        assign(id, state, .browser_sessions)
        return(.json_encode(json_object(ok = TRUE, result = json_object(url = wire$url, headers = .json_object(wire$headers), setup_frames = frames, wait_for_setup = lm$definition$dialect == "gemini"))))
      }
      if (action == "resource_prepare") {
        wires <- .browser_resource_build(lm, input)
        state <- new.env(parent = emptyenv()); state$lm <- lm; state$input <- input; state$events <- list(); state$json_only <- isTRUE(input$json_only)
        state$wire <- if (length(wires)) wires[[1L]] else list(url = "", headers = list())
        assign(id, state, .browser_sessions)
        return(.json_encode(json_object(ok = TRUE, result = json_object(requests = lapply(wires, function(wire) json_object(method = wire$method, url = wire$url, headers = .json_object(wire$headers), body_b64 = .base64_encode(wire$body)))))))
      }
      req <- if (is.character(input$request)) from_json(input$request, "request") else from_dict(input$request, "request")
      streaming <- isTRUE(input$stream)
      if (lm$definition$access$backend == "chatgpt-codex" && !streaming) .unsupported(lm$definition$id, "non-streaming Codex browser call; use stream()")
      wire <- build_request(lm, req, stream = streaming)
      state <- new.env(parent = emptyenv()); state$lm <- lm; state$request <- req; state$wire <- wire; state$events <- list(); state$stream <- NULL; state$json_only <- isTRUE(input$json_only)
      if (streaming) state$stream <- .new_stream(lm, req, function(e) state$events[[length(state$events) + 1L]] <- .redact_error_event(e, state$wire))
      assign(id, state, .browser_sessions)
      json_object(method = wire$method, url = wire$url, headers = .json_object(wire$headers), body_b64 = .base64_encode(wire$body))
    } else if (action == "dispose") {
      if (exists(id, .browser_sessions, inherits = FALSE)) rm(list = id, envir = .browser_sessions)
      json_object()
    } else {
      if (!exists(id, .browser_sessions, inherits = FALSE)) stop("Browser request id is not active.", call. = FALSE)
      state <- get(id, .browser_sessions, inherits = FALSE)
      take_events <- function() { out <- lapply(state$events, if (isTRUE(state$json_only)) as_json else as_dict); state$events <- list(); out }
      render <- function(value) if (isTRUE(state$json_only)) as_json(value, include_provider_data = TRUE) else as_dict(value, include_provider_data = TRUE)
      if (action == "live_send") {
        event <- if (is.character(input$event)) from_json(input$event, "live_client_event") else from_dict(input$event, "live_client_event")
        json_object(frames = live_encode(state$lm, state$config, event))
      } else if (action == "live_receive") {
        state$events <- lapply(live_decode(state$lm, jsonlite::base64_dec(input$body_b64)), .redact_error_event, wire = state$wire)
        json_object(events = take_events())
      } else if (action == "resource_response") {
        .browser_resource_parse(state, input$replies %||% list())
      } else if (action == "response") {
        bytes <- jsonlite::base64_dec(input$body_b64)
        out <- parse_response(state$lm, state$request, bytes, status = .wire_int(input$status), headers = input$headers %||% list())
        json_object(response = render(out))
      } else if (action == "feed") {
        if (is.null(state$stream)) stop("Browser request was not opened as a stream.", call. = FALSE)
        state$stream$feed(jsonlite::base64_dec(input$body_b64))
        json_object(events = take_events())
      } else if (action == "finish") {
        if (is.null(state$stream)) stop("Browser request was not opened as a stream.", call. = FALSE)
        out <- state$stream$finish()
        json_object(events = take_events(), response = render(out))
      } else stop("Unknown browser bridge action.", call. = FALSE)
    }
  }, error = function(e) {
    # Serialization below is the deliberate error boundary, with no R call,
    # environment, credential closure or network URL included.
    if (exists(id, .browser_sessions, inherits = FALSE)) {
      state <- get(id, .browser_sessions, inherits = FALSE)
      e <- .redact_wire_condition(e, state$wire)
    }
    fields <- json_object(code = if (inherits(e, "LM15Error")) e$code else "invalid_request", message = if (inherits(e, "LM15Error")) e$message else "Invalid browser request or malformed provider data.")
    if (inherits(e, "StreamAssemblyError") && !is.null(e$partial)) fields$partial <- as_dict(e$partial)
    if (exists(id, .browser_sessions, inherits = FALSE)) {
      state <- get(id, .browser_sessions, inherits = FALSE)
      if (isTRUE(state$json_only) && !is.null(fields$partial)) fields$partial <- .json_encode(fields$partial)
      if (length(state$events)) { fields$events <- lapply(state$events, if (isTRUE(state$json_only)) as_json else as_dict); state$events <- list() }
    }
    structure(fields, class = c("lm15_browser_error", class(fields)))
  })
  if (inherits(response, "lm15_browser_error")) .json_encode(json_object(ok = FALSE, error = .json_object(unclass(response))))
  else .json_encode(json_object(ok = TRUE, result = response))
}
