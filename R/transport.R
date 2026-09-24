# Transport implementations receive explicit wire requests and return status,
# headers and bytes. No retry or redirect may replay a paid request.
transport_curl <- function(..., timeout = 120, connect_timeout = 30, max_response_bytes = 128 * 1024^2) {
  .check_dots(...)
  .number(timeout, "timeout"); .number(connect_timeout, "connect_timeout")
  .number(max_response_bytes, "max_response_bytes", TRUE)
  if (timeout <= 0 || connect_timeout <= 0 || max_response_bytes <= 0) stop("Transport limits must be positive.", call. = FALSE)
  send <- function(wire, on_chunk = NULL) {
    if (!requireNamespace("curl", quietly = TRUE)) .abort("Desktop networking requires the curl R package. In webR, use the browser bridge or supply a transport.", "not_configured")
    handle <- curl::new_handle()
    curl::handle_setopt(handle, customrequest = wire$method, followlocation = FALSE, timeout = timeout, connecttimeout = connect_timeout)
    if (length(wire$body)) curl::handle_setopt(handle, postfields = wire$body)
    curl::handle_setheaders(handle, .list = wire$headers)
    chunks <- list(); size <- 0; status <- NULL
    curl::handle_setopt(handle, headerfunction = function(bytes) {
      line <- rawToChar(bytes)
      if (startsWith(line, "HTTP/")) {
        pieces <- strsplit(trimws(line), " +")[[1L]]
        if (length(pieces) >= 2L) status <<- suppressWarnings(as.integer(pieces[[2L]]))
      }
      length(bytes)
    })
    callback <- function(chunk) {
      size <<- size + length(chunk)
      if (size > max_response_bytes) .abort("HTTP response exceeds the configured byte limit.", "transport")
      if (is.null(on_chunk) || is.null(status) || is.na(status) || status >= 300L) chunks[[length(chunks) + 1L]] <<- chunk else on_chunk(chunk)
      TRUE
    }
    result <- tryCatch(curl::curl_fetch_stream(wire$url, callback, handle = handle),
      error = function(e) {
        if (inherits(e, "LM15Error") || inherits(e, "lm15_stream_cut")) stop(e)
        # libcurl errors may contain the full URL, including query credentials.
        .abort("HTTP transfer failed or timed out; credential-bearing diagnostics are suppressed.", "transport")
      })
    list(status = result$status_code, headers = .parse_headers(result$headers), body = if (length(chunks)) do.call(c, chunks) else raw())
  }
  structure(send, class = c("lm15_transport", "function"))
}
.parse_headers <- function(raw) {
  lines <- strsplit(rawToChar(raw), "\r?\n", perl = TRUE)[[1L]]
  out <- list()
  for (line in lines) {
    if (startsWith(line, "HTTP/")) { out <- list(); next }
    at <- regexpr(":", line, fixed = TRUE)[[1L]]
    if (at < 1L) next
    out[[tolower(substr(line, 1L, at - 1L))]] <- trimws(substring(line, at + 1L))
  }
  out
}

.emit <- function(lm, method, endpoint, payload = NULL, ..., params = list(), body = NULL, headers = list(), model = NULL, stream = FALSE) {
  .check_dots(...)
  d <- lm$definition; policy <- d$access; host <- policy$host
  hdr <- list()
  if (d$dialect == "anthropic") hdr[["anthropic-version"]] <- "2023-06-01"
  for (pair in policy$headers) hdr[[tolower(pair[[1L]])]] <- pair[[2L]]
  if (!is.null(payload) || d$dialect != "gemini") hdr[["content-type"]] <- "application/json"
  hdr[names(headers)] <- headers
  if (d$id == "openai-codex") {
    if (is.null(lm$account_id)) .abort("Codex requires account_id alongside its explicit access token.", "not_configured", d$id)
    hdr[["chatgpt-account-id"]] <- lm$account_id
  }
  if (d$dialect == "anthropic" && !is.null(payload$tools) && any(vapply(payload$tools, function(t) identical(t$name, "code_execution"), logical(1))))
    hdr[["anthropic-beta"]] <- paste(Filter(Negate(is.null), list(hdr[["anthropic-beta"]], "code-execution-2025-05-22")), collapse = ",")
  url <- paste0(lm$base_url, endpoint)
  if (!is.null(host)) {
    if (!is.null(host$stream_framing) && host$stream_framing != "sse" && stream) .unsupported(d$id, "binary event-stream framing")
    if (identical(host$model_in, "path") && !is.null(model)) {
      path <- host$paths[[if (stream) "messages/stream" else "messages"]]
      if (!is.null(path)) url <- paste0(lm$base_url, gsub("{model}", .path_id(model), path, fixed = TRUE))
      payload$model <- NULL
    }
    if (!is.null(host$anthropic_version_in) && startsWith(host$anthropic_version_in, "body:")) {
      payload$anthropic_version <- substring(host$anthropic_version_in, 6L)
      hdr[["anthropic-version"]] <- NULL
    }
    for (pair in host$required_headers %||% list()) hdr[[tolower(pair[[1L]])]] <- lm$settings[[pair[[2L]]]]
  }
  schemes <- unlist(policy$auth_scheme)
  credential <- .credential_value(d$id, lm$credential)
  scheme <- select_auth_scheme(credential, schemes, d$id)
  token <- credential$value
  if (scheme == "bearer") hdr$authorization <- paste("Bearer", token)
  if (scheme == "x-api-key") hdr[[if (d$dialect == "gemini") "x-goog-api-key" else "x-api-key"]] <- token
  if (scheme == "api-key") hdr[["api-key"]] <- token
  if (scheme == "query-key") params$key <- token
  if (length(params)) url <- paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?",  paste(vapply(names(params), function(k) paste0(.path_id(k), "=", .path_id(.wire_string(params[[k]]))), ""), collapse = "&"))
  if (!is.null(payload)) body <- charToRaw(enc2utf8(.json_encode(payload)))
  if (scheme == "sigv4") {
    if (is.null(host$sigv4_service) || is.null(lm$settings$region)) .abort("AWS signing requires a service and region.", "not_configured", d$id)
    hdr <- sigv4_sign(method, url, credential, region = lm$settings$region, service = host$sigv4_service, headers = hdr, body = body %||% raw(), now = lm$clock())$headers
  }
  structure(list(method = method, url = url, headers = hdr, body = body %||% raw()), class = "lm15_wire_request")
}
print.lm15_wire_request <- function(x, ...) { cat("<lm15 wire request: ", x$method, "; address, headers and body hidden>\n", sep = ""); invisible(x) }

.complete_impl <- function(lm, request, ...) {
  .check_dots(...)
  pair <- .route(lm, request); lm <- pair$lm; request <- pair$request
  if (lm$definition$access$backend == "chatgpt-codex" || .uses_live_completion(lm, request)) return(stream(lm, request, on_event = function(event) invisible(NULL)))
  wire <- build_request(lm, request)
  # MAP-13: a stop sequence the wire cannot take is honoured by streaming and
  # closing the connection at the cut; the usage report is then not reported.
  if (.client_side_stop(wire$adaptations)) return(stream(lm, request, on_event = function(event) invisible(NULL)))
  result <- .send_wire(lm, wire)
  out <- tryCatch(parse_response(lm, request, result$body), LM15Error = function(e) stop(.redact_wire_condition(e, wire)))
  visible <- .visible_adaptations(lm, wire$adaptations)
  if (length(visible) && !length(out$adaptations)) out["adaptations"] <- list(visible)
  out
}

str.lm15_wire_request <- function(object, ...) { print.lm15_wire_request(object); invisible(NULL) }
