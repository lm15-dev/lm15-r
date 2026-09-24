.oauth_callback_reply <- function(target, expected_path, expected_state) {
  page <- function(status, message, result = NULL, failed = FALSE) list(status = status,
    headers = list("Content-Type" = "text/html; charset=utf-8", "Cache-Control" = "no-store", "Content-Security-Policy" = "default-src 'none'"),
    body = paste0("<!doctype html><meta charset='utf-8'><title>Sign in</title><p>", message, "</p>"), result = result, failed = failed)
  if (nchar(target, type = "bytes") > 8192L) return(page(414L, "Callback address is too long."))
  split <- regexpr("?", target, fixed = TRUE)[[1L]]
  path <- if (split < 0L) target else substr(target, 1L, split - 1L)
  if (!identical(path, expected_path)) return(page(404L, "Callback route not found."))
  query <- if (split < 0L) "" else substring(target, split + 1L)
  params <- list()
  decode <- function(value) tryCatch(suppressWarnings(utils::URLdecode(gsub("+", " ", value, fixed = TRUE))), error = function(e) NA_character_)
  for (pair in strsplit(query, "&", fixed = TRUE)[[1L]]) {
    i <- regexpr("=", pair, fixed = TRUE)[[1L]]
    key <- decode(if (i < 0L) pair else substr(pair, 1L, i - 1L))
    if (is.na(key)) return(page(400L, "Malformed callback parameter."))
    if (!nzchar(key)) next
    if (key %in% names(params)) return(page(400L, "Duplicate callback parameter."))
    value <- decode(if (i < 0L) "" else substring(pair, i + 1L))
    if (is.na(value)) return(page(400L, "Malformed callback parameter."))
    params[[key]] <- value
  }
  if (nzchar(params$error %||% "")) return(page(400L, "Authorization was not completed.", failed = TRUE))
  if (!is.null(expected_state) && !identical(params$state, expected_state)) return(page(400L, "State mismatch."))
  if (!nzchar(params$code %||% "")) return(page(400L, "Missing authorization code."))
  result <- structure(list(code = params$code, state = params$state), class = "lm15_oauth_callback_result")
  page(200L, "Authentication completed. You can close this window.", result)
}
print.lm15_oauth_callback_result <- function(x, ...) { cat("<lm15 authorization callback: redacted>\n"); invisible(x) }
str.lm15_oauth_callback_result <- function(object, ...) { print.lm15_oauth_callback_result(object); invisible(NULL) }

oauth_callback_listener <- function(..., expected_state = NULL, host = "127.0.0.1", port = 0L, path = "/callback") {
  .check_dots(...)
  if (!identical(host, "127.0.0.1")) stop("OAuth callbacks bind only to 127.0.0.1.", call. = FALSE)
  if (!is.null(expected_state)) .string(expected_state, "expected_state")
  .string(path, "path")
  if (!startsWith(path, "/") || grepl("[?#\r\n]", path)) stop("Callback path must be an absolute path without a query or fragment.", call. = FALSE)
  port <- .number(port, "port", TRUE)
  if (port < 0L || port > 65535L) stop("Port must be between 0 and 65535.", call. = FALSE)
  if (!requireNamespace("httpuv", quietly = TRUE) || !requireNamespace("later", quietly = TRUE)) .abort("OAuth callbacks require the httpuv and later R packages.", "not_configured")
  result <- NULL; failed <- FALSE; closed <- FALSE
  app <- list(call = function(request) {
    if (!identical(request$REQUEST_METHOD, "GET")) return(list(status = 405L, headers = list("Allow" = "GET", "Cache-Control" = "no-store"), body = "Use GET."))
    query <- sub("^\\?", "", request$QUERY_STRING %||% "")
    target <- paste0(request$PATH_INFO, if (nzchar(query)) paste0("?", query) else "")
    reply <- .oauth_callback_reply(target, path, expected_state)
    if (is.null(result) && !failed) { result <<- reply$result; failed <<- reply$failed }
    reply[c("status", "headers", "body")]
  })
  server <- NULL
  for (attempt in seq_len(if (port == 0L) 20L else 1L)) {
    chosen <- if (port == 0L) httpuv::randomPort(host = host) else port
    server <- tryCatch(httpuv::startServer(host, chosen, app, quiet = TRUE), error = function(e) NULL)
    if (!is.null(server)) break
  }
  if (is.null(server)) .abort("Cannot bind the local OAuth callback listener.", "transport")
  close <- function() { if (!closed) { closed <<- TRUE; httpuv::stopServer(server) }; invisible(NULL) }
  wait <- function(timeout = 300) {
    timeout <- .number(timeout, "timeout")
    if (timeout < 0) stop("timeout must be non-negative.", call. = FALSE)
    deadline <- proc.time()[["elapsed"]] + timeout
    while (is.null(result) && !failed && !closed) {
      remaining <- deadline - proc.time()[["elapsed"]]
      if (remaining <= 0) .abort("Timed out waiting for the OAuth redirect.", "timeout")
      later::run_now(timeoutSecs = min(remaining, 0.05))
    }
    if (failed) .abort("Authorization was not completed; provider diagnostics are suppressed.", "auth")
    if (is.null(result)) .abort("OAuth callback listener was closed before authorization.", "transport")
    result
  }
  structure(list(redirect_uri = paste0("http://", host, ":", chosen, path), wait = wait, close = close), class = "lm15_oauth_callback_listener")
}
print.lm15_oauth_callback_listener <- function(x, ...) { cat("<lm15 loopback OAuth listener>\n"); invisible(x) }
str.lm15_oauth_callback_listener <- function(object, ...) { print.lm15_oauth_callback_listener(object); invisible(NULL) }
