fake_lm <- function(responses, ...) {
  .check_dots(...)
  if (inherits(responses, "lm15_Response") || inherits(responses, "condition")) responses <- list(responses)
  if (is.character(responses)) responses <- as.list(responses)
  if (!is.list(responses)) stop("responses must be a list of responses, text, or errors.", call. = FALSE)
  queue <- lapply(responses, function(value) {
    if (is.character(value)) return(response("fake", message_assistant(value), "stop"))
    if (inherits(value, "condition")) return(value)
    value <- validate(value)
    if (!inherits(value, "lm15_Response")) stop("Fake result must be a response, text, or error.", call. = FALSE)
    value
  })
  requests <- list(); position <- 0L
  next_response <- function(request) {
    request <- validate(request)
    if (!inherits(request, "lm15_Request")) stop("Expected request().", call. = FALSE)
    requests[[length(requests) + 1L]] <<- request
    if (position >= length(queue)) stop("Fake model has no remaining responses.", call. = FALSE)
    position <<- position + 1L; value <- queue[[position]]
    if (inherits(value, "condition")) stop(value)
    value
  }
  structure(list(complete = next_response, stream = function(request, on_event) {
    value <- next_response(request); response_to_events(value, on_event = on_event); value
  }, requests = function() requests, remaining = function() length(queue) - position), class = "lm15_fake_lm")
}
print.lm15_fake_lm <- function(x, ...) { cat("<lm15 fake model: ", x$remaining(), " results remaining>\n", sep = ""); invisible(x) }
str.lm15_fake_lm <- function(object, ...) { print.lm15_fake_lm(object); invisible(NULL) }

fake_transport <- function(responses, ...) {
  .check_dots(...)
  if (!is.list(responses)) stop("responses must be a list of HTTP replies.", call. = FALSE)
  position <- 0L; requests <- list()
  send <- function(wire, on_chunk = NULL) {
    requests[[length(requests) + 1L]] <<- wire
    if (position >= length(responses)) stop("Fake transport has no remaining responses.", call. = FALSE)
    position <<- position + 1L; reply <- responses[[position]]
    if (inherits(reply, "condition")) stop(reply)
    status <- .number(reply$status %||% 200L, "status", TRUE)
    if (status < 100L || status > 599L) stop("Invalid fake HTTP status.", call. = FALSE)
    chunks <- reply$chunks %||% list(reply$body %||% raw())
    chunks <- lapply(chunks, function(chunk) {
      if (is.character(chunk)) chunk <- charToRaw(enc2utf8(chunk))
      if (!is.raw(chunk)) stop("Fake body chunks must be raw bytes or text.", call. = FALSE)
      chunk
    })
    if (!is.null(on_chunk) && status < 300L) {
      for (chunk in chunks) on_chunk(chunk)
      body <- raw()
    } else body <- if (length(chunks)) do.call(c, chunks) else raw()
    list(status = status, headers = reply$headers %||% list(), body = body)
  }
  structure(send, class = c("lm15_fake_transport", "lm15_transport", "function"), requests = function() requests)
}
print.lm15_fake_transport <- function(x, ...) { cat("<lm15 fake HTTP transport; recorded wire values hidden>\n"); invisible(x) }
str.lm15_fake_transport <- function(object, ...) { print.lm15_fake_transport(object); invisible(NULL) }
recorded_requests <- function(x, ...) {
  .check_dots(...)
  if (inherits(x, "lm15_fake_lm")) return(x$requests())
  if (inherits(x, "lm15_fake_transport")) return(attr(x, "requests")())
  stop("Expected fake_lm() or fake_transport().", call. = FALSE)
}
