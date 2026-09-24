# Native R extension points: application backends can implement these S3
# methods without inheriting a provider implementation or altering routing.
complete <- function(lm, request, ...) UseMethod("complete")
stream <- function(lm, request, on_event, ...) UseMethod("stream")
complete.lm15_lm <- function(lm, request, ...) .complete_impl(lm, request, ...)
complete.lm15_router <- function(lm, request, ...) .complete_impl(lm, request, ...)
complete.lm15_fake_lm <- function(lm, request, ...) { .check_dots(...); lm$complete(request) }
complete.default <- function(lm, request, ...) stop("No completion method is defined for this client class.", call. = FALSE)
stream.lm15_lm <- function(lm, request, on_event, ...) .stream_impl(lm, request, on_event, ...)
stream.lm15_router <- function(lm, request, on_event, ...) .stream_impl(lm, request, on_event, ...)
stream.lm15_fake_lm <- function(lm, request, on_event, ...) {
  .check_dots(...)
  if (!is.function(on_event)) stop("on_event must be a function.", call. = FALSE)
  lm$stream(request, on_event)
}
stream.default <- function(lm, request, on_event, ...) stop("No streaming method is defined for this client class.", call. = FALSE)
