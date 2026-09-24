.sum_turn_usage <- function(acc, more) {
  if (is.null(acc)) return(more)
  fields <- lapply(names(.schemas$Usage), function(name) {
    if (is.null(acc[[name]]) || is.null(more[[name]])) NULL else .integer_add(acc[[name]], more[[name]])
  })
  names(fields) <- names(.schemas$Usage)
  .new_value("Usage", fields)
}
materialize_turn <- function(events, ...) {
  .check_dots(...)
  words <- character(); audio <- list(); mime <- NULL; calls <- list(); tokens <- NULL; error <- NULL
  for (event in events) {
    event <- validate(event)
    if (!inherits(event, "lm15_LiveServerEvent")) stop("Expected live server events.", call. = FALSE)
    if (event$type == "text") words <- c(words, event$text)
    if (event$type == "audio") {
      if (!is.null(mime) && !is.null(event$media_type) && mime != event$media_type) stop("A turn cannot concatenate different audio media types; consume raw events instead.", call. = FALSE)
      mime <- mime %||% event$media_type
      audio[[length(audio) + 1L]] <- media_bytes(audio_part(data = event$data, media_type = event$media_type %||% "audio/pcm"))
    }
    if (event$type == "tool_call") calls[[length(calls) + 1L]] <- tool_call_info(event$id, event$name, input = event$input)
    if (event$type %in% c("usage", "turn_end")) tokens <- .sum_turn_usage(tokens, event$usage)
    if (event$type == "error") error <- event$error
  }
  last <- if (length(events)) events[[length(events)]]$type else ""
  ending <- if (last %in% c("turn_end", "interrupted", "error", "tool_call")) last else "incomplete"
  structure(list(ended_by = ending, ok = ending == "turn_end", text = paste0(words, collapse = ""),
    audio = if (length(audio)) do.call(c, audio) else raw(), audio_media_type = mime,
    tool_calls = calls, usage = tokens, error = error, events = events), class = "lm15_turn")
}
print.lm15_turn <- function(x, ...) { cat("<lm15 turn: ", x$ended_by, "; ", length(x$events), " events>\n", sep = ""); invisible(x) }
str.lm15_turn <- function(object, ...) { print.lm15_turn(object); invisible(NULL) }

turn <- function(session, ..., max_events = 1024L, max_bytes = 128 * 1024^2, timeout = 120) {
  .check_dots(...)
  if (!inherits(session, "lm15_live_session")) stop("Expected a live session.", call. = FALSE)
  .number(max_events, "max_events", TRUE); .number(max_bytes, "max_bytes", TRUE); .number(timeout, "timeout")
  if (max_events <= 0 || max_bytes <= 0 || timeout <= 0) stop("Turn limits must be positive.", call. = FALSE)
  events <- list(); done <- FALSE; reading <- FALSE; failure <- NULL; sealed <- NULL; collected_bytes <- 0
  snapshot <- function() materialize_turn(events)
  next_event <- function() {
    if (reading) stop("A turn already has an active reader.", call. = FALSE)
    if (!is.null(failure)) stop(failure)
    if (done) return(NULL)
    reading <<- TRUE; on.exit(reading <<- FALSE, add = TRUE)
    tryCatch({
      if (length(events) >= max_events) .abort("Turn exceeds its collection limit; consume session events directly.", "transport")
      event <- session$next_event(wait = timeout)
      if (is.null(event)) .abort("Live session closed before the turn reached a boundary.", "transport")
      size <- as.double(utils::object.size(event))
      if (collected_bytes + size > max_bytes) .abort("Turn exceeds its memory limit; consume session events directly.", "transport")
      collected_bytes <<- collected_bytes + size
      events[[length(events) + 1L]] <<- event
      if (event$type %in% c("turn_end", "interrupted", "error")) done <<- TRUE
      event
    }, error = function(e) { failure <<- e; stop(e) })
  }
  result <- function() {
    if (!is.null(failure)) stop(failure)
    if (!is.null(sealed)) return(sealed)
    last_tool <- length(events) && events[[length(events)]]$type == "tool_call"
    if (!last_tool) while (!done) {
      event <- next_event()
      if (is.null(event) || event$type == "tool_call") break
    }
    value <- snapshot()
    if (value$ended_by == "incomplete") .abort("Turn view closed before reaching a boundary; inspect snapshot().", "transport")
    done <<- TRUE; sealed <<- value; value
  }
  close <- function() {
    if (reading) stop("Stop the active reader before closing its turn view.", call. = FALSE)
    done <<- TRUE; invisible(NULL)
  }
  structure(list(next_event = next_event, snapshot = snapshot, result = result, close = close), class = "lm15_turn_view")
}
print.lm15_turn_view <- function(x, ...) { cat("<lm15 live turn view>\n"); invisible(x) }
str.lm15_turn_view <- function(object, ...) { print.lm15_turn_view(object); invisible(NULL) }
