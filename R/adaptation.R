# MAP-13 — adapt freely, never invisibly; refuse only when a guess could hurt.
#
# When a wire cannot take a setting as asked, the adapter does the obvious
# thing and records it: the record rides the Response and the first stream
# event, and plan() previews it with no network. Translations (stop becoming
# stop_sequences, an effort word becoming a budget) are the adapter's ordinary
# job and are never recorded. The policy is set on the client and the router:
#   "note" (default): adapt and record.
#   "silent": adapt the same way; the response carries no record.
#   "refuse": every deviation (dropped, clamped, substituted, client_side) is an
#             UnsupportedFeatureError before the wire, naming the config path.
# satisfied and defaulted change nothing the caller asked for and are recorded
# under every policy but "silent". Nothing here prints.

.adaptation_policies <- c("note", "silent", "refuse")
.deviations <- c("dropped", "clamped", "substituted", "client_side")
.adapt_state <- new.env(parent = emptyenv())
.adapt_state$scope <- NULL

.check_policy <- function(policy) {
  if (!is.character(policy) || length(policy) != 1L || is.na(policy) || !policy %in% .adaptation_policies)
    stop('adaptations must be one of "note", "silent", or "refuse".', call. = FALSE)
  policy
}

adaptation <- function(field, action, reason, ..., asked = NULL, applied = NULL) {
  .check_dots(...)
  .new_value("Adaptation", list(field = field, action = action, reason = reason, asked = asked, applied = applied))
}

# One scope per request build; adapt() inside a builder appends to it. A
# builder called with no scope open adapts under "note" and keeps no record.
.collecting <- function(policy, provider, build) {
  previous <- .adapt_state$scope
  scope <- new.env(parent = emptyenv())
  scope$policy <- .check_policy(policy); scope$provider <- provider; scope$records <- list()
  .adapt_state$scope <- scope
  on.exit(.adapt_state$scope <- previous, add = TRUE)
  value <- build()
  list(value = value, records = scope$records)
}

.adapt <- function(field, action, reason, asked = NULL, applied = NULL, provider = NULL) {
  scope <- .adapt_state$scope
  policy <- if (is.null(scope)) "note" else scope$policy
  who <- provider %||% if (!is.null(scope)) scope$provider
  if (policy == "refuse" && action %in% .deviations) {
    verb <- c(dropped = "would be dropped", clamped = "would be clamped", substituted = "would be substituted", client_side = "would be applied client-side")[[action]]
    stop(lm15_error(paste0(if (!is.null(who)) paste0(who, ": "), field, " ", verb, ": ", reason, ' (adaptations = "refuse")'),
      code = "unsupported_feature", provider = who, feature = field))
  }
  if (!is.null(scope)) scope$records[[length(scope$records) + 1L]] <- adaptation(field, action, reason, asked = asked, applied = applied)
  invisible(NULL)
}

# What a call with this request would adapt, with no network and no
# credential read. Raises what the call would raise. Returns the full record
# under every policy, "silent" included: a preview that hid what it saw would
# be no preview.
plan <- function(lm, request, ..., stream = FALSE) {
  .check_dots(...)
  if (inherits(lm, "lm15_router")) {
    # Like resolve(), plan() is offline: a route with no key still plans. The
    # stand-in credential is never read, because nothing is sent.
    request <- validate(request)
    resolution <- resolve(lm, request$model)
    router <- lm
    lm <- tryCatch(.router_lm(router, resolution), NotConfiguredError = function(e)
      new_lm(resolution$provider, api_key = "unused: plan() sends nothing", env = router$env %||% Sys.getenv(), transport = function(...) stop("plan() sends nothing"), adaptations = router$adaptations %||% "note"))
    request$model <- resolution$model
  }
  pair <- .route(lm, request); lm <- pair$lm; req <- pair$request
  .collecting(lm$adaptations %||% "note", lm$definition$id, function() .build_payload(lm, req, isTRUE(stream)))$records
}

.visible_adaptations <- function(lm, records) if (identical(lm$adaptations, "silent")) list() else records
.client_side_stop <- function(records) any(vapply(records, function(a) a$field == "config.stop" && a$action == "client_side", logical(1)))

# The effort ladder, and the nearest declared level to an asked one. A tie
# goes to the lower level: the cheaper guess.
.effort_ladder <- c("minimal", "low", "medium", "high", "xhigh", "max")
.nearest_effort <- function(asked, available) {
  levels <- intersect(unlist(available), .effort_ladder)
  if (!length(levels)) stop("No comparable effort levels.", call. = FALSE)
  if (asked %in% levels) return(asked)
  want <- match(asked, .effort_ladder, nomatch = 1L)
  at <- match(levels, .effort_ladder)
  levels[order(abs(at - want), at)][[1L]]
}

# A client-side stop: text deltas form one stream; text that could still be
# the start of a stop sequence is held back, and the event holding a match is
# cut there. Nothing after the cut is delivered.
.first_stop <- function(text, stop) {
  best <- NULL
  for (s in stop) {
    at <- regexpr(s, text, fixed = TRUE)[[1L]]
    if (at > 0L && (is.null(best) || at < best)) best <- at
  }
  best
}
.new_stop_cutter <- function(stop) {
  stop <- Filter(nzchar, unlist(stop))
  hold <- max(c(nchar(stop), 1L)) - 1L
  segments <- list(); cut <- FALSE
  is_text <- function(e) identical(e$type, "delta") && identical(e$delta$type, "text")
  take <- function(count, cutting = FALSE) {
    out <- list()
    while (length(segments)) {
      e <- segments[[1L]]
      if (!is_text(e)) { out[[length(out) + 1L]] <- e; segments <<- segments[-1L]; next }
      t <- e$delta$text
      if (cutting && count == 0L) break
      if (nchar(t) <= count) { out[[length(out) + 1L]] <- e; segments <<- segments[-1L]; count <- count - nchar(t) }
      else if (cutting) {
        # A cut delta keeps no token scores: a score never describes a fragment.
        d <- e$delta; d$text <- substr(t, 1L, count); d$logprobs <- list()
        e$delta <- d; out[[length(out) + 1L]] <- e
        break
      } else break
    }
    out
  }
  feed <- function(e) {
    if (!is_text(e)) {
      if (!length(segments)) return(list(e))
      segments[[length(segments) + 1L]] <<- e
      return(list())
    }
    segments[[length(segments) + 1L]] <<- e
    buffered <- paste(vapply(Filter(is_text, segments), function(x) x$delta$text, ""), collapse = "")
    hit <- .first_stop(buffered, stop)
    if (!is.null(hit)) {
      out <- take(hit - 1L, cutting = TRUE)
      segments <<- list(); cut <<- TRUE
      return(out)
    }
    take(max(0L, nchar(buffered) - hold))
  }
  flush <- function() { out <- segments; segments <<- list(); out }
  list(active = length(stop) > 0L, feed = feed, flush = flush, is_cut = function() cut)
}
# Signalled from inside the transport's chunk callback to close the source at
# the cut; transports pass it through rather than report a network failure.
.stream_cut <- function() stop(structure(class = c("lm15_stream_cut", "error", "condition"), list(message = "client-side stop", call = NULL)))
