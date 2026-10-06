# JSON containers stay distinct, including when empty. Numeric wire tokens
# retain their spelling until a typed field deliberately interprets them.
json_object <- function(...) {
  x <- list(...)
  if (length(x) && (is.null(names(x)) || anyNA(names(x)) || any(!nzchar(names(x))) || anyDuplicated(names(x))))
    stop("JSON objects require unique, non-empty names.", call. = FALSE)
  structure(x, class = c("lm15_json_object", "list"))
}

json_array <- function(...) structure(unname(list(...)), class = c("lm15_json_array", "list"))

.json_object <- function(x = list()) {
  if (!is.list(x)) stop("Expected a JSON object.", call. = FALSE)
  if (!length(x)) names(x) <- character()
  structure(x, class = c("lm15_json_object", "list"))
}
.json_array <- function(x = list()) structure(unname(x), class = c("lm15_json_array", "list"))
.is_object <- function(x) inherits(x, "lm15_json_object") ||
  (is.list(x) && !inherits(x, "lm15_json_array") && !is.null(names(x)))
.is_array <- function(x) is.list(x) && !.is_object(x) && !inherits(x, "lm15_value")
`%||%` <- function(x, y) if (is.null(x)) y else x
.check_dots <- function(...) {
  if (length(list(...))) stop("Unused arguments; check their names.", call. = FALSE)
}

`$.lm15_json_object` <- function(x, name) unclass(x)[[name, exact = TRUE]]

.base64_encode <- function(bytes) gsub("[\r\n]", "", jsonlite::base64_enc(bytes))
.json_number <- function(x) structure(x, class = "lm15_json_number")
.json_string <- function(x) {
  # JSON permits escaped slashes, but signed request bodies require the
  # reference's exact spelling (not jsonlite's HTML-safe <\/ convention).
  gsub("\\/", "/", as.character(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE)), fixed = TRUE)
}

# jsonlite is used only for string escaping here. Its vector simplification
# and numeric conversion must not alter opaque JSON objects.
.json_encode <- function(x, depth = 0L) {
  if (depth > 256L) stop("JSON nesting exceeds 256 levels.", call. = FALSE)
  if (inherits(x, "lm15_value")) x <- as_dict(x)
  if (is.null(x)) return("null")
  if (inherits(x, "lm15_json_number") || inherits(x, "lm15_integer")) return(unclass(x))
  if (.is_object(x)) {
    n <- names(x)
    if (length(x) && (anyNA(n) || anyDuplicated(n))) stop("Invalid JSON object names.", call. = FALSE)
    entries <- vapply(seq_along(x), function(i) paste0(.json_string(n[[i]]), ":", .json_encode(x[[i]], depth + 1L)), "")
    return(paste0("{", paste(entries, collapse = ","), "}"))
  }
  if (.is_array(x)) return(paste0("[", paste(vapply(x, .json_encode, "", depth = depth + 1L), collapse = ","), "]"))
  if (length(x) != 1L || is.na(x)) stop("JSON scalars must have length one and cannot be NA; use json_array() for arrays.", call. = FALSE)
  if (is.object(x)) stop("Unsupported object in JSON payload.", call. = FALSE)
  if (is.character(x)) return(.json_string(x))
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.integer(x)) return(as.character(x))
  if (is.double(x) && is.finite(x)) {
    # Use the shortest decimal that restores exactly the same R double.
    # sprintf is locale-sensitive; JSON always uses a decimal point.
    out <- NULL
    for (digits in seq_len(17L)) {
      candidate <- chartr(",", ".", sprintf(paste0("%.", digits, "g"), x))
      if (identical(as.numeric(candidate), x)) { out <- candidate; break }
    }
    if (is.null(out)) stop("Cannot encode a finite number exactly.", call. = FALSE)
    if (!grepl("[.eE]", out)) out <- paste0(out, ".0")
    return(out)
  }
  stop("Only finite JSON values are supported.", call. = FALSE)
}

# Fails exactly where .json_encode fails, without building the text. A field
# that only has to be encodable (a provider's reply object, an opaque value)
# is checked with this: such an object can carry a 30 MB image (INV-056),
# and encoding it only to discard the result cost seconds per check.
.json_check <- function(x, depth = 0L) {
  if (depth > 256L) stop("JSON nesting exceeds 256 levels.", call. = FALSE)
  if (inherits(x, "lm15_value")) x <- as_dict(x)
  if (is.null(x) || inherits(x, "lm15_json_number") || inherits(x, "lm15_integer")) return(invisible(TRUE))
  if (.is_object(x)) {
    n <- names(x)
    if (length(x) && (anyNA(n) || anyDuplicated(n))) stop("Invalid JSON object names.", call. = FALSE)
    for (i in seq_along(x)) .json_check(x[[i]], depth + 1L)
    return(invisible(TRUE))
  }
  if (.is_array(x)) {
    for (i in seq_along(x)) .json_check(x[[i]], depth + 1L)
    return(invisible(TRUE))
  }
  if (length(x) != 1L || is.na(x)) stop("JSON scalars must have length one and cannot be NA; use json_array() for arrays.", call. = FALSE)
  if (is.object(x)) stop("Unsupported object in JSON payload.", call. = FALSE)
  if (is.character(x) || is.logical(x) || is.integer(x) || (is.double(x) && is.finite(x))) return(invisible(TRUE))
  stop("Only finite JSON values are supported.", call. = FALSE)
}

# A recursive-descent reader that preserves numeric tokens, arrays and
# objects. One regular-expression pass splits the text into tokens (linear
# in its size; a provider's model catalog can be several hundred KB), and
# every string holding an escape is decoded by jsonlite in one vectorized
# call. Anything between tokens that is not a token is malformed JSON.
.json_token_pattern <- paste0(
  '"(?:[^"\\\\\\x00-\\x1f]++|\\\\(?:["\\\\/bfnrt]|u[0-9A-Fa-f]{4}))*+"',
  "|-?(?:0|[1-9][0-9]*+)(?:\\.[0-9]++)?(?:[eE][+-]?[0-9]++)?",
  "|true|false|null|[{}\\[\\]:,]|[ \\t\\n\\r]++")
.json_decode <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) stop("JSON must be one string.", call. = FALSE)
  fail <- function(at) stop(sprintf("Malformed JSON near byte %d.", at), call. = FALSE)
  text <- enc2utf8(text)
  if (!validUTF8(text)) stop("JSON must be valid UTF-8 text.", call. = FALSE)
  # Offsets are bytes: R maps character offsets in non-ASCII text with a cost
  # that grows with the square of its length.
  Encoding(text) <- "bytes"
  n <- nchar(text, type = "bytes")
  if (!n) fail(1L)
  found <- tryCatch(gregexpr(.json_token_pattern, text, perl = TRUE, useBytes = TRUE)[[1L]], error = function(e) fail(1L))
  if (found[[1L]] < 0L) fail(1L)
  width <- attr(found, "match.length")
  ends <- found + width
  # Tokens must tile the whole text: a gap is a character no token accepts.
  gap <- which(found != c(1L, ends[-length(ends)]))
  if (length(gap)) fail(if (gap[[1L]] == 1L) 1L else ends[[gap[[1L]] - 1L]])
  if (ends[[length(ends)]] != n + 1L) fail(ends[[length(ends)]])
  tokens <- regmatches(text, list(found))[[1L]]
  Encoding(tokens) <- "UTF-8"
  keep <- !grepl("^[ \t\n\r]", tokens)
  tokens <- tokens[keep]; where <- found[keep]
  if (!length(tokens)) fail(1L)
  first <- substr(tokens, 1L, 1L)
  strings <- which(first == '"')
  values <- vector("list", length(tokens))
  if (length(strings)) {
    body <- substr(tokens[strings], 2L, nchar(tokens[strings]) - 1L)
    escaped <- grepl("\\", body, fixed = TRUE)
    decoded <- body
    if (any(escaped)) {
      decoded[escaped] <- tryCatch(as.character(jsonlite::fromJSON(paste0("[", paste(tokens[strings][escaped], collapse = ","), "]"))), error = function(e) fail(where[strings[escaped][[1L]]]))
      if (length(decoded[escaped]) != sum(escaped)) fail(where[strings[escaped][[1L]]])
    }
    values[strings] <- as.list(decoded)
  }
  count <- length(tokens); pos <- 1L
  value <- function(depth) {
    if (depth > 256L) stop("JSON nesting exceeds 256 levels.", call. = FALSE)
    if (pos > count) fail(n)
    token <- tokens[[pos]]; ch <- first[[pos]]
    if (ch == '"') { pos <<- pos + 1L; return(values[[pos - 1L]]) }
    if (ch == "{" || ch == "[") {
      object <- ch == "{"; close <- if (object) "}" else "]"
      pos <<- pos + 1L
      if (pos <= count && tokens[[pos]] == close) { pos <<- pos + 1L; return(if (object) .json_object() else .json_array()) }
      out <- vector("list", 8L); keys <- character(8L); size <- 0L
      repeat {
        if (object) {
          if (pos > count || first[[pos]] != '"') fail(if (pos > count) n else where[[pos]])
          key <- values[[pos]]; pos <<- pos + 1L
          if (pos > count || tokens[[pos]] != ":") fail(if (pos > count) n else where[[pos]])
          pos <<- pos + 1L
        }
        item <- value(depth + 1L)
        size <- size + 1L
        if (size > length(out)) { length(out) <- 2L * length(out); length(keys) <- length(out) }
        if (!is.null(item)) out[[size]] <- item
        if (object) keys[[size]] <- key
        if (pos > count) fail(n)
        sep <- tokens[[pos]]; pos <<- pos + 1L
        if (sep == close) break
        if (sep != ",") fail(where[[pos - 1L]])
      }
      out <- out[seq_len(size)]
      if (object) {
        keys <- keys[seq_len(size)]
        if (anyDuplicated(keys)) fail(where[[pos - 1L]])
        names(out) <- keys
        return(.json_object(out))
      }
      return(.json_array(out))
    }
    pos <<- pos + 1L
    if (token == "true") return(TRUE)
    if (token == "false") return(FALSE)
    if (token == "null") return(NULL)
    if (ch == "-" || grepl("^[0-9]", ch)) {
      if (grepl("[.eE]", token) && !is.finite(suppressWarnings(as.numeric(token)))) fail(where[[pos - 1L]])
      return(.json_number(token))
    }
    fail(where[[pos - 1L]])
  }
  out <- value(0L)
  if (pos <= count) fail(where[[pos]])
  out
}

as_json <- function(x, ..., include_provider_data = FALSE) {
  .check_dots(...)
  .json_encode(if (inherits(x, "lm15_value")) as_dict(x, include_provider_data = include_provider_data) else x)
}

from_json <- function(text, kind, ...) {
  .check_dots(...)
  from_dict(.json_decode(text), kind)
}

.empty <- function(x) is.null(x) || (is.character(x) && length(x) == 1L && !nzchar(x)) ||
  ((is.list(x) || is.atomic(x)) && !length(x))

# Python's legacy scalar-to-text leniency is explicit, never used for a
# request adapter's media conversion.
.scalar_text <- function(x) {
  if (is.null(x)) return("None")
  if (is.logical(x) && length(x) == 1L) return(if (x) "True" else "False")
  if (inherits(x, "lm15_json_number")) return(unclass(x))
  if (is.character(x) && length(x) == 1L) return(x)
  .json_encode(x)
}
