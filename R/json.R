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

# A small recursive-descent reader preserves numeric tokens, arrays and
# objects. String escape correctness is delegated to jsonlite.
.json_decode <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) stop("JSON must be one string.", call. = FALSE)
  n <- nchar(text, type = "chars"); pos <- 1L
  fail <- function() stop(sprintf("Malformed JSON near character %d.", pos), call. = FALSE)
  peek <- function() if (pos <= n) substr(text, pos, pos) else ""
  skip <- function() while (pos <= n && peek() %in% c(" ", "\t", "\n", "\r")) pos <<- pos + 1L
  string <- function() {
    # Scan whole unescaped spans in native code. Per-character R scanning
    # makes provider media strings and large model catalogs prohibitively slow.
    rest <- substring(text, pos)
    match <- regexpr('^"(?:[^"\\\\\\x00-\\x1f]++|\\\\(?:["\\\\/bfnrt]|u[0-9A-Fa-f]{4}))*"', rest, perl = TRUE)
    if (match[[1L]] != 1L) fail()
    token <- regmatches(rest, match)
    pos <<- pos + nchar(token)
    tryCatch(jsonlite::fromJSON(token), error = function(e) fail())
  }
  value <- function(depth = 0L) {
    if (depth > 256L) stop("JSON nesting exceeds 256 levels.", call. = FALSE)
    skip(); ch <- peek()
    if (ch == '"') return(string())
    if (ch %in% c("{", "[")) {
      object <- ch == "{"; close <- if (object) "}" else "]"
      pos <<- pos + 1L; skip(); out <- list(); keys <- character()
      if (peek() == close) { pos <<- pos + 1L; return(if (object) .json_object() else .json_array()) }
      repeat {
        skip()
        if (object) {
          if (peek() != '"') fail()
          key <- string(); skip()
          if (key %in% keys || peek() != ":") fail()
          keys <- c(keys, key); pos <<- pos + 1L
        }
        item <- value(depth + 1L)
        out[length(out) + 1L] <- list(item)
        skip(); ch <- peek(); pos <<- pos + 1L
        if (ch == close) break
        if (ch != ",") fail()
      }
      if (object) { names(out) <- keys; .json_object(out) } else .json_array(out)
    } else {
      rest <- substr(text, pos, n)
      for (literal in c("true", "false", "null")) {
        if (startsWith(rest, literal)) {
          pos <<- pos + nchar(literal)
          return(switch(literal, true = TRUE, false = FALSE, null = NULL))
        }
      }
      match <- regexpr("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?", rest, perl = TRUE)
      if (match[[1]] != 1L) fail()
      token <- regmatches(rest, match); pos <<- pos + nchar(token)
      if (grepl("[.eE]", token) && !is.finite(suppressWarnings(as.numeric(token)))) fail()
      .json_number(token)
    }
  }
  out <- value(); skip(); if (pos <= n) fail()
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
