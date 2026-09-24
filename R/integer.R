# R doubles cannot hold every large integer. Decimal values preserve exact
# counters without adding a native arithmetic dependency to browser builds.
.integer_digits <- function(x) {
  if (inherits(x, "lm15_integer")) return(unclass(x))
  if (inherits(x, "lm15_json_number")) return(.integer_normalize(unclass(x)))
  if (!is.numeric(x) || is.object(x) || length(x) != 1L || !is.finite(x) || x != trunc(x)) stop("Expected one finite integer.", call. = FALSE)
  sprintf("%.0f", x)
}
.integer_normalize <- function(token) {
  if (!grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?([eE][+-]?[0-9]+)?$", token)) stop("Invalid integer spelling.", call. = FALSE)
  negative <- startsWith(token, "-")
  pieces <- strsplit(tolower(sub("^-", "", token)), "e", fixed = TRUE)[[1L]]
  exponent <- if (length(pieces) == 2L) as.numeric(pieces[[2L]]) else 0
  mantissa <- strsplit(pieces[[1L]], ".", fixed = TRUE)[[1L]]
  fraction <- if (length(mantissa) == 2L) nchar(mantissa[[2L]]) else 0L
  digits <- sub("^0+", "", paste(mantissa, collapse = ""))
  if (!nzchar(digits)) return("0")
  shift <- exponent - fraction
  if (!is.finite(shift) || abs(shift) > 1000000) stop("Integer expansion exceeds one million digits.", call. = FALSE)
  if (shift < 0) {
    if (-shift >= nchar(digits) || grepl("[1-9]", substring(digits, nchar(digits) + shift + 1))) stop("Expected an integer, without rounding.", call. = FALSE)
    digits <- substr(digits, 1L, nchar(digits) + shift)
  } else if (shift > 0) digits <- paste0(digits, strrep("0", shift))
  paste0(if (negative) "-" else "", digits)
}
.integer_from_digits <- function(digits) {
  value <- suppressWarnings(as.numeric(digits))
  if (is.finite(value) && abs(value) <= .Machine$integer.max) return(as.integer(value))
  structure(digits, class = "lm15_integer")
}
integer_value <- function(decimal, ...) {
  .check_dots(...); .string(decimal, "decimal")
  digits <- .integer_normalize(decimal)
  value <- .integer_from_digits(digits)
  if (is.double(value)) structure(digits, class = "lm15_integer") else value
}
.integer_compare <- function(a, b) {
  negative_a <- startsWith(a, "-"); negative_b <- startsWith(b, "-")
  if (negative_a != negative_b) return(if (negative_a) -1L else 1L)
  a <- sub("^-", "", a); b <- sub("^-", "", b)
  result <- if (nchar(a) != nchar(b)) sign(nchar(a) - nchar(b)) else if (identical(a, b)) 0L else {
    av <- utf8ToInt(a); bv <- utf8ToInt(b); i <- which(av != bv)[[1L]]; sign(av[[i]] - bv[[i]])
  }
  if (negative_a) -result else result
}
.integer_add <- function(a, b) {
  a <- .integer_digits(a); b <- .integer_digits(b)
  negative_a <- startsWith(a, "-"); negative_b <- startsWith(b, "-")
  a <- sub("^-", "", a); b <- sub("^-", "", b)
  subtract <- negative_a != negative_b
  if (subtract && .integer_compare(a, b) < 0) { temp <- a; a <- b; b <- temp; negative_a <- negative_b }
  av <- rev(utf8ToInt(a) - 48L); bv <- rev(utf8ToInt(b) - 48L)
  n <- max(length(av), length(bv)); length(av) <- n; length(bv) <- n
  av[is.na(av)] <- 0L; bv[is.na(bv)] <- 0L
  out <- integer(n + 1L); carry <- 0L
  for (i in seq_len(n)) {
    value <- av[[i]] + (if (subtract) -bv[[i]] else bv[[i]]) + carry
    out[[i]] <- value %% 10L; carry <- value %/% 10L
  }
  out[[n + 1L]] <- carry
  digits <- sub("^0+", "", paste(rev(out), collapse = ""))
  if (!nzchar(digits)) return(0L)
  .integer_from_digits(paste0(if (negative_a) "-" else "", digits))
}
Ops.lm15_integer <- function(e1, e2) {
  a <- .integer_digits(e1)
  if (missing(e2)) {
    if (.Generic == "+") return(e1)
    if (.Generic == "-") return(.integer_from_digits(if (startsWith(a, "-")) substring(a, 2L) else paste0("-", a)))
    stop("Unsupported integer operation.", call. = FALSE)
  }
  b <- .integer_digits(e2)
  if (.Generic %in% c("==", "!=", "<", ">", "<=", ">=")) {
    cmp <- .integer_compare(a, b)
    return(switch(.Generic, `==` = cmp == 0, `!=` = cmp != 0, `<` = cmp < 0, `>` = cmp > 0, `<=` = cmp <= 0, `>=` = cmp >= 0))
  }
  if (.Generic == "+") return(.integer_add(e1, e2))
  if (.Generic == "-") return(.integer_add(e1, .integer_from_digits(if (startsWith(b, "-")) substring(b, 2L) else paste0("-", b))))
  stop("Large exact integers support addition, subtraction, and comparisons; use an arithmetic package for other operations.", call. = FALSE)
}
as.double.lm15_integer <- function(x, ...) {
  if (.integer_compare(sub("^-", "", unclass(x)), "9007199254740991") <= 0) return(as.numeric(unclass(x)))
  stop("This integer is outside R's exact numeric range. Use as.character() or as_json() to preserve it.", call. = FALSE)
}
as.character.lm15_integer <- function(x, ...) unclass(x)
print.lm15_integer <- function(x, ...) { cat(unclass(x), "\n", sep = ""); invisible(x) }
