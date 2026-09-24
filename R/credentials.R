select_auth_scheme <- function(credential, schemes, provider = NULL) {
  credential <- validate(credential)
  if (!inherits(credential, "lm15_Credential")) stop("Expected a credential value.", call. = FALSE)
  accepted <- switch(credential$kind, api_key = c("bearer", "x-api-key", "api-key", "query-key"), bearer_token = c("bearer", "x-api-key"), aws = "sigv4")
  order <- if (credential$kind == "bearer_token") accepted[accepted %in% schemes] else schemes[schemes %in% accepted]
  if (!length(order)) .abort("Credential kind is incompatible with this access policy.", "not_configured", provider)
  scheme <- order[[1L]]
  if (credential$kind == "api_key" && scheme %in% c("x-api-key", "api-key") && "bearer" %in% schemes) {
    segments <- strsplit(credential$value, ".", fixed = TRUE)[[1L]]
    if (length(segments) == 3L) {
      head <- tryCatch(.json_decode(rawToChar(.base64url_decode(segments[[1L]]))), error = function(e) NULL)
      if (.is_object(head) && !is.null(head$alg)) .abort("This looks like a bearer token; wrap it in bearer_token() instead of sending it as an API key.", "not_configured", provider)
    }
  }
  scheme
}
credential_expired <- function(credential, ..., now = Sys.time(), skew_seconds = 300) {
  .check_dots(...); credential <- validate(credential)
  if (credential$kind == "api_key" || is.null(credential$expires_at)) return(FALSE)
  expiry <- .parse_rfc3339(credential$expires_at)
  as.numeric(difftime(expiry, now, units = "secs")) <= skew_seconds
}
.parse_rfc3339 <- function(x) {
  .string(x, "expires_at")
  value <- sub("[Zz]$", "+0000", x)
  value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
  out <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"))
  if (length(out) != 1L || is.na(out)) stop("Timestamp must be RFC 3339 with an explicit timezone.", call. = FALSE)
  out
}
.crypto <- function() {
  if (!requireNamespace("openssl", quietly = TRUE)) .abort("This operation requires the openssl R package; no custom cryptography is substituted.", "not_configured")
}
.base64url <- function(bytes) sub("=+$", "", chartr("+/", "-_", .base64_encode(bytes)))
.base64url_decode <- function(text) jsonlite::base64_dec(paste0(chartr("-_", "+/", text), strrep("=", (4L - nchar(text) %% 4L) %% 4L)))
.hex_hash <- function(bytes) { .crypto(); as.character(openssl::sha256(bytes)) }
.hmac <- function(key, text) { .crypto(); unclass(openssl::sha256(charToRaw(enc2utf8(text)), key = key)) }

sigv4_sign <- function(method, url, credential, ..., region, service, headers = list(), body = raw(), now = Sys.time()) {
  .check_dots(...); .crypto()
  credential <- validate(credential)
  if (!inherits(credential, "lm15_AwsCredentials")) stop("sigv4_sign needs aws_credentials().", call. = FALSE)
  .string(region, "region"); .string(service, "service"); .string(method, "method"); .string(url, "url")
  if (is.character(body)) body <- charToRaw(enc2utf8(body))
  if (!is.raw(body)) stop("Signing body must be raw bytes or a string.", call. = FALSE)
  match <- regmatches(url, regexec("^https?://([^/?#]+)([^?#]*)(?:\\?([^#]*))?$", url, perl = TRUE))[[1L]]
  if (length(match) < 3L || grepl("@", match[[2L]], fixed = TRUE)) stop("Signing URL must be HTTP(S), without userinfo or fragment.", call. = FALSE)
  authority <- match[[2L]]; path <- match[[3L]]; query <- if (length(match) >= 4L) match[[4L]] else ""
  kept <- character()
  for (segment in strsplit(path, "/", fixed = TRUE)[[1L]]) {
    if (segment == "..") { if (length(kept)) kept <- kept[-length(kept)] }
    else if (nzchar(segment) && segment != ".") kept <- c(kept, segment)
  }
  normalized <- paste0("/", paste(vapply(kept, function(s) .path_id(utils::URLdecode(s)), ""), collapse = "/"), if (endsWith(path, "/") && length(kept)) "/" else "")
  query_pairs <- list()
  for (pair in if (nzchar(query)) strsplit(query, "&", fixed = TRUE)[[1L]] else character()) {
    eq <- regexpr("=", pair, fixed = TRUE)[[1L]]
    key <- if (eq < 0L) pair else substr(pair, 1L, eq - 1L)
    value <- if (eq < 0L) "" else substring(pair, eq + 1L)
    decode <- function(s) utils::URLdecode(gsub("+", " ", s, fixed = TRUE))
    query_pairs[[length(query_pairs) + 1L]] <- c(.path_id(decode(key)), .path_id(decode(value)))
  }
  canonical_query <- ""
  if (length(query_pairs)) {
    keys <- vapply(query_pairs, `[[`, "", 1L); values <- vapply(query_pairs, `[[`, "", 2L)
    sorted <- order(keys, values, method = "radix")
    canonical_query <- paste(paste0(keys[sorted], "=", values[sorted]), collapse = "&")
  }
  trim <- function(s) gsub("[[:space:]]+", " ", trimws(s))
  hdr <- list()
  for (name in names(headers)) {
    low <- tolower(name); if (low == "authorization") next
    value <- paste(vapply(as.list(headers[[name]]), trim, ""), collapse = ",")
    if (!is.null(hdr[[low]])) value <- paste(hdr[[low]], value, sep = ",")
    hdr[[low]] <- value
  }
  hdr$host <- hdr$host %||% authority
  hdr[["x-amz-date"]] <- hdr[["x-amz-date"]] %||% format(now, "%Y%m%dT%H%M%SZ", tz = "UTC")
  if (!is.null(credential$session_token)) hdr[["x-amz-security-token"]] <- credential$session_token
  names_sorted <- sort(names(hdr), method = "radix")
  signed_headers <- paste(names_sorted, collapse = ";")
  canonical_headers <- paste0(paste0(names_sorted, ":", vapply(hdr[names_sorted], trim, ""), "\n"), collapse = "")
  canonical <- paste(toupper(method), normalized, canonical_query, canonical_headers, signed_headers, .hex_hash(body), sep = "\n")
  date <- substring(hdr[["x-amz-date"]], 1L, 8L)
  scope <- paste(date, region, service, "aws4_request", sep = "/")
  to_sign <- paste("AWS4-HMAC-SHA256", hdr[["x-amz-date"]], scope, .hex_hash(charToRaw(enc2utf8(canonical))), sep = "\n")
  key <- charToRaw(paste0("AWS4", credential$secret_access_key))
  for (piece in c(date, region, service, "aws4_request")) key <- .hmac(key, piece)
  signature <- paste(sprintf("%02x", as.integer(.hmac(key, to_sign))), collapse = "")
  auth <- paste0("AWS4-HMAC-SHA256 Credential=", credential$access_key_id, "/", scope, ", SignedHeaders=", signed_headers, ", Signature=", signature)
  hdr$authorization <- auth
  structure(list(canonical_request = canonical, string_to_sign = to_sign, authorization = auth, headers = hdr), class = "lm15_signature")
}
print.lm15_signature <- function(x, ...) { cat("<lm15 signature: redacted>\n"); invisible(x) }
str.lm15_signature <- function(object, ...) { print.lm15_signature(object); invisible(NULL) }

pkce_challenge <- function(verifier) {
  .string(verifier, "PKCE verifier"); .crypto()
  if (!grepl("^[A-Za-z0-9._~-]{43,128}$", verifier)) stop("PKCE verifier must contain 43-128 unreserved ASCII characters.", call. = FALSE)
  .base64url(openssl::sha256(charToRaw(verifier)))
}
generate_pkce <- function() {
  .crypto(); verifier <- .base64url(openssl::rand_bytes(64L))
  structure(list(verifier = verifier, challenge = pkce_challenge(verifier), method = "S256"), class = "lm15_pkce")
}
print.lm15_pkce <- function(x, ...) { cat("<lm15 PKCE S256 pair; verifier redacted>\n"); invisible(x) }
str.lm15_pkce <- function(object, ...) { print.lm15_pkce(object); invisible(NULL) }

jwt_rs256 <- function(header, claims, private_key_pem) {
  .crypto()
  if (!.is_object(header) || !identical(header$alg, "RS256") || !.is_object(claims)) stop("RS256 signing requires an RS256 header and a claims object.", call. = FALSE)
  key <- tryCatch(openssl::read_key(private_key_pem, password = function(...) stop("Encrypted key", call. = FALSE)), error = function(e) .abort("Cannot load the RSA key; provide unencrypted PKCS#1 or PKCS#8 PEM.", "not_configured"))
  if (!inherits(key, "rsa")) .abort("RS256 requires an RSA private key.", "not_configured")
  bytes <- paste0(.base64url(charToRaw(.json_encode(header))), ".", .base64url(charToRaw(.json_encode(claims))))
  signature <- tryCatch(openssl::signature_create(charToRaw(bytes), hash = openssl::sha256, key = key), error = function(e) .abort("RSA signing failed; key material is not shown.", "auth"))
  paste0(bytes, ".", .base64url(signature))
}
