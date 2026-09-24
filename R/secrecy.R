# Known request credentials are removed from provider error text without
# invoking a rotating credential provider a second time.
.redact_wire_condition <- function(error, wire) {
  if (!inherits(error, "LM15Error")) return(error)
  secrets <- character()
  for (name in names(wire$headers)) {
    if (tolower(name) %in% c("authorization", "x-api-key", "api-key", "x-goog-api-key", "x-amz-security-token")) {
      value <- wire$headers[[name]]
      secrets <- c(secrets, value, sub("^Bearer +", "", value))
    }
  }
  query_at <- regexpr("?", wire$url, fixed = TRUE)[[1L]]
  if (query_at > 0L) for (pair in strsplit(substring(wire$url, query_at + 1L), "&", fixed = TRUE)[[1L]]) {
    at <- regexpr("=", pair, fixed = TRUE)[[1L]]
    if (at > 0L && utils::URLdecode(substr(pair, 1L, at - 1L)) == "key") secrets <- c(secrets, utils::URLdecode(substring(pair, at + 1L)))
  }
  for (name in c("message", "provider_code", "request_id", "credential_hint")) {
    value <- error[[name]]
    if (!is.character(value)) next
    for (secret in unique(secrets[nzchar(secrets)])) value <- gsub(secret, "<redacted>", value, fixed = TRUE)
    error[[name]] <- value
  }
  error
}
.redact_error_event <- function(event, wire) {
  if (event$type != "error") return(event)
  error <- .redact_wire_condition(lm15_error(event$error$message, code = event$error$code, provider_code = event$error$provider_code), wire)
  event$error <- error_detail(error$code, message = error$message, provider_code = error$provider_code)
  event
}
str.LM15Error <- function(object, ...) { print.LM15Error(object); invisible(NULL) }
