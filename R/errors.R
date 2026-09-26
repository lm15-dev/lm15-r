.error_classes <- c(auth = "AuthError", billing = "BillingError", rate_limit = "RateLimitError", invalid_request = "InvalidRequestError", context_length = "ContextLengthError", timeout = "TimeoutError", server = "ServerError", unsupported_model = "UnsupportedModelError", unsupported_feature = "UnsupportedFeatureError", not_configured = "NotConfiguredError", unknown_model = "UnknownModelError", ambiguous_model = "AmbiguousModelError", transport = "TransportError", lock_timeout = "LockTimeoutError", stream_assembly = "StreamAssemblyError", collection_limit = "CollectionLimitError", provider = "ProviderError")

lm15_error <- function(message, ..., code = "provider", provider = NULL, provider_code = NULL, status = NULL, request_id = NULL, retry_after = NULL, partial = NULL, part_index = NULL, model = NULL, providers = NULL, credential_hint = NULL, path = NULL, lock_path = NULL, feature = NULL, rate_limit_headers = NULL) {
  .check_dots(...)
  if (!code %in% names(.error_classes)) code <- "provider"
  ancestry <- switch(code,
    context_length = c("InvalidRequestError", "ProviderError"),
    unsupported_model = c("InvalidRequestError", "ProviderError"),
    not_configured = "ConfigurationError", unknown_model = "ConfigurationError", ambiguous_model = "ConfigurationError",
    unsupported_feature = "CapabilityError", transport = character(), lock_timeout = character(), stream_assembly = character(), collection_limit = character(), provider = character(), "ProviderError")
  if (!is.null(retry_after) && (!is.numeric(retry_after) || length(retry_after) != 1L || !is.finite(retry_after) || retry_after < 0)) retry_after <- NULL
  structure(list(message = message, call = NULL, code = code, provider = provider, provider_code = provider_code, status = status, request_id = request_id, retry_after = retry_after, partial = partial, part_index = part_index, model = model, providers = providers, credential_hint = credential_hint, path = path, lock_path = lock_path, feature = feature, rate_limit_headers = .freeze_rate_limits(rate_limit_headers)),
    class = c(unname(.error_classes[[code]]), ancestry, "LM15Error", "error", "condition"))
}
.abort <- function(message, code = "provider", provider = NULL, ...) stop(lm15_error(message, code = code, provider = provider, ...))
.unsupported <- function(provider, feature) .abort(paste0(provider, ": ", feature, " cannot be represented by this provider's wire format."), "unsupported_feature", provider)
conditionMessage.LM15Error <- function(c) {
  context <- c(c$provider, if (!is.null(c$status)) paste("HTTP", c$status), if (!is.null(c$request_id)) paste("request", c$request_id))
  out <- c$message
  if (length(context)) out <- paste0(out, " (", paste(context, collapse = ", "), ")")
  if (!is.null(c$credential_hint)) out <- paste0(out, "\nTo fix: ", c$credential_hint)
  paste0(out, .diagnostics_text(c$rate_limit_headers, c$retry_after))
}
print.LM15Error <- function(x, ...) { cat(conditionMessage(x), "\n", sep = ""); invisible(x) }
retryable <- function(error) inherits(error, "LM15Error") && error$code %in% c("rate_limit", "timeout", "server", "transport", "lock_timeout")

.http_code <- function(status) {
  if (status %in% c(401L, 403L)) return("auth")
  if (status == 402L) return("billing")
  if (status %in% c(408L, 504L)) return("timeout")
  if (status == 429L) return("rate_limit")
  if (status %in% c(400L, 404L, 409L, 413L, 422L)) return("invalid_request")
  if (status >= 500L && status <= 599L) return("server")
  "provider"
}
.error_type_maps <- list(
  anthropic = c(authentication_error = "auth", permission_error = "auth", billing_error = "billing", rate_limit_error = "rate_limit", request_too_large = "invalid_request", not_found_error = "invalid_request", resource_not_found_error = "invalid_request", DeploymentNotFound = "unsupported_model", invalid_authentication_error = "auth", invalid_request_error = "invalid_request", api_error = "server", overloaded_error = "server", timeout_error = "timeout"),
  gemini = c(INVALID_ARGUMENT = "invalid_request", FAILED_PRECONDITION = "billing", PERMISSION_DENIED = "auth", UNAUTHENTICATED = "auth", NOT_FOUND = "invalid_request", RESOURCE_EXHAUSTED = "rate_limit", INTERNAL = "server", UNAVAILABLE = "server", DEADLINE_EXCEEDED = "timeout"),
  openai = c(server_error = "server", rate_limit_exceeded = "rate_limit", invalid_prompt = "invalid_request", vector_store_timeout = "timeout", model_not_found = "unsupported_model", model_not_available = "unsupported_model", unsupported_model = "unsupported_model", DeploymentNotFound = "unsupported_model", context_length_exceeded = "context_length", invalid_api_key = "auth", insufficient_quota = "billing", "1113" = "billing", exceeded_current_quota_error = "billing", authentication_error = "auth", rate_limit_error = "rate_limit")
)
# MAP-15: the pinned forms of a provider's "no such model" answer that carry no
# model-specific code and no not-found class (lm15-contract
# spec/model-not-found.json, carried verbatim; each form has a live receipt).
.model_not_found_forms <- list(
  list(code = "not_found_error", prefix = "model: "),                                 # Anthropic, Claude Code
  list(code = "invalid_request_error", contains = "The supported API model names are "), # DeepSeek
  list(code = "1211"),                                                                 # Z.AI: Unknown Model
  list(code = "1214", prefix = "modelCode: "),                                         # Z.AI: the model field is invalid
  list(code = "400", suffix = " is not a valid model ID"),                             # OpenRouter
  list(code = "invalid-argument", prefix = "Model not found: "),                       # xAI (2026-09-01)
  list(code = "validation_error", contains = "The provided model identifier is invalid"), # Bedrock Chat
  list(code = "invalid_request_error", prefix = "Deployment ", suffix = " doesn't exist or isn't accessible.") # Parasail (2026-09-26)
)
.pinned_model_not_found <- function(code, message) {
  if (!nzchar(code)) return(FALSE)
  for (f in .model_not_found_forms) {
    if (!identical(f$code, code)) next
    if (!is.null(f$prefix) && !startsWith(message, f$prefix)) next
    if (!is.null(f$contains) && !grepl(f$contains, message, fixed = TRUE)) next
    if (!is.null(f$suffix) && !endsWith(message, f$suffix)) next
    return(TRUE)
  }
  FALSE
}
.model_error <- function(message) grepl("model", message, ignore.case = TRUE) && grepl("not found|does not exist|not exist|not supported|unsupported|not available|unknown", message, ignore.case = TRUE)
.context_error <- function(message, dialect) {
  if (dialect == "anthropic") return(grepl("prompt is too long|too many tokens|context window|context length", message, ignore.case = TRUE) || (grepl("token", message, ignore.case = TRUE) && grepl("limit|exceed", message, ignore.case = TRUE)))
  grepl("too long|context length", message, ignore.case = TRUE) || (grepl("token", message, ignore.case = TRUE) && grepl("limit|exceed", message, ignore.case = TRUE))
}
normalize_error <- function(lm, status, body, ..., headers = json_object(), now = Sys.time()) {
  .check_dots(...)
  if (lm$definition$dialect == "typesafe") return(.typesafe_error(lm, status, body, headers))
  data <- tryCatch(if (is.character(body)) .json_decode(body) else body, error = function(e) json_object())
  data <- .wire_object(data)
  dialect <- lm$definition$dialect; provider <- lm$definition$id
  raw <- data$error
  if (provider == "xai" && is.character(raw)) raw <- json_object(message = raw, code = data$code)
  if (dialect == "anthropic" && is.null(raw)) raw <- data
  message <- if (.is_object(raw)) .wire_string(raw$message) else if (is.character(raw)) raw else ""
  if (lm$definition$access$backend == "chatgpt-codex" && is.character(data$detail)) message <- data$detail
  pc <- if (.is_object(raw)) .wire_string(raw$code %||% raw$type %||% raw$status) else ""
  if (dialect == "gemini" && .is_object(raw)) pc <- .wire_string(raw$status)
  if (dialect == "anthropic" && .is_object(raw)) pc <- .wire_string(raw$type %||% raw$code)
  code <- .http_code(status)
  map <- .error_type_maps[[if (dialect %in% c("anthropic", "gemini")) dialect else "openai"]]
  if (pc %in% names(map)) code <- unname(map[[pc]])
  # Azure Responses documents no_capacity and too_many_requests as rate
  # limiting, including inside HTTP 200 streams (error diagnostics).
  if (dialect == "openai-responses" && pc %in% c("no_capacity", "too_many_requests")) code <- "rate_limit"
  if (dialect %in% c("openai-chat", "openai-responses") && pc %in% c("invalid_image", "invalid_image_format", "invalid_base64_image", "invalid_image_url", "image_too_large", "image_too_small", "image_parse_error", "image_content_policy_violation", "invalid_image_mode", "image_file_too_large", "unsupported_image_media_type", "empty_image_file", "failed_to_download_image", "image_file_not_found")) code <- "invalid_request"
  if (dialect %in% c("anthropic", "gemini") && .context_error(message, dialect)) code <- "context_length"
  model_candidate <- if (dialect == "anthropic") pc %in% c("not_found_error", "resource_not_found_error") else status == 404L || dialect == "gemini" || lm$definition$access$backend == "chatgpt-codex"
  if (model_candidate && .model_error(message)) code <- "unsupported_model"
  if (.pinned_model_not_found(pc, message)) code <- "unsupported_model"  # MAP-15
  if (!nzchar(message)) message <- paste("Provider returned HTTP", status)
  pairs <- .header_pairs(headers)
  header <- function(name) { for (p in pairs) if (identical(p[[1L]], tolower(name)) && nzchar(p[[2L]])) return(p[[2L]]); NULL }
  request_id <- if (.is_object(data)) .nonempty_string(data$request_id) else NULL
  if (is.null(request_id)) for (name in .request_id_headers) {
    request_id <- header(name); if (!is.null(request_id)) break
  }
  body_hint <- if (.is_object(raw)) raw$retry_after %||% data$retry_after else data$retry_after
  hint <- .retry_after_hint(body_hint, pairs, now)
  lm15_error(message, code = code, provider = provider, provider_code = if (nzchar(pc)) pc else NULL, status = status, request_id = request_id, retry_after = hint, rate_limit_headers = .rate_limit_snapshot(pairs))
}

# Provider error diagnostics (docs/error-diagnostics.md, 2026-09-19): a
# bounded snapshot of rate-limit evidence, never a quota model or a retry
# policy. Only this CLOSED set of header names is retained.
.rate_limit_names <- c("retry-after", "retry-after-ms", "x-ms-retry-after-ms", "x-ratelimit-type", "x-ratelimit-abusepenalty-active",
  as.vector(outer(c("limit", "remaining", "reset", "renewalperiod"), c("requests", "tokens"), function(f, u) paste0("x-ratelimit-", f, "-", u))),
  as.vector(outer(c("requests", "tokens", "input-tokens", "output-tokens"), c("limit", "remaining", "reset"), function(u, f) paste0("anthropic-ratelimit-", u, "-", f))))
# pairs: list of c(name, value) in arrival order. At most four values per
# name, each 1-256 printable ASCII characters; others are rejected, not clipped.
.rate_limit_snapshot <- function(pairs) {
  out <- list()
  for (pair in pairs) {
    if (length(pair) != 2L || !is.character(pair)) next
    name <- tolower(pair[[1L]]); value <- pair[[2L]]
    if (!name %in% .rate_limit_names || is.na(value) || nchar(value, type = "bytes") < 1L || nchar(value, type = "bytes") > 256L) next
    if (grepl("[^\x20-\x7e]", value, useBytes = TRUE)) next
    if (length(out[[name]]) < 4L) out[[name]] <- c(out[[name]], value)
  }
  .json_object(lapply(out, function(v) .json_array(as.list(v))))
}
.freeze_rate_limits <- function(x) {
  if (is.null(x) || !length(x)) return(json_object())
  if (!.is_object(x)) return(json_object())
  .rate_limit_snapshot(unlist(lapply(names(x), function(n) lapply(unlist(x[[n]]), function(v) c(n, v))), recursive = FALSE))
}
.header_pairs <- function(headers) {
  pairs <- attr(headers, "pairs", exact = TRUE)
  if (!is.null(pairs)) return(pairs)
  if (!length(headers)) return(list())
  unlist(lapply(names(headers), function(n) lapply(unlist(headers[[n]]), function(v) c(tolower(n), as.character(v)))), recursive = FALSE)
}
.ms_seconds <- function(value) {
  if (!is.character(value) || length(value) != 1L || nchar(value) > 256L) return(NULL)
  value <- trimws(value)
  if (!grepl("^\\+?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?$", value, perl = TRUE)) return(NULL)
  number <- as.numeric(value)
  if (is.finite(number) && number >= 0) number / 1000 else NULL
}
# The wait hint: a body-derived number of seconds, else the FIRST Retry-After
# (delta-seconds or HTTP-date), else the first retry-after-ms, else the first
# x-ms-retry-after-ms. Never derived from reset or remaining counters.
.retry_after_hint <- function(body_hint, pairs, now) {
  hint <- if (!is.null(body_hint)) tryCatch(.number(body_hint, "retry_after"), error = function(e) NULL) else NULL
  if (!is.null(hint) && is.finite(hint) && hint >= 0) return(hint)
  first <- function(name) { for (p in pairs) if (identical(tolower(p[[1L]]), name)) return(p[[2L]]); NULL }
  value <- first("retry-after")
  if (!is.null(value)) {
    value <- trimws(value)
    if (grepl("^[0-9]+$", value)) return(as.numeric(value))
    date <- suppressWarnings(as.POSIXct(value, format = "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT"))
    if (length(date) == 1L && !is.na(date)) return(max(0, as.numeric(difftime(date, now, units = "secs"))))
  }
  for (name in c("retry-after-ms", "x-ms-retry-after-ms")) {
    hint <- .ms_seconds(first(name))
    if (!is.null(hint)) return(hint)
  }
  NULL
}
.request_id_headers <- c("x-request-id", "request-id", "x-amzn-requestid", "x-amz-request-id", "x-ms-request-id", "apim-request-id", "x-typesafe-request-id")
.diagnostics_text <- function(snapshot, retry_after) {
  pieces <- character()
  if (is.numeric(retry_after) && length(retry_after) == 1L && is.finite(retry_after) && retry_after >= 0)
    pieces <- c(pieces, paste0("Retry advice: ", format(retry_after, digits = 6), " seconds (not a guarantee)."))
  if (length(snapshot)) {
    keys <- sort(names(snapshot))
    raw <- .json_encode(.json_object(setNames(lapply(keys, function(k) snapshot[[k]]), keys)))
    if (nchar(raw) > 2048L) raw <- paste0(substr(raw, 1L, 2048L), "... [full retained values in rate_limit_headers]")
    pieces <- c(pieces, paste0("Provider rate-limit headers (raw; advisory): ", raw))
  }
  if (length(pieces)) paste0("\n\n  ", paste(pieces, collapse = "\n  ")) else ""
}
# The handshake block an HTTP stream driver attaches to an in-stream error
# event (ErrorDetail$http_response); NULL when there is no evidence.
.handshake_block <- function(headers, now = Sys.time()) {
  if (is.null(headers)) return(NULL)
  pairs <- .header_pairs(headers)
  first <- function(name) { for (p in pairs) if (identical(p[[1L]], name) && nzchar(p[[2L]])) return(p[[2L]]); NULL }
  request_id <- NULL
  for (name in .request_id_headers) { request_id <- first(name); if (!is.null(request_id)) break }
  out <- json_object()
  if (!is.null(request_id)) out$request_id <- request_id
  wait <- .retry_after_hint(NULL, pairs, now)
  if (!is.null(wait)) out$retry_after <- as.double(wait)
  snapshot <- .rate_limit_snapshot(pairs)
  if (length(snapshot)) out$rate_limit_headers <- snapshot
  if (length(out)) out else NULL
}
