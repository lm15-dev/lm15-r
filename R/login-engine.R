# The machinery every managed login runs on (AUTH-18, AUTH-20, AUTH-21).
# Provider flows (login-flows.R) describe their protocol; this file supplies
# what must be identical for all of them: one attempt's deadline,
# cancellation and UI; bounded TLS-only auth exchanges whose failures never
# carry a token or a reflected provider string; RFC 8628 device polling with
# the ratified pacing; and manual-return parsing against the attempt's
# registered return context. Nothing here knows a provider's URL.

.attempt_lifetime_s <- 15 * 60        # AUTH-18
.exchange_timeout_s <- 30             # AUTH-20.5
.device_default_interval_s <- 5       # RFC 8628 section 3.2
.device_slow_down_step_s <- 5         # RFC 8628 section 3.5
.auth_response_limit <- 1024 * 1024   # AUTH-18: 1 MiB auth HTTP body
.callback_target_limit <- 8 * 1024    # AUTH-18: 8 KiB request target
.oauth_error_codes <- c("invalid_request", "invalid_client", "invalid_grant", "unauthorized_client", "unsupported_grant_type",
  "invalid_scope", "access_denied", "server_error", "temporarily_unavailable", "authorization_pending", "slow_down", "expired_token")

# Outcomes the engine signals; the manager maps them to AUTH-24 errors.
login_cancelled <- function(message = "login cancelled") structure(class = c("LoginCancelled", "error", "condition"), list(message = message, call = NULL))
.login_expired <- function(message) structure(class = c("lm15_login_expired", "error", "condition"), list(message = message, call = NULL))
.login_denied <- function(message, status = NULL, provider_code = NULL, stage = "authorization")
  structure(class = c("lm15_login_denied", "error", "condition"), list(message = message, call = NULL, status = status, provider_code = provider_code, stage = stage))
.deny <- function(...) stop(.login_denied(...))

# One attempt's context. `clock` is monotonic seconds, `wall` epoch seconds;
# `http`, `sleep` are seams a test injects; production leaves the defaults.
.login_context <- function(ui, deadline, provider, clock, wall, http = NULL, sleep = NULL, cancelled = function() FALSE) {
  ctx <- new.env(parent = emptyenv())
  ctx$ui <- ui; ctx$deadline <- deadline; ctx$provider <- provider; ctx$clock <- clock; ctx$wall <- wall
  ctx$http <- http; ctx$sleep <- sleep; ctx$cancelled <- cancelled
  ctx$remaining <- function() ctx$deadline - ctx$clock()
  ctx$check <- function() {
    if (isTRUE(ctx$cancelled())) stop(login_cancelled())
    if (ctx$remaining() <= 0) stop(.login_expired("login attempt deadline reached"))
  }
  ctx$wait <- function(seconds) {
    ctx$check()
    seconds <- min(max(seconds, 0), max(ctx$remaining(), 0))
    if (seconds <= 0) return(ctx$check())
    if (is.function(ctx$sleep)) ctx$sleep(seconds)
    else {
      end <- ctx$clock() + seconds
      while ((left <- end - ctx$clock()) > 0) {
        if (isTRUE(ctx$cancelled())) stop(login_cancelled())
        Sys.sleep(min(left, 0.25))
      }
    }
    ctx$check()
  }
  ctx$notify <- function(notice) { if (!is.null(ctx$ui)) ctx$ui$notify(notice); invisible(NULL) }
  ctx$prompt <- function(prompt) {
    ctx$check()
    if (is.null(ctx$ui)) .abort_auth("This operation needs a choice or input and no UI was supplied.", reason = "interaction_required", stage = "interaction", recovery = "provide_input", provider = ctx$provider)
    answer <- tryCatch(ctx$ui$prompt(prompt), interrupt = function(e) stop(login_cancelled("login cancelled at the prompt")))
    if (!is.character(answer) || length(answer) != 1L || is.na(answer)) stop("The UI's prompt must return one string.", call. = FALSE)
    answer
  }
  ctx$budget <- function() max(0.1, min(.exchange_timeout_s, ctx$remaining()))
  ctx
}

# Typed prompts and notices (AUTH-16): plain lists with a type, so an
# application UI can switch on it.
.prompt <- function(type, field_id, label, ...) structure(c(list(type = type, field_id = field_id, label = label), list(...)), class = c(paste0("lm15_", type, "_prompt"), "lm15_prompt"))
.option <- function(id, label = id, description = NULL) list(id = id, label = label, description = description)
.notice <- function(type, ...) structure(c(list(type = type), list(...)), class = c(paste0("lm15_", type, "_notice"), "lm15_notice"))
print.lm15_prompt <- function(x, ...) { cat("<lm15 ", x$type, " prompt: ", x$field_id, ">\n", sep = ""); invisible(x) }
print.lm15_notice <- function(x, ...) { cat("<lm15 ", x$type, " notice>\n", sep = ""); invisible(x) }

# The version string lm15 identifies itself with on auth exchanges.
.lm15_version <- function() tryCatch(as.character(utils::packageVersion("lm15")), error = function(e) "dev")

.url_parts <- function(url) {
  m <- regmatches(url, regexec("^([A-Za-z][A-Za-z0-9+.-]*)://(?:([^@/?#]*)@)?(\\[[^]]*\\]|[^:/?#]*)(?::([0-9]*))?([^?#]*)(?:\\?([^#]*))?(?:#(.*))?$", url, perl = TRUE))[[1L]]
  if (!length(m)) return(NULL)
  list(scheme = tolower(m[[2L]]), userinfo = if (nzchar(m[[3L]])) m[[3L]] else NULL, host = tolower(m[[4L]]), port = if (nzchar(m[[5L]])) as.integer(m[[5L]]) else NULL,
    path = m[[6L]], query = m[[7L]], fragment = if (nzchar(m[[8L]])) m[[8L]] else NULL, has_query = grepl("?", url, fixed = TRUE))
}
.form_decode <- function(x) utils::URLdecode(gsub("+", " ", x, fixed = TRUE))
.parse_query <- function(query) {
  if (!nzchar(query)) return(list())
  lapply(strsplit(query, "&", fixed = TRUE)[[1L]], function(pair) {
    i <- regexpr("=", pair, fixed = TRUE)[[1L]]
    c(.form_decode(if (i < 0L) pair else substr(pair, 1L, i - 1L)), .form_decode(if (i < 0L) "" else substring(pair, i + 1L)))
  })
}
.form_encode <- function(params) paste(vapply(names(params), function(k) paste0(.form_component(k), "=", .form_component(params[[k]])), ""), collapse = "&")
.form_component <- function(x) gsub("%20", "+", .path_id(x), fixed = TRUE)

# One bounded auth exchange (AUTH-18/20/21). The reply body may hold tokens:
# it is returned for the flow to read and never rendered. Only fixed protocol
# words (an OAuth error code, a response-format category) leave it.
.auth_send <- function(ctx, method, url, headers = list(), body = NULL, content_type = NULL) {
  parts <- .url_parts(url)
  if (is.null(parts) || parts$scheme != "https" || !nzchar(parts$host))
    .abort_auth("Refusing a credential-bearing exchange over a non-HTTPS URL.", reason = "method_unavailable", stage = "exchange", recovery = "operator_action", provider = ctx$provider)
  hdr <- list(accept = "application/json")
  if (!is.null(content_type)) hdr[["content-type"]] <- content_type
  for (name in names(headers)) hdr[[tolower(name)]] <- headers[[name]]
  if (is.null(hdr[["user-agent"]])) hdr[["user-agent"]] <- paste0("lm15/", .lm15_version())
  wire <- structure(list(method = method, url = url, headers = hdr, body = body %||% raw()), class = "lm15_wire_request")
  send <- ctx$http %||% transport_curl(timeout = ctx$budget(), connect_timeout = min(10, ctx$budget()), max_response_bytes = .auth_response_limit)
  reply <- tryCatch(send(wire), error = function(e) {
    if (inherits(e, "LM15Error") && isTRUE(e$limit_exceeded))
      .abort(paste0(ctx$provider, ": the authentication response exceeded ", .auth_response_limit, " bytes; refused."), "auth", ctx$provider)
    # The exception text can hold anything the network stack saw; only a
    # classification leaves. A refused connection or DNS failure never
    # reached the provider; a timeout after sending may have (AUTH-20.6).
    err <- lm15_error(paste0(ctx$provider, ": network failure during an authentication exchange to ", url), code = "transport", provider = ctx$provider)
    err$exchange_uncertain <- !isFALSE(e$exchange_uncertain)
    stop(err)
  })
  raw_body <- reply$body %||% raw()
  if (is.character(raw_body)) raw_body <- charToRaw(enc2utf8(raw_body))
  if (length(raw_body) > .auth_response_limit) .abort(paste0(ctx$provider, ": the authentication response exceeded ", .auth_response_limit, " bytes; refused."), "auth", ctx$provider)
  pairs <- .header_pairs(reply$headers %||% list())
  header <- function(name) { for (p in pairs) if (identical(p[[1L]], name)) return(p[[2L]]); "" }
  ctype <- tolower(trimws(sub(";.*$", "", header("content-type"))))
  format <- "empty"; oauth_error <- NULL; parsed <- json_object()
  if (length(raw_body)) {
    format <- if (ctype %in% c("text/html", "application/xhtml+xml")) "html" else if (ctype == "application/json" || endsWith(ctype, "+json")) "invalid_json" else "text_or_binary"
    text <- rawToChar(raw_body[raw_body != as.raw(0L)])
    value <- if (validUTF8(text)) tryCatch(.json_decode(text), error = function(e) NULL) else NULL
    if (!is.null(value) || identical(trimws(text), "null")) format <- "json"
    if (.is_object(value)) {
      parsed <- value
      candidate <- value$error
      if (.is_object(candidate)) candidate <- candidate$code %||% candidate$type
      if (!is.null(.json_str(candidate)) && candidate %in% .oauth_error_codes) oauth_error <- candidate
    }
  }
  status <- as.integer(reply$status)
  if (status >= 500L) .abort(paste0(ctx$provider, ": the authentication server answered HTTP ", status), "server", ctx$provider, status = status)
  list(status = status, body = parsed, ok = status >= 200L && status < 300L, response_format = format,
    oauth_error = oauth_error, security_challenge = tolower(trimws(header("cf-mitigated"))) == "challenge")
}
.failure_summary <- function(reply) {
  details <- c(paste("HTTP", reply$status), paste0("response=", reply$response_format),
    if (!is.null(reply$oauth_error)) paste0("OAuth error=", reply$oauth_error) else "no recognized OAuth error code; cause not established",
    if (isTRUE(reply$security_challenge)) "response explicitly marked as a security challenge" else if (reply$response_format == "html") "HTML alone does not establish a security block")
  paste(details, collapse = "; ")
}
.http_json <- function(ctx, url, payload, headers = list(), method = "POST")
  .auth_send(ctx, method, url, headers, if (method == "GET") NULL else charToRaw(enc2utf8(.json_encode(payload))), if (method == "GET") NULL else "application/json")
.http_form <- function(ctx, url, payload, headers = list())
  .auth_send(ctx, "POST", url, headers, charToRaw(enc2utf8(.form_encode(payload))), "application/x-www-form-urlencoded")
.http_get <- function(ctx, url, headers = list()) .auth_send(ctx, "GET", url, headers)

# RFC 8628 polling (AUTH-18): the provider's interval or 5 s; slow_down
# never shortens it and adds at least 5 s; the provider's expiry bounds the
# attempt and never extends it. `poll` returns list(status, value, interval).
.run_device_flow <- function(ctx, poll, interval_s = NULL, expires_in_s = NULL, wait_first = TRUE) {
  interval <- if (!is.null(interval_s) && interval_s > 0) interval_s else .device_default_interval_s
  interval <- max(interval, 1)
  if (!is.null(expires_in_s) && expires_in_s > 0) ctx$deadline <- min(ctx$deadline, ctx$clock() + expires_in_s)
  if (wait_first) ctx$wait(interval)
  repeat {
    ctx$check()
    step <- poll()
    if (step$status == "complete") return(step$value)
    if (step$status == "denied") .deny("the provider reported that authorization was denied")
    if (step$status == "expired") stop(.login_expired("the provider reported that the device code expired"))
    if (step$status == "slow_down") interval <- max(interval + .device_slow_down_step_s, step$interval %||% 0)
    else if (step$status != "pending") stop("device poll returned an unknown status", call. = FALSE)
    ctx$wait(interval)
  }
}
.positive <- function(x) {
  if (is.logical(x) || is.null(x)) return(NULL)
  v <- suppressWarnings(as.numeric(unclass(x)))
  if (length(v) == 1L && is.finite(v) && v > 0) v else NULL
}

# A manual return (AUTH-18): a pasted redirect URL, `code#state`, a query
# string, or a bare code where the profile permits one, checked against the
# attempt's registered return context. No pasted value enters a message.
.parse_manual_return <- function(text, expected_state, allow_bare_code, registered_path = NULL, registered_uri = NULL) {
  invalid <- function(message) .abort_auth(message, reason = "invalid_login_state", stage = "interaction", recovery = "provide_input")
  value <- trimws(text %||% "")
  if (!nzchar(value)) invalid("nothing was pasted")
  if (nchar(value, type = "bytes") > .callback_target_limit) invalid("the pasted return is too long")
  code <- NULL; state <- NULL; params <- NULL; bare <- FALSE
  if (grepl("://", value, fixed = TRUE)) {
    parts <- .url_parts(value)
    if (is.null(parts) || !is.null(parts$userinfo) || !is.null(parts$fragment)) invalid("the pasted URL is not this sign-in's registered return URL")
    if (!is.null(registered_path) && !identical(parts$path, registered_path)) invalid("the pasted URL is not this sign-in's registered return URL")
    if (!is.null(registered_uri)) {
      want <- .url_parts(registered_uri)
      port <- function(p) p$port %||% switch(p$scheme, https = 443L, http = 80L, NA_integer_)
      if (!identical(parts$scheme, want$scheme) || !identical(parts$host, want$host) || !identical(port(parts), port(want)) || !identical(parts$path, want$path))
        invalid("the pasted URL is not this sign-in's registered return URL")
    }
    params <- tryCatch(.parse_query(parts$query), error = function(e) invalid("the pasted URL is not this sign-in's registered return URL"))
  } else if (grepl("^(code|state|error)=", value)) {
    params <- tryCatch(.parse_query(value), error = function(e) invalid("the pasted return is malformed"))
  } else if (grepl("#", value, fixed = TRUE)) {
    at <- regexpr("#", value, fixed = TRUE)[[1L]]
    code <- substr(value, 1L, at - 1L); state <- substring(value, at + 1L)
  } else { code <- value; bare <- TRUE }
  denied <- FALSE
  if (!is.null(params)) {
    names <- vapply(params, function(p) p[[1L]], "")
    if (anyDuplicated(names)) invalid("the pasted return repeats a parameter")
    query <- setNames(lapply(params, function(p) p[[2L]]), names)
    if (!is.null(query$code) && !is.null(query$error)) invalid("the pasted return contains both a code and an error")
    denied <- !is.null(query$error); code <- query$code; state <- query$state
  }
  if (bare && !allow_bare_code) invalid("paste the complete code#state or return URL, not the code alone")
  if (!is.null(expected_state)) {
    if (is.null(state)) { if (!(bare && allow_bare_code)) invalid("this provider's return must carry its state value") }
    else if (!.constant_time_equal(state, expected_state)) invalid("the pasted return does not belong to this sign-in attempt")
  }
  # A wrong-state error never terminates the legitimate attempt as denied.
  if (denied) .deny("the validated pasted return carries a provider error")
  if (is.null(code) || !nzchar(code)) invalid("no authorization code in the pasted text")
  list(code = code, state = state)
}
.constant_time_equal <- function(a, b) {
  x <- as.integer(charToRaw(enc2utf8(a))); y <- as.integer(charToRaw(enc2utf8(b)))
  if (length(x) != length(y)) return(FALSE)
  sum(bitwXor(x, y)) == 0L
}

# The authorization return, validated. R runs one thread, so a loopback
# listener is not raced against a pasted return: with a listener the wait
# services it, and an interrupt (Ctrl-C / Esc) switches to pasting the
# return; without one the manual prompt is asked. A paste that fails
# validation is rejected with a notice and the legitimate wait goes on.
.await_return <- function(ctx, listener, prompt, parse) {
  if (!is.null(listener)) {
    got <- tryCatch(listener$wait_ctx(ctx), interrupt = function(e) NULL)
    if (!is.null(got)) return(got)
    ctx$notify(.notice("info", message = "Waiting for the browser was interrupted; paste the return instead."))
  }
  repeat {
    pasted <- ctx$prompt(prompt)
    got <- tryCatch(parse(pasted), AuthOperationError = function(e) {
      if (!identical(e$reason, "invalid_login_state")) stop(e)
      ctx$notify(.notice("info", message = paste0(conditionMessage(e), ". Try again.")))
      NULL
    })
    if (!is.null(got)) return(got)
  }
}

# A one-shot loopback listener for an authorization-code return (AUTH-18):
# binds 127.0.0.1 only; `redirect_host` is the registered host the provider
# was told (it may say localhost); exact path; state checked on success and
# error returns alike; a return carrying both code and error is invalid.
.loopback_listener <- function(path, expected_state, port = 0L, redirect_host = "127.0.0.1") {
  listener <- tryCatch(oauth_callback_listener(expected_state = expected_state, port = port, path = path), LM15Error = function(e) NULL)
  if (is.null(listener)) return(NULL)
  parts <- .url_parts(listener$redirect_uri)
  listener$redirect_uri <- paste0("http://", redirect_host, ":", parts$port, path)
  listener$wait_ctx <- function(ctx) {
    on.exit(listener$close(), add = TRUE)
    got <- tryCatch(listener$wait(timeout = max(ctx$remaining(), 0)), LM15Error = function(e) {
      if (identical(e$code, "timeout")) stop(.login_expired("login attempt deadline reached"))
      if (identical(e$code, "auth")) .deny("the provider returned an error to the sign-in callback")
      stop(e)
    })
    list(code = got$code, state = got$state)
  }
  listener
}
