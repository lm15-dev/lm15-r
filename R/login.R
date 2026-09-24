start_device_login <- function(provider = "xai", ..., transport = NULL, clock = Sys.time) {
  .check_dots(...); provider <- canonical_provider(provider)
  if (provider != "xai") .unsupported(provider, "an owned device login flow")
  req <- .token_request("https://auth.x.ai/oauth2/device/code", json_object(client_id = .oauth_clients$xai$id, scope = "openid profile email offline_access grok-cli:access api:access", referrer = "lm15"))
  reply <- .token_http(req, transport); p <- reply$body
  if (reply$status < 200 || reply$status >= 300) .auth_failure(provider, "Device authorization request failed.")
  required <- function(x) { if (!is.character(x) || length(x) != 1L || !nzchar(x)) .auth_failure(provider, "Device authorization response is malformed."); x }
  uri <- function(x) {
    x <- required(x)
    if (!grepl("^https://[^/@?#[:space:]]+(?:/[^#[:space:]]*)?$", x, perl = TRUE)) .auth_failure(provider, "Device authorization returned an unsafe verification address.")
    x
  }
  lifetime <- tryCatch(.number(p$expires_in, "expires_in"), error = function(e) .auth_failure(provider))
  interval <- tryCatch(.number(p$interval %||% 5, "interval"), error = function(e) .auth_failure(provider))
  if (lifetime <= 0 || interval <= 0) .auth_failure(provider)
  structure(list(provider = provider, user_code = required(p$user_code), device_code = required(p$device_code), verification_uri = uri(p$verification_uri), verification_uri_complete = if (!is.null(p$verification_uri_complete)) uri(p$verification_uri_complete) else NULL, interval = interval, expires_at = as.numeric(clock()) + lifetime), class = "lm15_device_login")
}
print.lm15_device_login <- function(x, ...) { cat("<lm15 pending device login; codes redacted>\n"); invisible(x) }
str.lm15_device_login <- function(object, ...) { print.lm15_device_login(object); invisible(NULL) }

poll_device_login <- function(device, ..., path = credentials_path(env = env), env = NULL, transport = NULL, clock = Sys.time, sleep = Sys.sleep, cancelled = function() FALSE, lock_timeout = 30) {
  .check_dots(...)
  if (!inherits(device, "lm15_device_login")) stop("Expected start_device_login()'s result.", call. = FALSE)
  path <- .auth_expand(path, env)
  interval <- device$interval; provider <- device$provider
  fail <- function(reason, message) {
    error <- lm15_error(message, code = "auth", provider = provider, provider_code = reason)
    class(error) <- c(if (reason == "expired_token") "DeviceExpiredError" else if (reason == "access_denied") "DeviceDeniedError" else "DeviceLoginError", class(error))
    stop(error)
  }
  repeat {
    if (isTRUE(cancelled())) fail("cancelled", "Device login was cancelled locally.")
    remaining <- device$expires_at - as.numeric(clock())
    if (remaining <= 0) fail("expired_token", "Device code expired before approval.")
    sleep(min(interval, remaining))
    if (as.numeric(clock()) >= device$expires_at) fail("expired_token", "Device code expired before approval.")
    if (isTRUE(cancelled())) fail("cancelled", "Device login was cancelled locally.")
    req <- .token_request(.oauth_clients[[provider]]$url, json_object(grant_type = "urn:ietf:params:oauth:grant-type:device_code", client_id = .oauth_clients[[provider]]$id, device_code = device$device_code))
    reply <- .token_http(req, transport)
    if (reply$status >= 200 && reply$status < 300) {
      .with_credential_lock(path, function() {
        current <- .read_auth_file(path)
        if (is.null(current) && file.exists(path)) .auth_failure(provider, "Existing credential store is unreadable; it was not overwritten.")
        .write_credentials_unlocked(path, .oauth_update(provider, current %||% json_object(), reply$body, now = clock()))
      }, env, lock_timeout)
      return(load_local_credential(provider, path = path, env = env, now = clock(), transport = transport))
    }
    reason <- reply$body$error
    if (identical(reason, "authorization_pending")) next
    if (identical(reason, "slow_down")) {
      next_interval <- tryCatch(.number(reply$body$interval, "interval"), error = function(e) NULL)
      interval <- if (!is.null(next_interval) && next_interval > 0) next_interval else interval + 5
      next
    }
    if ((reason %||% "") %in% c("access_denied", "authorization_denied")) fail("access_denied", "Device login was denied.")
    if (identical(reason, "expired_token")) fail("expired_token", "Device code expired before approval.")
    fail("device_login_failed", "Device login failed; secret server diagnostics are suppressed.")
  }
}

login <- function(provider, ..., path = credentials_path(env = env), env = NULL, transport = NULL, echo = base::message, clock = Sys.time, sleep = Sys.sleep, cancelled = function() FALSE) {
  .check_dots(...); d <- .definition(provider)
  if (d$id != "xai") .abort("This provider's login is owned by its CLI or account console; no browser was opened or token requested.", "unsupported_feature", d$id, credential_hint = d$access$login_hint %||% d$console_url %||% "Supply an explicit API key.")
  device <- start_device_login(d$id, transport = transport, clock = clock)
  echo(paste0("Open ", device$verification_uri_complete %||% device$verification_uri, " and enter code: ", device$user_code))
  poll_device_login(device, path = path, env = env, transport = transport, clock = clock, sleep = sleep, cancelled = cancelled)
}
