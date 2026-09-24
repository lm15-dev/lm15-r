.auth_failure <- function(provider, message = "Login renewal failed; secret diagnostics are suppressed.") {
  .abort(message, "auth", provider, credential_hint = .definition(provider)$access$login_hint %||% "Run login('xai') to sign in again.")
}
.lock_path <- function(path, env) {
  directory <- .auth_env(env, "LM15_LOCK_DIR")
  if (!nzchar(directory)) {
    cache <- .auth_env(env, "XDG_CACHE_HOME")
    if (!nzchar(cache)) cache <- file.path(.auth_home(env), ".cache")
    directory <- file.path(cache, "lm15", "locks")
  }
  directory <- .auth_expand(directory, env)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE, mode = "0700")
  canonical <- if (file.exists(path)) normalizePath(path, mustWork = TRUE) else file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
  if (.Platform$OS.type == "windows") canonical <- tolower(canonical)
  file.path(directory, paste0(.hex_hash(charToRaw(enc2utf8(canonical))), ".lock"))
}
.with_credential_lock <- function(path, action, env = NULL, timeout = 30) {
  if (!requireNamespace("filelock", quietly = TRUE)) .abort("Credential writes require the filelock R package.", "not_configured")
  .number(timeout, "lock timeout")
  if (timeout < 0) stop("Lock timeout must be non-negative.", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE, mode = "0700")
  target <- .lock_path(path, env)
  lock <- tryCatch(filelock::lock(target, timeout = timeout * 1000), error = function(e) .abort("Cannot acquire credential file lock.", "not_configured"))
  if (is.null(lock)) .abort("Timed out waiting for another process to finish updating credentials.", "lock_timeout", path = path, lock_path = target)
  on.exit(filelock::unlock(lock), add = TRUE)
  Sys.chmod(target, "0600")
  action()
}
.write_credentials_unlocked <- function(path, value) {
  if (!.is_object(value)) stop("Credential storage requires a JSON object.", call. = FALSE)
  target <- if (file.exists(path)) normalizePath(path, mustWork = TRUE) else file.path(normalizePath(dirname(path), mustWork = TRUE), basename(path))
  temporary <- tempfile(".lm15-", tmpdir = dirname(target))
  bytes <- charToRaw(enc2utf8(paste0(.json_encode(value), "\n")))
  tryCatch(.Call(C_lm15_atomic_write, target, temporary, bytes), error = function(e) .abort("Could not durably save credentials. No secret details are shown.", "auth"))
  invisible(NULL)
}
write_credentials <- function(value, ..., path = credentials_path(env = env), env = NULL, lock_timeout = 30) {
  .check_dots(...); path <- .auth_expand(path, env)
  .with_credential_lock(path, function() .write_credentials_unlocked(path, value), env, lock_timeout)
  invisible(NULL)
}
.form_body <- function(body) charToRaw(paste(vapply(names(body), function(name) paste0(.path_id(name), "=", .path_id(.scalar_text(body[[name]]))), ""), collapse = "&"))
.token_http <- function(request, transport = NULL) {
  transport <- transport %||% transport_curl(timeout = 30, connect_timeout = 10, max_response_bytes = 1024^2)
  bytes <- if (request$body_encoding == "form") .form_body(request$body) else charToRaw(enc2utf8(.json_encode(request$body)))
  wire <- structure(list(method = request$method, url = request$url, headers = request$headers, body = bytes), class = "lm15_wire_request")
  reply <- tryCatch(transport(wire), error = function(e) .abort("Credential HTTP exchange failed; secret diagnostics are suppressed.", "auth"))
  data <- tryCatch(.decode_body(reply$body), error = function(e) json_object())
  list(status = reply$status, body = data)
}
.oauth_clients <- list(
  `claude-code` = list(id = "9d1c250a-e61b-44d5-88ed-5944d1962f5e", url = "https://platform.claude.com/v1/oauth/token", encoding = "json"),
  `openai-codex` = list(id = "app_EMoamEEZ73f0CkXaXp7hrann", url = "https://auth.openai.com/oauth/token", encoding = "json"),
  xai = list(id = "b1a00492-073a-47ea-816f-4c329264a828", url = "https://auth.x.ai/oauth2/token", encoding = "form")
)
.oauth_update <- function(provider, body, response, previous_refresh = NULL, now = Sys.time()) {
  access <- response$access_token
  if (!is.character(access) || length(access) != 1L || !nzchar(access)) .auth_failure(provider)
  refresh <- response$refresh_token %||% previous_refresh
  if (!is.null(refresh) && (!is.character(refresh) || length(refresh) != 1L || !nzchar(refresh))) .auth_failure(provider)
  lifetime <- tryCatch(.number(response$expires_in %||% 3600, "expires_in"), error = function(e) .auth_failure(provider))
  if (lifetime <= 300) .auth_failure(provider, "Provider returned a credential already inside the renewal window.")
  expiry <- .json_number(sprintf("%.0f", (as.numeric(now) + lifetime) * 1000))
  key <- switch(provider, `claude-code` = "claudeAiOauth", `openai-codex` = "tokens", xai = "xai")
  entry <- .wire_object(body[[key]])
  if (provider == "claude-code") {
    entry$accessToken <- access; entry$refreshToken <- refresh; entry$expiresAt <- expiry
  } else if (provider == "openai-codex") {
    entry$access_token <- access; entry$refresh_token <- refresh
    if (!is.null(response$id_token)) entry$id_token <- response$id_token
    account <- .jwt_claims(access)[["https://api.openai.com/auth"]]$chatgpt_account_id
    if (!is.null(account)) entry$account_id <- account
    body$last_refresh <- .utc_timestamp(as.numeric(now))
  } else {
    entry$type <- "oauth"; entry$access <- access; entry$refresh <- refresh; entry$expires <- expiry
  }
  body[[key]] <- entry
  body
}
.refresh_local_credential <- function(provider, info, env, now, transport, lock_timeout) {
  if (!info$refreshable) .auth_failure(provider, "Stored login expired and has no renewal token.")
  .with_credential_lock(info$source, function() {
    current <- .stored_info(provider, info$source, env, now)
    if (is.null(current)) .auth_failure(provider, "Stored login was removed or became unreadable while waiting for its lock.")
    if (!current$expired) return(current)
    if (!current$refreshable) .auth_failure(provider, "Stored login expired and has no renewal token.")
    client <- .oauth_clients[[provider]]
    exchange <- .token_request(client$url, json_object(grant_type = "refresh_token", client_id = client$id, refresh_token = current$refresh_token), client$encoding)
    reply <- .token_http(exchange, transport)
    if (reply$status < 200 || reply$status >= 300) .auth_failure(provider)
    body <- .read_auth_file(current$source)
    if (is.null(body)) .auth_failure(provider, "Cannot read the original credential file for a safe update.")
    updated <- .oauth_update(provider, body, reply$body, current$refresh_token, now)
    .write_credentials_unlocked(current$source, updated)
    result <- .stored_info(provider, current$source, env, now)
    if (is.null(result) || result$expired) .auth_failure(provider)
    result
  }, env, lock_timeout)
}
