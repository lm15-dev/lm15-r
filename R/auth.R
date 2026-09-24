.auth_env <- function(env, name) {
  if (is.null(env)) return(Sys.getenv(name, unset = ""))
  value <- unname(env[name])
  if (length(value) != 1L || is.na(value)) "" else value
}
credentials_path <- function(..., env = NULL) {
  .check_dots(...)
  override <- .auth_env(env, "LM15_CREDENTIALS_PATH")
  if (nzchar(override)) return(.auth_expand(override, env))
  config <- .auth_env(env, "XDG_CONFIG_HOME")
  if (!nzchar(config)) config <- file.path(.auth_home(env), ".config")
  file.path(.auth_expand(config, env), "lm15", "credentials.json")
}
.auth_home <- function(env) {
  home <- .auth_env(env, "HOME")
  if (!nzchar(home) && .Platform$OS.type == "windows") home <- .auth_env(env, "USERPROFILE")
  if (nzchar(home)) return(home)
  if (!is.null(env)) return(NULL)
  path.expand("~")
}
.auth_expand <- function(path, env) {
  if (!length(path)) return(character())
  .string(path, "credential path")
  if (path == "~" || startsWith(path, "~/") || startsWith(path, "~\\")) {
    home <- .auth_home(env)
    if (is.null(home)) .abort("Home-relative credential paths require HOME in the supplied environment.", "not_configured")
    return(if (path == "~") home else file.path(home, substring(path, 3L)))
  }
  if (startsWith(path, "~")) .abort("Expand another user's home path explicitly before passing it to lm15.", "not_configured")
  path
}
.stored_paths <- function(provider, path, env) {
  if (!is.null(path)) return(.auth_expand(path, env))
  home <- .auth_home(env)
  if (is.null(home)) return(character())
  switch(provider, "claude-code" = file.path(home, ".claude", ".credentials.json"), "openai-codex" = file.path(home, ".codex", "auth.json"), xai = c(credentials_path(env = env), file.path(home, ".pi", "agent", "auth.json")), character())
}
.read_auth_file <- function(path) {
  # Secret parser diagnostics are suppressed: neither content nor an
  # exception embedding malformed JSON may enter a condition message.
  tryCatch({
    size <- file.info(path)$size
    if (length(size) != 1L || is.na(size) || size > 1024^2) return(NULL)
    con <- file(path, "rb"); on.exit(close(con), add = TRUE)
    out <- .json_decode(rawToChar(readBin(con, "raw", n = size)))
    if (.is_object(out)) out else NULL
  }, error = function(e) NULL)
}
.jwt_claims <- function(value) {
  tryCatch({
    parts <- strsplit(value, ".", fixed = TRUE)[[1L]]
    if (length(parts) != 3L) return(json_object())
    .wire_object(.json_decode(rawToChar(.base64url_decode(parts[[2L]]))))
  }, error = function(e) json_object())
}
.stored_info <- function(provider, path = NULL, env = NULL, now = Sys.time()) {
  for (source in .stored_paths(provider, path, env)) {
    body <- .read_auth_file(source)
    if (is.null(body)) next
    entry <- switch(provider, "claude-code" = body$claudeAiOauth, "openai-codex" = body$tokens, xai = body$xai)
    if (!.is_object(entry)) next
    token <- switch(provider, "claude-code" = entry$accessToken, "openai-codex" = entry$access_token, xai = entry$access)
    if (!is.character(token) || length(token) != 1L || !nzchar(token)) next
    refresh <- switch(provider, "claude-code" = entry$refreshToken, "openai-codex" = entry$refresh_token, xai = entry$refresh)
    claims <- if (provider == "openai-codex") .jwt_claims(token) else json_object()
    expiry <- switch(provider, "claude-code" = entry$expiresAt, "openai-codex" = claims$exp, xai = entry$expires)
    # Borrowed stores use milliseconds; JWT exp uses seconds. Apply the
    # contract's refresh window at the read boundary, never in the parser.
    seconds <- if (is.null(expiry)) NULL else tryCatch(.number(expiry, "expiry") / if (provider == "openai-codex") 1 else 1000, error = function(e) NULL)
    expires_at <- if (is.null(seconds)) NULL else as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
    account <- if (provider == "openai-codex") entry$account_id %||% claims[["https://api.openai.com/auth"]]$chatgpt_account_id else NULL
    credential <- bearer_token(token, expires_at = if (!is.null(expires_at)) format(expires_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") else NULL)
    return(structure(list(credential = credential, source = source, account_id = account, expired = !is.null(seconds) && seconds <= as.numeric(now) + 300, refreshable = is.character(refresh) && length(refresh) == 1L && nzchar(refresh), refresh_token = refresh), class = "lm15_stored_credential"))
  }
  NULL
}
print.lm15_stored_credential <- function(x, ...) { cat("<lm15 stored credential: redacted>\n"); invisible(x) }
str.lm15_stored_credential <- function(object, ...) { print.lm15_stored_credential(object); invisible(NULL) }
load_local_credential <- function(provider, ..., path = NULL, env = NULL, now = Sys.time(), transport = NULL, lock_timeout = 30) {
  .check_dots(...); provider <- canonical_provider(provider)
  info <- .stored_info(provider, path, env, now)
  if (is.null(info)) .abort("No readable local OAuth credential was found.", "not_configured", provider, credential_hint = .definition(provider)$access$login_hint)
  if (info$expired) return(.refresh_local_credential(provider, info, env, now, transport, lock_timeout))
  info
}
.explicit_source <- function(provider, entries) {
  if (!length(entries)) return(NULL)
  keys <- names(entries)
  if (is.null(keys)) .abort("Explicit credentials must be named by provider.", "not_configured")
  canonical <- gsub("_", "-", keys, fixed = TRUE)
  if (anyDuplicated(canonical)) .abort("Duplicate credential provider spellings.", "not_configured")
  if (any(!canonical %in% providers())) .abort("Unknown explicit credential provider.", "not_configured")
  candidates <- keys[canonical == provider]
  if (!length(candidates)) {
    env_keys <- .definition(provider)$access$env_keys
    if (length(env_keys)) candidates <- Filter(function(key) identical(.definition(key)$access$env_keys, env_keys), keys)
  }
  if (length(candidates) > 1L) .abort("Several explicit credential sources share this provider; configure its exact provider name.", "not_configured", provider)
  if (!length(candidates)) return(NULL)
  key <- candidates[[1L]]; value <- entries[[key]]
  if (is.null(value) || (is.character(value) && (length(value) != 1L || is.na(value) || !nzchar(value)))) .abort("An explicit credential is empty; no environment fallback is allowed.", "not_configured", provider)
  key
}

explain_auth <- function(provider, ..., api_keys = list(), env = NULL, path = NULL, now = Sys.time(), settings = list()) {
  .check_dots(...)
  if (is.list(provider) && !is.null(provider$provider)) provider <- provider$provider
  d <- .definition(provider); provider <- d$id
  policy <- d$access$credential_policy
  if (policy %in% c("aws-chain", "azure-chain", "gcp-chain")) return(.explain_cloud_auth(provider, api_keys, env, settings, now))
  steps <- list(); selected <- FALSE
  add <- function(kind, present, detail) {
    state <- if (!present) "absent" else if (selected) "shadowed" else "selected"
    steps[[length(steps) + 1L]] <<- list(kind = kind, state = state, detail = detail)
    if (present) selected <<- TRUE
  }
  if (policy != "oauth") {
    key <- .explicit_source(provider, api_keys)
    add("api_keys", !is.null(key), if (is.null(key)) "not provided" else paste("provided via", key, "(value hidden)"))
  }
  if (policy %in% c("oauth", "oauth-unless-explicit")) {
    info <- .stored_info(provider, path, env, now)
    add("oauth-file", !is.null(info) && (!info$expired || info$refreshable), if (is.null(info)) "missing or unreadable" else if (info$expired) "renewal required (value hidden)" else "fresh (value hidden)")
  }
  if (policy != "oauth") {
    for (key in unlist(d$access$env_keys)) add(paste0("env:", key), nzchar(.auth_env(env, key)), "value hidden")
    if (!is.null(d$placeholder_key)) add("placeholder", TRUE, "local-server placeholder")
  }
  structure(list(provider = provider, configured = selected, steps = steps), class = "lm15_auth_report")
}
print.lm15_auth_report <- function(x, ...) {
  cat("Authentication for ", x$provider, ":\n", sep = "")
  for (step in x$steps) cat("  ", step$state, " ", step$kind, ": ", step$detail, "\n", sep = "")
  for (name in names(x$settings)) cat("  ", name, ": ", x$settings[[name]], "\n", sep = "")
  cat("Configured: ", if (x$configured) "yes" else "no", "\n", sep = "")
  if (!is.null(x$limitation)) cat(x$limitation, "\n")
  invisible(x)
}
