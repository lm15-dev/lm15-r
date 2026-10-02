.auth_env <- function(env, name) {
  if (is.null(env)) return(Sys.getenv(name, unset = ""))
  value <- unname(unclass(env)[name])
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
# The stored xAI subscription's state, offline (AUTH-1, R2/R3 2026-09-22):
# "usable" (fresh, or expired with a refresh token); "unusable" (expired with
# no refresh token); "logged_out" (the non-secret marker lm15's store keeps
# after a sign-out); "absent" (nothing stored). Unusable and logged-out
# subscriptions block automatic use of an ambient API key: a failed
# subscription is never silently replaced by a metered key.
.xai_stored_state <- function(path = NULL, env = NULL, now = Sys.time()) {
  for (source in .stored_paths("xai", path, env)) {
    body <- .read_auth_file(source)
    if (is.null(body)) next
    info <- .stored_info("xai", source, env, now)
    if (!is.null(info)) return(if (!info$expired || info$refreshable) "usable" else "unusable")
    slot <- body[["_lm15"]]$slots$xai
    if (.is_object(slot) && isTRUE(slot$logged_out)) return("logged_out")
  }
  "absent"
}
.subscription_block_error <- function(provider, state, keys) {
  what <- if (state == "logged_out") "signed out" else "expired and cannot be renewed"
  .abort(paste0("The saved ", provider, " subscription sign-in is ", what, ". lm15 does not switch to a paid API key on its own; sign in again with login(\"", provider, "\"), or pass the key explicitly (", paste(keys, collapse = " or "), " is used only when passed explicitly)."), "not_configured", provider, credential_hint = paste0("login(\"", provider, "\")"))
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
# `router`, when given, adds its declared providers to the names an entry may
# use and to the shared-key comparison (AUTH-1 § Shared explicit keys).
.explicit_source <- function(provider, entries, router = NULL) {
  if (!length(entries)) return(NULL)
  keys <- names(entries)
  if (is.null(keys)) .abort("Explicit credentials must be named by provider.", "not_configured")
  canonical <- gsub("_", "-", keys, fixed = TRUE)
  if (anyDuplicated(canonical)) .abort("Duplicate credential provider spellings.", "not_configured")
  lookup <- if (is.null(router)) .definition else function(p) .router_definition(router, p)
  known <- if (is.null(router)) providers() else c(providers(), vapply(router$providers %||% list(), function(d) d$id, ""))
  if (any(!canonical %in% known)) .abort("Unknown explicit credential provider.", "not_configured")
  candidates <- keys[canonical == provider]
  if (!length(candidates)) {
    env_keys <- lookup(provider)$access$env_keys
    if (length(env_keys)) candidates <- Filter(function(key) identical(lookup(key)$access$env_keys, env_keys), keys)
  }
  if (length(candidates) > 1L) .abort("Several explicit credential sources share this provider; configure its exact provider name.", "not_configured", provider)
  if (!length(candidates)) return(NULL)
  key <- candidates[[1L]]; value <- entries[[key]]
  if (is.null(value) || (is.character(value) && (length(value) != 1L || is.na(value) || !nzchar(value)))) .abort("An explicit credential is empty; no environment fallback is allowed.", "not_configured", provider)
  key
}

explain_auth <- function(provider, ..., api_keys = list(), env = NULL, path = NULL, now = Sys.time(), settings = list(), credential = NULL, auth = NULL) {
  .check_dots(...)
  if (inherits(provider, "lm15_router")) stop("Pass a provider name; give the router's auth with auth = .", call. = FALSE)
  if (is.list(provider) && !is.null(provider$provider)) provider <- provider$provider
  d <- .definition(provider); provider <- d$id
  policy <- d$access$credential_policy
  if (!is.null(auth)) return(.explain_managed(provider, d, .auth_arg(auth), api_keys, env, credential))
  if (policy %in% c("aws-chain", "azure-chain", "gcp-chain")) return(.explain_cloud_auth(provider, api_keys, env, settings, now, credential))
  if (!is.null(credential)) .check_named(provider, credential)
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
  blocked <- FALSE
  if (policy %in% c("oauth", "oauth-unless-explicit")) {
    info <- .stored_info(provider, path, env, now)
    state <- if (policy == "oauth-unless-explicit") .xai_stored_state(path, env, now) else "absent"
    if (!selected && state %in% c("unusable", "logged_out")) blocked <- TRUE
    add("oauth-file", !is.null(info) && (!info$expired || info$refreshable), if (state == "logged_out") "signed out (marker present)" else if (is.null(info)) "missing or unreadable" else if (info$expired && !info$refreshable) "expired, no refresh token" else if (info$expired) "renewal required (value hidden)" else "fresh (value hidden)")
  }
  if (policy != "oauth") {
    for (key in unlist(d$access$env_keys)) {
      set <- nzchar(.auth_env(env, key))
      if (blocked && set) steps[[length(steps) + 1L]] <- list(kind = paste0("env:", key), state = "shadowed", detail = "set, blocked by the failed/signed-out subscription (pass it explicitly to use it)")
      else add(paste0("env:", key), set, "value hidden")
    }
    if (!is.null(d$placeholder_key)) add("placeholder", TRUE, "local-server placeholder")
  }
  .with_backend_report(structure(list(provider = provider, configured = selected, steps = steps), class = "lm15_auth_report"), d, settings, env)
}

# A door without a host prints its backend settings the way a cloud door prints
# its host settings (AUTH-7; AUTH-10 amended 2026-09-30): the Claude Code release
# the claude-code door claims, and where it came from.
.with_backend_report <- function(report, d, settings, env) {
  if (!is.null(d$access$host) || (!length(d$access$backend_settings) && !length(settings))) return(report)
  resolved <- .resolve_backend_settings(d$access, settings, function(name) .auth_env(env, name))
  report$settings <- structure(resolved$values, sources = resolved$sources)
  report
}
print.lm15_auth_report <- function(x, ...) {
  cat("Authentication for ", x$provider, ":\n", sep = "")
  for (step in x$steps) cat("  ", step$state, " ", step$kind, ": ", step$detail, "\n", sep = "")
  sources <- attr(x$settings, "sources")
  from_text <- c(explicit = "passed explicitly", `adc-env` = "the GOOGLE_APPLICATION_CREDENTIALS file", `gcloud-config` = "gcloud's active configuration", `adc-file` = "the application-default credentials file", metadata = "the Google Cloud metadata server", `aws-profile` = "the active AWS profile", default = "the default")
  describe <- function(from) if (startsWith(from, "env:")) paste0("env $", substring(from, 5L)) else unname(from_text[from]) %|NA|% from
  for (name in names(x$settings)) {
    from <- sources[[name]]$from
    cat("  setting ", name, ": ", x$settings[[name]], if (!is.null(from)) paste0(" (from ", describe(from), ")"), "\n", sep = "")
  }
  for (name in names(sources)) {
    s <- sources[[name]]
    if (identical(s$state, "unprobed")) cat("  setting ", name, ": not found offline; ", describe(s$from), " is asked at request time\n", sep = "")
    else if (is.null(s$value)) cat("  setting ", name, ": missing (required, no default)\n", sep = "")
  }
  cat("Configured: ", if (x$configured) "yes" else "no", "\n", sep = "")
  if (!is.null(x$limitation)) cat(x$limitation, "\n")
  invisible(x)
}

`%|NA|%` <- function(x, y) if (length(x) != 1L || is.na(x)) y else x

# AUTH-15 mode B, rung by rung: the explicit entry, the named cloud identity,
# the scope's saved connection; environment keys are shown and marked not
# consulted. Store reads only, no renewal (AUTH-7).
.explain_managed <- function(provider, d, auth, api_keys, env, named) {
  steps <- list(); selected <- FALSE
  add <- function(kind, state, detail) steps[[length(steps) + 1L]] <<- list(kind = kind, state = state, detail = detail)
  source <- .explicit_source(provider, api_keys)
  if (!is.null(source)) { add("api_keys", "selected", paste("provided via", source, "(value hidden)")); selected <- TRUE }
  else add("api_keys", "absent", "not provided")
  if (!is.null(named)) { .check_named(provider, named); add("named_cloud", if (selected) "shadowed" else "selected", paste0("named identity '", named, "'")); selected <- TRUE }
  s <- auth$status(provider)
  if (!is.null(s$connection)) {
    state <- if (selected) "shadowed" else if (s$usability %in% c("ready", "renewal_due")) "selected" else "absent"
    add("connection", state, paste0(s$connection$label, " (", s$usability, if (!is.null(s$expires_at)) paste0(", expires ", s$expires_at), ")"))
    selected <- selected || state == "selected"
  } else add("connection", "absent", if (isTRUE(s$logged_out)) "signed out (marker present)" else paste("none saved in", auth$store$description))
  for (key in unlist(d$access$env_keys)) {
    if (nzchar(.auth_env(env, key))) add(paste0("env:", key), "shadowed", "set, not consulted under a managed Auth (pass it explicitly to use it)")
    else add(paste0("env:", key), "absent", "not set")
  }
  if (!is.null(d$placeholder_key) && !isTRUE(s$logged_out)) { add("placeholder", if (selected) "shadowed" else "selected", "local-server placeholder"); selected <- TRUE }
  .with_backend_report(structure(list(provider = provider, configured = selected, steps = steps), class = "lm15_auth_report"), d, list(), env)
}
