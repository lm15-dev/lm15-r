canonical_provider <- function(name) gsub("_", "-", .string(name, "provider"), fixed = TRUE)
.provider_tables <- local({
  value <- NULL
  function() {
    if (is.null(value)) value <<- .json_decode(.provider_table_json)
    value
  }
})
providers <- function() vapply(.provider_tables()$providers, function(p) p$id, "")
.definition <- function(provider) {
  provider <- canonical_provider(provider)
  for (d in .provider_tables()$providers) if (d$id == provider) return(d)
  for (d in .declared_providers()) if (d$id == provider) return(d)
  .abort(paste("Unknown provider:", provider), "not_configured", provider)
}
# Routes that exist only for a managed connection (lm15-contract
# auth/managed/profiles.json): no contract wire receipt, so they are not
# registry rows (a row is a support claim, AUTH-26). A router routes them
# only when it carries an Auth, the only way to hold their credential.
.declared_providers <- local({
  value <- NULL
  function() {
    if (!is.null(value)) return(value)
    access <- function(id, base_url, supports, headers = list()) .json_object(list(provider = id, supports = .json_object(supports), auth_modes = list("bearer"), env_keys = list(),
      base_url = base_url, credential_policy = "key", auth_scheme = list("bearer"), headers = headers, backend = "api", backend_options = json_object()))
    value <<- list(
      list(id = "kimi-code", dialect = "anthropic", compat = NULL, access = access("kimi-code", "https://api.kimi.com/coding", list(complete = TRUE, stream = TRUE)), placeholder_key = NULL, console_url = NULL, declared = TRUE),
      list(id = "github-copilot", dialect = "openai-chat", compat = json_object(instruction_role = "system", max_tokens_field = "max_completion_tokens", stream_usage = "include", thinking_format = "reasoning_effort"),
        access = access("github-copilot", "https://api.individual.githubcopilot.com", list(complete = TRUE, stream = TRUE, models = TRUE),
          headers = lapply(names(.copilot_headers), function(k) list(k, .copilot_headers[[k]]))), placeholder_key = NULL, console_url = NULL, declared = TRUE))
    value
  }
})
.router_providers <- function(router) c(providers(), if (!is.null(router$auth)) vapply(.declared_providers(), function(d) d$id, ""))

# Credentials live in closures so print/str of client configuration cannot
# traverse into a token value. Supplying a function supports rotation.
.credential_closure <- function(value) {
  force(value)
  structure(function() value, class = c("lm15_credential", "function"))
}
print.lm15_credential <- function(x, ...) { cat("<lm15 credential: redacted>\n"); invisible(x) }
.credential_value <- function(provider, credential) {
  out <- tryCatch(credential(), error = function(e) {
    if (inherits(e, "LM15Error")) stop(e)
    .abort("Credential provider failed; no secret details are included.", "auth", provider)
  })
  if (is.character(out)) out <- api_key(out)
  if (!inherits(out, "lm15_Credential")) .abort("Credential provider must return an ApiKey, BearerToken, AwsCredentials, or non-empty string.", "not_configured", provider)
  validate(out)
}

new_lm <- function(provider, ..., api_key = NULL, credential = NULL, base_url = NULL, compat = NULL, settings = list(), transport = NULL, env = NULL, account_id = NULL, clock = Sys.time, credentials_path = NULL, live_connect = NULL, adaptations = "note") {
  .check_dots(...)
  .check_policy(adaptations)
  d <- .definition(provider)
  if (!is.function(clock)) stop("clock must be a function returning the current time.", call. = FALSE)
  if (!is.null(transport) && !is.function(transport)) stop("transport must be a function.", call. = FALSE)
  if (!is.null(live_connect) && !is.function(live_connect)) stop("live_connect must be a function.", call. = FALSE)
  if (!is.null(env) && (!is.character(env) || (length(env) && is.null(names(env))))) stop("env must be a named character vector; character() disables environment lookup.", call. = FALSE)
  # unclass: Sys.getenv() is a "Dlist", and a value keeping that class is not a plain string
  lookup <- function(name) if (is.null(env)) Sys.getenv(name, unset = "") else unname(unclass(env)[name]) %||% ""
  named <- .check_named(d$id, credential)
  if (!is.null(named) && !is.null(api_key)) .abort("A named identity and an explicit api_key both answer 'who am I'; give one.", "not_configured", d$id)
  if (is.null(api_key)) {
    if (is.null(named) && d$access$credential_policy %in% c("oauth", "oauth-unless-explicit")) {
      stored <- .stored_info(d$id, credentials_path, env, clock())
      if (d$access$credential_policy == "oauth" || (!is.null(stored) && (!stored$expired || stored$refreshable))) {
        initial <- load_local_credential(d$id, path = credentials_path, env = env, now = clock(), transport = transport)
        account_id <- account_id %||% initial$account_id
        api_key <- local({
          id <- d$id; source <- initial$source; environment <- env; time <- clock; send <- transport
          structure(function() load_local_credential(id, path = source, env = environment, now = time(), transport = send)$credential,
            class = c("lm15_credential", "function"))
        })
      }
    }
    if (d$access$credential_policy %in% c("aws-chain", "azure-chain", "gcp-chain")) {
      # Resolve only on the first request, after host settings are finalized.
      # Each client owns its cache, so distinct identities cannot share tokens.
      api_key <- local({
        cached_provider <- NULL
        structure(function() {
          if (is.null(cached_provider)) cached_provider <<- cloud_credential_provider(d$id, env = env, settings = settings, transport = transport, clock = clock, named = named)
          cached_provider()
        }, class = c("lm15_credential", "function"))
      })
    }
    if (is.null(api_key) && d$access$credential_policy == "oauth-unless-explicit") {
      state <- .xai_stored_state(credentials_path, env, clock())
      if (state %in% c("unusable", "logged_out")) .subscription_block_error(d$id, state, unlist(d$access$env_keys))
    }
    if (is.null(api_key)) for (key in unlist(d$access$env_keys)) {
      candidate <- lookup(key)
      if (length(candidate) == 1L && !is.na(candidate) && nzchar(candidate)) { api_key <- candidate; break }
    }
    api_key <- api_key %||% d$placeholder_key
  }
  if (is.character(api_key)) {
    value <- .string(api_key, "api_key")
    api_key <- .new_value("ApiKey", list(value = value))
  }
  if (inherits(api_key, "lm15_Credential")) api_key <- .credential_closure(validate(api_key))
  if (!is.function(api_key)) .abort("No credential configured; pass api_key explicitly or configure the provider's environment variable.", "not_configured", d$id)
  tables <- .provider_tables()
  table_name <- switch(d$dialect, "openai-chat" = "chat", "openai-responses" = "responses", "anthropic" = "anthropic", NULL)
  preset <- compat %||% d$compat %||% switch(d$dialect, anthropic = "anthropic", "openai")
  if (is.character(preset)) {
    preset <- tolower(gsub("[- .]", "_", preset))
    aliases <- c(openai_chat = "openai", chat = "openai", chat_completions = "openai", responses = "openai", openai_responses = "openai", lm_studio = "lmstudio", dashscope_qwen = "qwen", z_ai = "zai")
    if (preset %in% names(aliases)) preset <- unname(aliases[[preset]])
    policy <- if (!is.null(table_name)) tables[[table_name]][[preset]] else json_object()
    if (is.null(policy)) .abort("Unknown compatibility preset.", "not_configured", d$id)
    if (is.null(base_url) && !is.null(compat) && is.null(d$access$host)) {
      base_url <- tables[[paste0(table_name, "_urls")]][[preset]]
      if (is.null(base_url)) .abort("This preset has no declared address; pass base_url explicitly.", "not_configured", d$id)
    }
  } else if (.is_object(preset)) policy <- preset else stop("compat must be a preset name or a named policy list.", call. = FALSE)
  if (!is.null(d$access$host)) {
    # Cloud hosts must never quietly resolve to a public API URL.
    host <- d$access$host
    if (any(!names(settings) %in% vapply(host$settings %||% list(), function(s) s$name, ""))) stop("Unknown host setting.", call. = FALSE)
    found <- .resolve_host_settings(d, settings, .cloud_context(d$id, env, settings, transport = transport), probe = TRUE)
    if (!is.null(found$missing)) {
      hint <- if (found$missing == "project") "; set GOOGLE_CLOUD_PROJECT, run `gcloud config set project <id>`, or pass settings = list(project = ...)" else paste0("; pass settings = list(", found$missing, " = ...)")
      .abort(paste0("Missing host setting: ", found$missing, hint), "not_configured", d$id)
    }
    resolved <- list()
    for (s in host$settings %||% list()) {
      value <- found$values[[s$name]]
      .string(value, s$name)
      resource_url <- s$name == "resource" && grepl("://", value, fixed = TRUE)
      if (resource_url) base_url <- base_url %||% .cloud_endpoint_root(d, value)
      if (s$name %in% c("region", "resource", "location") && !resource_url && !grepl("^[A-Za-z0-9-]+$", value)) .abort("Host settings must be DNS labels or an explicit Azure resource URL.", "not_configured", d$id)
      resolved[[s$name]] <- value
    }
    setting_sources <- found$sources
    settings <- resolved
    if (is.null(base_url)) {
      base_url <- host$base_url
      for (name in names(settings)) base_url <- gsub(paste0("{", name, "}"), if (name == "project") .path_id(settings[[name]]) else settings[[name]], base_url, fixed = TRUE)
      loc <- settings$location
      if (!is.null(loc)) base_url <- gsub("{location_host}", if (loc == "global") "aiplatform.googleapis.com" else if (loc %in% c("us", "eu")) paste0("aiplatform.", loc, ".rep.googleapis.com") else paste0(loc, "-aiplatform.googleapis.com"), base_url, fixed = TRUE)
    }
  }
  base_url <- base_url %||% d$access$base_url %||% switch(d$dialect, anthropic = "https://api.anthropic.com/v1", gemini = "https://generativelanguage.googleapis.com/v1beta", typesafe = "https://api.typesafe.ai", "https://api.openai.com/v1")
  .string(base_url, "base_url")
  if (!grepl("^https?://[^/?#[:space:]@]+(?:/[^?#[:space:]]*)?$", base_url, perl = TRUE))
    stop("base_url must be an HTTP(S) root without userinfo, whitespace, query, or fragment.", call. = FALSE)
  structure(list(definition = d, base_url = sub("/+$", "", base_url), compat = policy, credential = api_key, settings = settings, setting_sources = if (exists("setting_sources", inherits = FALSE)) setting_sources, account_id = account_id, clock = clock, transport = transport %||% transport_curl(), live_connect = live_connect, adaptations = adaptations), class = "lm15_lm")
}
print.lm15_lm <- function(x, ...) { cat("<lm15 client: ", x$definition$id, ">\n", sep = ""); invisible(x) }

new_router <- function(..., api_keys = list(), base_urls = list(), settings = list(), catalog = list(), rules = NULL, env = NULL, transport = NULL, live_connect = NULL, adaptations = "note", auth = NULL, credentials = list()) {
  .check_dots(...)
  if (!is.null(auth) && !inherits(auth, "lm15_auth")) stop("auth must come from local_auth(), memory_auth() or new_auth().", call. = FALSE)
  .check_policy(adaptations)
  if (!is.null(live_connect) && !is.function(live_connect)) stop("live_connect must be a function.", call. = FALSE)
  for (entries in list(api_keys, base_urls, settings)) {
    if (length(entries) && (is.null(names(entries)) || any(!gsub("_", "-", names(entries), fixed = TRUE) %in% providers()))) .abort("Configuration contains an unknown provider name.", "not_configured")
    if (anyDuplicated(gsub("_", "-", names(entries), fixed = TRUE))) .abort("Duplicate spellings of a provider are not allowed.", "not_configured")
  }
  for (name in names(api_keys)) if (is.character(api_keys[[name]])) api_keys[[name]] <- api_key(api_keys[[name]])
  structure(list(api_keys = api_keys, base_urls = base_urls, settings = settings, catalog = catalog, client_cache = new.env(parent = emptyenv()), auth = auth, credentials = .check_named_credentials(credentials, api_keys),
    rules = rules %||% list(c("claude-", "anthropic"), c("gpt-", "openai"), c("o1", "openai"), c("o3", "openai"), c("o4", "openai"), c("gemini-", "gemini"), c("gemma-", "gemini"), c("nano-banana", "gemini"), c("grok-", "xai"), c("sora-", "openai"), c("veo-", "gemini"), c("chat-latest", "openai"), c("jev-", "typesafe")), env = env, transport = transport, live_connect = live_connect, adaptations = adaptations), class = "lm15_router")
}
print.lm15_router <- function(x, ...) { cat("<lm15 router; credentials redacted>\n"); invisible(x) }
str.lm15_router <- function(object, ...) { print.lm15_router(object); invisible(NULL) }
str.lm15_lm <- function(object, ...) { print.lm15_lm(object); invisible(NULL) }
str.lm15_credential <- function(object, ...) { print.lm15_credential(object); invisible(NULL) }
resolve <- function(router, model, ...) {
  .check_dots(...); .string(model, "model", empty = TRUE)
  head <- sub(":.*$", "", model); rest <- substring(model, nchar(head) + 2L)
  if (grepl(":", model, fixed = TRUE) && canonical_provider(head) %in% .router_providers(router) && nzchar(rest)) return(list(provider = canonical_provider(head), model = rest, source = "prefix"))
  catalog <- if (inherits(router$catalog, "lm15_model_registry")) router$catalog$list() else router$catalog
  matches <- Filter(function(info) info$id == model || model %in% unlist(info$aliases), catalog)
  if (length(matches)) {
    p <- unique(vapply(matches, function(info) canonical_provider(info$provider), ""))
    if (length(p) > 1L) .abort("Model appears under several providers; use provider:model.", "ambiguous_model", model = model, providers = p)
    exact <- Filter(function(info) info$id == model, matches)
    if (length(exact)) matches <- exact
    if (length(matches) > 1L) .abort("Model matches several catalog entries; use a canonical model id.", "ambiguous_model", model = model, providers = p)
    if (!p %in% providers()) .abort("Catalog names an unknown provider.", "unknown_model", model = model)
    return(list(provider = p, model = matches[[1L]]$id, source = "catalog"))
  }
  for (rule in router$rules) if (startsWith(model, rule[[1L]])) {
    p <- canonical_provider(rule[[2L]])
    if (!p %in% providers()) .abort("Routing rule names an unknown provider.", "unknown_model", model = model)
    return(list(provider = p, model = model, source = "rule"))
  }
  .abort("Cannot route this model; use an explicit provider:model prefix.", "unknown_model", model = model)
}
.router_lm <- function(router, resolution) {
  p <- resolution$provider
  exact <- function(entries) {
    key <- which(gsub("_", "-", names(entries), fixed = TRUE) == p)
    if (length(key)) entries[[key]] else NULL
  }
  definition <- .definition(p)
  if (!is.null(router$auth)) return(.managed_router_lm(router, p, definition, exact))
  # The doctor and runtime use the same explicit/shared-key selection.
  source <- if (definition$access$credential_policy == "oauth") NULL else .explicit_source(p, router$api_keys)
  credential <- if (is.null(source)) NULL else router$api_keys[[source]]
  if (!is.null(exact(router$base_urls)) && !is.null(definition$access$host)) .abort("Cloud addresses must be configured through settings, not base_urls.", "not_configured", p)
  environment <- router$env %||% Sys.getenv()
  inputs <- list(credential = credential, base_url = exact(router$base_urls), settings = exact(router$settings) %||% list(), env = environment, transport = router$transport, live_connect = router$live_connect, adaptations = router$adaptations %||% "note")
  cached <- if (is.environment(router$client_cache)) router$client_cache[[p]] else NULL
  if (!is.null(cached) && identical(cached$inputs, inputs)) return(cached$lm)
  lm <- new_lm(p, api_key = credential, base_url = inputs$base_url, settings = inputs$settings, env = environment, transport = router$transport, live_connect = router$live_connect, adaptations = inputs$adaptations)
  if (is.environment(router$client_cache)) router$client_cache[[p]] <- list(inputs = inputs, lm = lm)
  lm
}
router_lm <- function(router, model, ...) {
  .check_dots(...)
  if (!inherits(router, "lm15_router")) stop("Expected new_router().", call. = FALSE)
  .router_lm(router, resolve(router, model))
}
.route <- function(lm, request) {
  request <- validate(request)
  if (inherits(lm, "lm15_router")) {
    resolution <- resolve(lm, request$model)
    lm <- .router_lm(lm, resolution)
    request$model <- resolution$model
  }
  list(lm = lm, request = request)
}
.require_surface <- function(lm, name) {
  if (!isTRUE(lm$definition$access$supports[[name]])) .unsupported(lm$definition$id, name)
}

.wire_string <- function(x, default = "") if (is.null(x)) default else .scalar_text(x)
.wire_object <- function(x) if (.is_object(x)) x else json_object()
.wire_array <- function(x) if (.is_array(x)) x else list()
.wire_int <- function(x) if (is.null(x)) NULL else .number(x, "provider counter", TRUE)
.path_id <- function(x, resource = FALSE) {
  bytes <- as.integer(charToRaw(enc2utf8(x)))
  safe <- c(45L, 46L, 95L, 126L, 48:57, 65:90, 97:122, if (resource) 47L)
  paste(vapply(bytes, function(b) if (b %in% safe) rawToChar(as.raw(b)) else sprintf("%%%02X", b), ""), collapse = "")
}

# AUTH-15 mode B: an explicit api_keys entry; an explicit named cloud
# identity; the scope's saved connection (renewed per request); a keyless
# local server's placeholder. Never an environment key, a foreign CLI file
# or the machine's cloud chain.
.managed_router_lm <- function(router, p, definition, exact) {
  auth <- router$auth
  source <- .explicit_source(p, router$api_keys)
  credential <- if (is.null(source)) NULL else router$api_keys[[source]]
  named <- router$credentials[[p]] %||% router$credentials[[gsub("-", "_", p, fixed = TRUE)]]
  base_url <- exact(router$base_urls); account_id <- NULL; headers <- list()
  if (is.null(credential) && is.null(named)) {
    saved <- tryCatch(auth$request_auth(p), AuthOperationError = function(e) e)
    if (inherits(saved, "AuthOperationError")) {
      if (identical(saved$reason, "login_required") && !is.null(definition$placeholder_key) && !isTRUE(auth$status(p)$logged_out)) credential <- definition$placeholder_key
      else if (identical(saved$reason, "login_required") && !is.null(definition$access$host))
        .abort_auth(paste0(p, ": no saved connection in this scope; the machine's cloud identity is not used under a managed Auth -- save a named identity (configure(auth, provider, 'cloud', answers = list(named = ...))) or pass credentials = explicitly."),
          reason = "login_required", stage = "resolution", recovery = "select_connection", provider = p)
      else stop(saved)
    } else if (!is.null(saved$named)) named <- saved$named
    else {
      credential <- auth$credential_provider(p)
      account_id <- saved$account_id; headers <- saved$headers
      base_url <- base_url %||% saved$base_url
    }
  }
  environment <- router$env %||% Sys.getenv()
  lm <- new_lm(p, api_key = credential, credential = named, base_url = base_url, settings = exact(router$settings) %||% list(), env = environment,
    transport = router$transport, live_connect = router$live_connect, adaptations = router$adaptations %||% "note", account_id = account_id)
  if (length(headers)) {
    have <- tolower(vapply(lm$definition$access$headers, function(h) h[[1L]], ""))
    extra <- Filter(function(k) !tolower(k) %in% have, names(headers))
    lm$definition$access$headers <- c(lm$definition$access$headers, lapply(extra, function(k) list(k, headers[[k]])))
  }
  lm
}
