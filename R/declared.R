# provider_definition(): declare a provider lm15 does not list, for one router
# (new_router(providers = )). Documented in tools/reference.R.
provider_definition <- function(id, ..., dialect = c("openai-chat", "openai-responses", "anthropic"), base_url,
                                env_keys = character(), compat = list(), supports = c("complete", "stream"),
                                auth_scheme = c("bearer", "x-api-key"), headers = character(), aliases = character(),
                                placeholder_key = NULL, console_url = NULL, note = "") {
  .check_dots(...)
  .string(id, "id")
  if (!identical(id, canonical_provider(id)) || grepl("[:/[:space:]]", id) || !identical(id, tolower(id)))
    stop("id must be a lower-case provider name with words joined by '-', such as \"nebius\".", call. = FALSE)
  dialect <- match.arg(dialect)
  auth_scheme <- match.arg(auth_scheme)
  if (missing(base_url)) stop("A declared provider names its base_url.", call. = FALSE)
  .string(base_url, "base_url")
  if (!grepl("^https?://[^/?#[:space:]@]+(?:/[^?#[:space:]]*)?$", base_url, perl = TRUE))
    stop("base_url must be an HTTP(S) root without userinfo, whitespace, query, or fragment.", call. = FALSE)
  if (!is.character(env_keys) || anyNA(env_keys) || any(!nzchar(env_keys)) || anyDuplicated(env_keys))
    stop("env_keys must be distinct, non-empty variable names.", call. = FALSE)
  if (!is.null(placeholder_key)) {
    .string(placeholder_key, "placeholder_key")
    if (length(env_keys)) stop("A keyless local server (placeholder_key) declares no env_keys.", call. = FALSE)
  }
  surfaces <- c("complete", "stream", "live", "files", "batches", "images", "speech", "video", "responses_api", "models", "caches")
  if (!is.character(supports) || anyNA(supports) || any(!supports %in% surfaces))
    stop(paste0("supports must name surfaces among: ", paste(surfaces, collapse = ", "), "."), call. = FALSE)
  if (!is.character(headers) || (length(headers) && (is.null(names(headers)) || any(!nzchar(names(headers))))))
    stop("headers must be a named character vector.", call. = FALSE)
  if (!is.character(aliases) || anyNA(aliases) || any(!nzchar(aliases)) || any(aliases != gsub("_", "-", aliases, fixed = TRUE)) ||
      anyDuplicated(c(id, aliases)))
    stop("aliases must be distinct, non-empty provider names with words joined by '-', other than id.", call. = FALSE)
  if (!is.null(console_url)) .string(console_url, "console_url")
  .string(note, "note", empty = TRUE)
  structure(list(
    id = id, dialect = dialect, compat = .declared_compat(dialect, compat),
    access = .json_object(list(
      provider = id, supports = .json_object(setNames(as.list(surfaces %in% supports), surfaces)),
      auth_modes = list(auth_scheme), env_keys = as.list(env_keys), base_url = base_url,
      credential_policy = "key", auth_scheme = list(auth_scheme),
      headers = lapply(names(headers), function(k) list(k, unname(headers[[k]]))),
      backend = "api", backend_options = json_object())),
    placeholder_key = placeholder_key, console_url = console_url, note = note,
    aliases = as.list(aliases), declared = TRUE
  ), class = "lm15_provider_definition")
}

# A preset name, or a policy object whose every name is a knob the dialect
# declares (the reference's compat fields, copied with the provider tables).
.declared_compat <- function(dialect, compat) {
  if (is.character(compat)) {
    .string(compat, "compat")
    return(compat)
  }
  if (!is.list(compat)) stop("compat must be a preset name or a named list of knobs.", call. = FALSE)
  if (!length(compat)) return(json_object())
  key <- switch(dialect, "openai-chat" = "chat", "openai-responses" = "responses", anthropic = "anthropic")
  known <- unlist(.provider_tables()$compat_fields[[key]])
  if (is.null(names(compat)) || any(!nzchar(names(compat))))
    stop("compat must be a named list of knobs.", call. = FALSE)
  unknown <- setdiff(names(compat), known)
  if (length(unknown))
    stop(paste0("Unknown ", dialect, " compat knob(s): ", paste(unknown, collapse = ", "), ". Known: ", paste(known, collapse = ", "), "."), call. = FALSE)
  .json_object(compat)
}

print.lm15_provider_definition <- function(x, ...) {
  cat("<lm15 declared provider: ", x$id, " (", x$dialect, ") at ", x$access$base_url, ">\n", sep = "")
  invisible(x)
}

# RouterConfig(providers=...): definitions only, each spelling (id and
# aliases) naming one door that nothing built in already names. Returns the
# spelling -> id map the router resolves prefixes with.
.declared_spellings <- function(providers) {
  if (!is.list(providers) || inherits(providers, "lm15_provider_definition"))
    stop("providers must be a list of provider_definition() values.", call. = FALSE)
  builtin <- c(providers(), vapply(.declared_providers(), function(d) d$id, ""),
               gsub("_", "-", names(.provider_tables()$chat_model_prefixes), fixed = TRUE))
  taken <- character()
  for (d in providers) {
    if (!inherits(d, "lm15_provider_definition")) stop("providers must contain provider_definition() values.", call. = FALSE)
    for (spelling in c(d$id, unlist(d$aliases))) {
      if (spelling %in% builtin)
        .abort(paste0("'", spelling, "' already names one of lm15's own providers or litellm spellings; a declared provider takes a new id and aliases."), "not_configured", d$id)
      if (spelling %in% names(taken))
        .abort(paste0("'", spelling, "' is spelled by both '", taken[[spelling]], "' and '", d$id, "'."), "not_configured", d$id)
      taken[[spelling]] <- d$id
    }
  }
  taken
}
