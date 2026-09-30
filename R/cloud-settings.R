# Host settings (AUTH-10): each value and where it came from.
#
# One resolver serves client construction and the offline explain_auth
# report, so the two can never disagree. The `from` vocabulary is the
# contract's: explicit, env:<VAR>, adc-env, gcloud-config, adc-file,
# metadata, aws-profile, default. A setting only the metadata server could
# supply is "unprobed" offline; a missing required setting has no origin.

.gcloud_config_name <- "^[a-z][-a-z0-9]*$"  # gcloud's own rule; keeps the name inside its directory

.gcloud_config_project <- function(ctx) {
  value <- trimws(.cloud_env(ctx, "CLOUDSDK_CORE_PROJECT", ""))
  if (nzchar(value)) return(list(value = value, from = "env:CLOUDSDK_CORE_PROJECT"))
  base <- sub("/+$", "", .cloud_env(ctx, "CLOUDSDK_CONFIG", "~/.config/gcloud"))
  name <- trimws(.cloud_env(ctx, "CLOUDSDK_ACTIVE_CONFIG_NAME", ""))
  if (!nzchar(name)) name <- trimws(.cloud_read(ctx, file.path(base, "active_config")) %||% "")
  if (!nzchar(name)) name <- "default"
  if (!grepl(.gcloud_config_name, name)) return(NULL)
  sections <- tryCatch(.cloud_ini(ctx, file.path(base, "configurations", paste0("config_", name))), error = function(e) list())
  value <- trimws(sections$core$project %||% "")
  if (nzchar(value)) list(value = value, from = "gcloud-config") else NULL
}

.gcp_metadata_project <- function(ctx, probe) {
  if (tolower(.cloud_env(ctx, "NO_GCE_CHECK", "")) %in% c("1", "true")) return(NULL)
  if (!probe) return(list(value = NULL, from = "metadata", unprobed = TRUE))
  host <- .cloud_env(ctx, "GCE_METADATA_HOST") %||% .cloud_env(ctx, "GCE_METADATA_ROOT") %||% "metadata.google.internal"
  url <- paste0("http://", host, "/computeMetadata/v1/project/project-id")
  transport <- ctx$transport %||% transport_curl(timeout = 1, connect_timeout = 1, max_response_bytes = 64 * 1024)
  reply <- tryCatch(transport(structure(list(method = "GET", url = url, headers = list("Metadata-Flavor" = "Google"), body = raw()), class = "lm15_wire_request")), error = function(e) NULL)
  if (is.null(reply) || !identical(as.integer(reply$status), 200L)) return(NULL)
  body <- reply$body
  value <- trimws(if (is.raw(body)) rawToChar(body) else as.character(body %||% ""))
  if (!nzchar(value) || grepl("[[:space:]/?#]", value)) return(NULL)
  list(value = value, from = "metadata")
}

# The value a cloud's own configuration carries, after the caller and the
# setting's env variables. `probe` allows the one network source (the
# metadata server); offline it is reported unprobed instead.
.cloud_profile_setting <- function(ctx, name, probe = FALSE) {
  policy <- .definition(ctx$provider)$access$credential_policy
  if (policy == "aws-chain" && name == "region") {
    f <- .cloud_aws_files(ctx)
    value <- f$section$region %||% f$credentials[[f$profile]]$region
    return(if (!is.null(value) && nzchar(value)) list(value = value, from = "aws-profile") else NULL)
  }
  if (policy == "gcp-chain" && name == "project") {
    path <- .cloud_env(ctx, "GOOGLE_APPLICATION_CREDENTIALS")
    if (!is.null(path)) {
      info <- .cloud_json(ctx, path)
      value <- .nonempty_string(info$project_id) %||% .nonempty_string(info$quota_project_id)
      if (!is.null(value)) return(list(value = value, from = "adc-env"))
    }
    found <- .gcloud_config_project(ctx)
    if (!is.null(found)) return(found)
    info <- .cloud_json(ctx, .cloud_adc_path(ctx))
    value <- .nonempty_string(info$quota_project_id) %||% .nonempty_string(info$project_id)
    if (!is.null(value)) return(list(value = value, from = "adc-file"))
    return(.gcp_metadata_project(ctx, probe))
  }
  NULL
}
.nonempty_string <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NULL

# Every declared setting: list(values = <name -> value>, sources = <name ->
# list(value, from, state?)>, missing = <first missing name or NULL>).
.resolve_host_settings <- function(definition, explicit, ctx, probe = FALSE) {
  values <- list(); sources <- list(); missing <- NULL
  for (s in definition$access$host$settings %||% list()) {
    value <- explicit[[s$name]]; from <- if (!is.null(value)) "explicit"
    if (is.null(value) && s$name == "resource") {
      value <- .cloud_resource_endpoint(definition, ctx$env)
      if (!is.null(value)) from <- paste0("env:", switch(definition$access$backend, `azure-openai` = "AZURE_OPENAI_ENDPOINT", "ANTHROPIC_FOUNDRY_BASE_URL"))
    }
    if (is.null(value)) for (key in unlist(s$env)) {
      candidate <- .cloud_env(ctx, key)
      if (!is.null(candidate)) { value <- candidate; from <- paste0("env:", key); break }
    }
    unprobed <- NULL
    if (is.null(value)) {
      found <- .cloud_profile_setting(ctx, s$name, probe = probe)
      if (isTRUE(found$unprobed)) unprobed <- found$from
      else if (!is.null(found)) { value <- found$value; from <- found$from }
    }
    if (is.null(value) && !is.null(s$default)) { value <- s$default; from <- "default" }
    if (!is.null(value)) {
      values[[s$name]] <- value
      sources[[s$name]] <- json_object(value = value, from = from)
    } else if (!is.null(unprobed)) {
      sources[[s$name]] <- json_object(value = NULL, from = unprobed, state = "unprobed")
    } else {
      sources[[s$name]] <- json_object(value = NULL, from = NULL)
      missing <- missing %||% s$name
    }
  }
  list(values = values, sources = sources, missing = missing)
}

.cloud_report_settings <- function(ctx) {
  d <- .definition(ctx$provider)
  resolved <- .resolve_host_settings(d, ctx$settings, ctx, probe = FALSE)
  structure(resolved$values, sources = resolved$sources)
}

# A door's backend settings (AUTH-10, amended 2026-09-30): the caller's value,
# then `lookup` (a router's environment; NULL for a client built by hand), then
# the table's backend_options value. A name the door does not declare is a
# not_configured error naming the ones it does: a setting nothing reads would
# otherwise be dropped with nothing said. Returns list(values, sources).
.resolve_backend_settings <- function(access, explicit, lookup = NULL) {
  explicit <- explicit %||% list()
  known <- vapply(access$backend_settings %||% list(), function(s) s$name, "")
  unknown <- sort(setdiff(names(explicit), known))
  if (length(unknown)) {
    hint <- if (length(known)) paste0("known: ", paste(known, collapse = ", ")) else "this door takes no settings"
    fix <- if (length(known)) paste0("Pass only ", paste(known, collapse = ", "), " for ", access$provider) else paste0("Remove the settings entry for ", access$provider)
    .abort(paste0(access$provider, ": unknown setting(s) ", paste0("'", unknown, "'", collapse = ", "), "; ", hint), "not_configured", access$provider, credential_hint = fix)
  }
  values <- list(); sources <- list()
  for (s in access$backend_settings %||% list()) {
    value <- .nonempty_string(explicit[[s$name]]); from <- "explicit"
    if (is.null(value) && !is.null(lookup)) for (key in unlist(s$env)) {
      candidate <- .nonempty_string(lookup(key))
      if (!is.null(candidate)) { value <- candidate; from <- paste0("env:", key); break }
    }
    if (is.null(value)) { value <- access$backend_options[[s$name]]; from <- "default" }
    values[[s$name]] <- value
    sources[[s$name]] <- json_object(value = value, from = from)
  }
  list(values = values, sources = sources)
}

# The policy with resolved backend settings in backend_options. client_version
# on the claude-code backend is also the version the user-agent header claims
# (claude-cli/<client_version>); on chatgpt-codex it is the /models parameter.
.with_backend_settings <- function(access, values) {
  for (name in names(values)) access$backend_options[[name]] <- values[[name]]
  if (identical(access$backend, "claude-code") && !is.null(values$client_version))
    access$headers <- lapply(access$headers, function(h) if (tolower(h[[1L]]) == "user-agent") list(h[[1L]], paste0("claude-cli/", values$client_version)) else h)
  access
}

# The claude-code door's minimum-version refusal with what an lm15 caller
# changes (AUTH-10 backend settings): the server says "run 'claude update'",
# which does not move the version lm15 claims. Other messages are unchanged.
.claude_code_version_guidance <- function(message) {
  m <- regmatches(message, regexec("Claude Code (\\S+) does not support this model; version (\\S+) or newer is required", message, perl = TRUE))[[1L]]
  if (!length(m) || grepl("\n\n  To fix:", message, fixed = TRUE)) return(message)
  required <- m[[3L]]
  paste0(message, "\n\n  To fix:\n",
    "    - lm15 sends this version itself; updating Claude Code does not change it\n",
    "    - Set the claude-code setting client_version to ", required, " or newer (or LM15_CLAUDE_CODE_VERSION=", required, ")\n")
}
