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
