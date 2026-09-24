.cloud_profile_setting <- function(ctx, name) {
  policy <- .definition(ctx$provider)$access$credential_policy
  if (policy == "aws-chain" && name == "region") {
    f <- .cloud_aws_files(ctx)
    return(f$section$region %||% f$credentials[[f$profile]]$region)
  }
  if (policy == "gcp-chain" && name == "project") {
    for (path in c(.cloud_env(ctx, "GOOGLE_APPLICATION_CREDENTIALS"), .cloud_adc_path(ctx))) {
      info <- .cloud_json(ctx, path)
      value <- info$quota_project_id %||% info$project_id
      if (!is.null(value)) return(value)
    }
  }
  NULL
}
.cloud_report_settings <- function(ctx) {
  out <- ctx$settings
  for (s in .definition(ctx$provider)$access$host$settings %||% list()) {
    value <- out[[s$name]]
    if (is.null(value) && s$name == "resource") value <- .cloud_resource_endpoint(.definition(ctx$provider), ctx$env)
    if (is.null(value)) for (key in unlist(s$env)) {
      value <- .cloud_env(ctx, key)
      if (!is.null(value)) break
    }
    value <- value %||% .cloud_profile_setting(ctx, s$name) %||% s$default
    if (!is.null(value)) out[[s$name]] <- value
  }
  out
}
