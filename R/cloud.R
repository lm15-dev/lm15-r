.cloud_context <- function(provider, env = NULL, settings = list(), transport = NULL, run = NULL, clock = Sys.time) {
  structure(list(provider = provider, env = env %||% Sys.getenv(), settings = settings, transport = transport, run = run, clock = clock), class = "lm15_cloud_context")
}
print.lm15_cloud_context <- function(x, ...) { cat("<lm15 cloud credential context: redacted>\n"); invisible(x) }
str.lm15_cloud_context <- function(object, ...) { print.lm15_cloud_context(object); invisible(NULL) }
.cloud_env <- function(ctx, key, default = NULL) { value <- .auth_env(ctx$env, key); if (nzchar(value)) value else default }
.cloud_path <- function(ctx, path) {
  if (is.null(path)) return(NULL)
  if (startsWith(path, "~/")) {
    home <- .auth_home(ctx$env)
    if (is.null(home)) return(NULL)
    return(file.path(home, substring(path, 3L)))
  }
  path
}
.cloud_read <- function(ctx, path) {
  path <- .cloud_path(ctx, path)
  if (is.null(path)) return(NULL)
  tryCatch({
    size <- file.info(path)$size
    if (is.na(size) || size > 1024^2) return(NULL)
    con <- file(path, "rb"); on.exit(close(con), add = TRUE)
    rawToChar(readBin(con, "raw", n = size))
  }, error = function(e) NULL)
}
.cloud_json <- function(ctx, path, strict = FALSE) {
  text <- .cloud_read(ctx, path)
  tryCatch(if (is.null(text)) NULL else .wire_object(.json_decode(text)), error = function(e) {
    if (strict) .abort("Configured cloud credential file is malformed; no other identity was selected.", "not_configured", ctx$provider)
    NULL
  })
}
.cloud_ini <- function(ctx, path) {
  text <- .cloud_read(ctx, path)
  if (is.null(text)) return(list())
  out <- list(); section <- NULL
  fail <- function() .abort("Malformed AWS profile configuration; contents are not shown.", "not_configured", ctx$provider)
  for (line in strsplit(text, "\r?\n", perl = TRUE)[[1L]]) {
    line <- trimws(line)
    if (!nzchar(line) || startsWith(line, "#") || startsWith(line, ";")) next
    if (startsWith(line, "[")) {
      if (!endsWith(line, "]")) fail()
      section <- substr(line, 2L, nchar(line) - 1L)
      if (!is.null(out[[section]])) fail()
      out[[section]] <- list(); next
    }
    at <- regexpr("[=:]", line)[[1L]]
    if (is.null(section) || at < 2L) fail()
    key <- tolower(trimws(substr(line, 1L, at - 1L)))
    if (!is.null(out[[section]][[key]])) fail()
    out[[section]][[key]] <- trimws(substring(line, at + 1L))
  }
  out
}
.cloud_aws_files <- function(ctx) {
  profile <- .cloud_env(ctx, "AWS_PROFILE", "default")
  credentials <- .cloud_ini(ctx, .cloud_env(ctx, "AWS_SHARED_CREDENTIALS_FILE", "~/.aws/credentials"))
  config <- .cloud_ini(ctx, .cloud_env(ctx, "AWS_CONFIG_FILE", "~/.aws/config"))
  list(profile = profile, credentials = credentials, config = config,
    section = config[[if (profile == "default") profile else paste("profile", profile)]] %||% config[[profile]] %||% list())
}
.cloud_aws_static <- function(section) {
  if (is.null(section$aws_access_key_id) || !nzchar(section$aws_access_key_id) || is.null(section$aws_secret_access_key) || !nzchar(section$aws_secret_access_key)) return(NULL)
  aws_credentials(section$aws_access_key_id, section$aws_secret_access_key, session_token = section$aws_session_token)
}
.cloud_adc_path <- function(ctx) file.path(.cloud_env(ctx, "CLOUDSDK_CONFIG", "~/.config/gcloud"), "application_default_credentials.json")
.cloud_on_path <- function(ctx, command, executable = FALSE) {
  paths <- if (grepl("[/\\\\]", command)) command else file.path(strsplit(.cloud_env(ctx, "PATH", ""), .Platform$path.sep, fixed = TRUE)[[1L]], command)
  if (.Platform$OS.type == "windows") paths <- unique(c(paths, paste0(paths, ".exe"), paste0(paths, ".cmd")))
  for (path in paths) {
    path <- .cloud_path(ctx, path)
    if (!is.null(path) && file.exists(path) && !dir.exists(path) && (!executable || file.access(path, 1L) == 0L)) return(path)
  }
  NULL
}
.cloud_command <- function(ctx, argv) {
  if (!is.null(ctx$run)) return(ctx$run(argv, ctx$env))
  if (!requireNamespace("processx", quietly = TRUE)) .abort("Cloud CLI credentials require the processx R package.", "not_configured", ctx$provider)
  command <- .cloud_on_path(ctx, argv[[1L]], executable = TRUE)
  if (is.null(command)) .abort("Configured cloud credential command is not available.", "not_configured", ctx$provider)
  result <- tryCatch(processx::run(command, argv[-1L], env = ctx$env, timeout = 30000, error_on_status = FALSE, echo = FALSE), error = function(e) .auth_failure(ctx$provider))
  if (result$status != 0 || nchar(result$stdout, type = "bytes") > 1024^2) .auth_failure(ctx$provider)
  result$stdout
}
.cloud_http <- function(ctx, method, url, headers = list(), body = raw(), metadata = FALSE) {
  if (!grepl("^https?://[^/@?#[:space:]]+(?:/[^#[:space:]]*)?$", url, perl = TRUE)) .abort("Invalid credential endpoint URL.", "not_configured", ctx$provider)
  if (!metadata && !startsWith(url, "https://")) .abort("Token endpoints must use HTTPS.", "not_configured", ctx$provider)
  transport <- ctx$transport %||% transport_curl(timeout = if (metadata) 5 else 30, connect_timeout = if (metadata) 1 else 10, max_response_bytes = 1024^2)
  tryCatch(transport(structure(list(method = method, url = url, headers = headers, body = body), class = "lm15_wire_request")), error = function(e) .auth_failure(ctx$provider))
}
.cloud_token <- function(ctx, request, rung) {
  reply <- .token_http(request, ctx$transport)
  token_exchange_parse(ctx$provider, rung, reply$status, reply$body, now = ctx$clock())
}
.cloud_container_url <- function(ctx) {
  relative <- .cloud_env(ctx, "AWS_CONTAINER_CREDENTIALS_RELATIVE_URI")
  if (!is.null(relative)) {
    if (!startsWith(relative, "/") || startsWith(relative, "//")) .abort("Invalid container credential path.", "not_configured", ctx$provider)
    return(paste0("http://169.254.170.2", relative))
  }
  url <- .cloud_env(ctx, "AWS_CONTAINER_CREDENTIALS_FULL_URI")
  if (is.null(url)) return(NULL)
  if (!grepl("^https://[^/@?#]+/|^http://(?:localhost|127\\.[0-9.]+|169\\.254\\.170\\.(2|23)|\\[::1\\]|\\[fd00:ec2::23\\])(?::[0-9]+)?/", paste0(sub("/+$", "", url), "/"), perl = TRUE)) .abort("Container credential endpoint is not on the allowed host list.", "not_configured", ctx$provider)
  url
}

# The same ordered rungs drive offline reports and actual acquisition.
.cloud_rungs <- function(ctx, on_rung = NULL) {
  policy <- .definition(ctx$provider)$access; out <- list()
  add <- function(name, present, value = NULL) {
    rung <- list(name = name, state = if (!present) "absent" else if (is.null(value)) "unprobed" else "selected", value = value)
    if (!is.null(on_rung)) on_rung(rung)
    out[[length(out) + 1L]] <<- rung
  }
  key <- if (length(policy$env_keys)) policy$env_keys[[1L]] else NULL
  if (!is.null(key)) {
    value <- .cloud_env(ctx, key)
    add(paste0("env:", key), !is.null(value), if (!is.null(value)) if (key == "AWS_BEARER_TOKEN_BEDROCK") bearer_token(value) else api_key(value))
  }
  if (policy$credential_policy == "aws-chain") {
    if (!is.null(on_rung) && !is.null(.cloud_env(ctx, "AWS_ACCESS_KEY_ID")) && is.null(.cloud_env(ctx, "AWS_SECRET_ACCESS_KEY"))) .abort("AWS_ACCESS_KEY_ID is set without AWS_SECRET_ACCESS_KEY; no other identity was selected.", "not_configured", ctx$provider)
    value <- .cloud_aws_static(list(aws_access_key_id = .cloud_env(ctx, "AWS_ACCESS_KEY_ID"), aws_secret_access_key = .cloud_env(ctx, "AWS_SECRET_ACCESS_KEY"), aws_session_token = .cloud_env(ctx, "AWS_SESSION_TOKEN")))
    add("env:AWS_ACCESS_KEY_ID", !is.null(value), value)
    f <- .cloud_aws_files(ctx); s <- f$section
    add("assume-role", !is.null(s$role_arn) && (!is.null(s$source_profile) || !is.null(s$credential_source)))
    add("web-identity", !is.null(.cloud_env(ctx, "AWS_ROLE_ARN", s$role_arn)) && !is.null(.cloud_env(ctx, "AWS_WEB_IDENTITY_TOKEN_FILE", s$web_identity_token_file)))
    add("sso", !is.null(s$sso_account_id) && !is.null(s$sso_role_name) && (!is.null(s$sso_session) || !is.null(s$sso_start_url)))
    value <- .cloud_aws_static(f$credentials[[f$profile]])
    add("shared-credentials-file", !is.null(value), value)
    login <- .cloud_login_cached(ctx, s)
    add("login", !is.null(s$login_session), if (!is.null(login) && !credential_expired(login, now = ctx$clock())) login)
    command <- if (!is.null(s$credential_process)) .cloud_split_command(s$credential_process) else character()
    add("credential_process", length(command) > 0 && !is.null(.cloud_on_path(ctx, command[[1L]])))
    value <- .cloud_aws_static(s); add("config-file", !is.null(value), value)
    add("container", !is.null(.cloud_container_url(ctx)))
    add("imds", tolower(.cloud_env(ctx, "AWS_EC2_METADATA_DISABLED", "false")) != "true")
  } else if (policy$credential_policy == "azure-chain") {
    narrowed <- function(name, developer = FALSE) {
      value <- tolower(.cloud_env(ctx, "AZURE_TOKEN_CREDENTIALS", ""))
      if (!nzchar(value)) return(FALSE)
      if (value == "prod") return(developer)
      if (value == "dev") return(!developer)
      value != tolower(name)
    }
    identity <- !is.null(.cloud_env(ctx, "AZURE_TENANT_ID")) && !is.null(.cloud_env(ctx, "AZURE_CLIENT_ID"))
    add("environment", !narrowed("EnvironmentCredential") && identity && (!is.null(.cloud_env(ctx, "AZURE_CLIENT_SECRET")) || !is.null(.cloud_env(ctx, "AZURE_CLIENT_CERTIFICATE_PATH"))))
    add("workload-identity", !narrowed("WorkloadIdentityCredential") && identity && !is.null(.cloud_env(ctx, "AZURE_FEDERATED_TOKEN_FILE")))
    add("managed-identity", !narrowed("ManagedIdentityCredential"))
    names <- c(az = "AzureCliCredential", pwsh = "AzurePowerShellCredential", azd = "AzureDeveloperCliCredential")
    for (command in names(names)) add(command, !narrowed(names[[command]], TRUE) && !is.null(.cloud_on_path(ctx, command)))
  } else if (policy$credential_policy == "gcp-chain") {
    add("adc-env", !is.null(.cloud_json(ctx, .cloud_env(ctx, "GOOGLE_APPLICATION_CREDENTIALS"), strict = !is.null(on_rung))))
    add("adc-file", !is.null(.cloud_json(ctx, .cloud_adc_path(ctx), strict = !is.null(on_rung))))
    add("metadata", !tolower(.cloud_env(ctx, "NO_GCE_CHECK", "")) %in% c("1", "true"))
    add("gcloud", !is.null(.cloud_on_path(ctx, "gcloud")))
  } else .abort("Provider does not have a cloud credential chain.", "not_configured", ctx$provider)
  out
}
.cloud_split_command <- function(command) {
  # Parse arguments without asking a shell to expand or execute syntax.
  chars <- strsplit(command, "", fixed = TRUE)[[1L]]; quote <- ""; escape <- FALSE; token <- ""; active <- FALSE; out <- character()
  for (ch in chars) {
    if (escape) { token <- paste0(token, ch); escape <- FALSE; active <- TRUE; next }
    if (ch == "\\" && quote != "'") { escape <- TRUE; active <- TRUE; next }
    if (nzchar(quote)) { if (ch == quote) quote <- "" else token <- paste0(token, ch); next }
    if (ch %in% c("'", '"')) { quote <- ch; active <- TRUE; next }
    if (ch %in% c(" ", "\t", "\n")) {
      if (active) { out <- c(out, token); token <- ""; active <- FALSE }
    } else { token <- paste0(token, ch); active <- TRUE }
  }
  if (escape || nzchar(quote)) .abort("Malformed credential_process arguments.", "not_configured")
  if (active) out <- c(out, token)
  out
}
.explain_cloud_auth <- function(provider, api_keys, env, settings, now) {
  ctx <- .cloud_context(provider, env, settings, clock = function() now)
  selected <- !is.null(.explicit_source(provider, api_keys))
  steps <- list(list(kind = "api_keys", state = if (selected) "selected" else "absent", detail = "values hidden"))
  for (rung in .cloud_rungs(ctx)) {
    state <- if (selected && rung$state != "absent") "shadowed" else rung$state
    steps[[length(steps) + 1L]] <- list(kind = rung$name, state = state, detail = if (state == "unprobed") "checked at request time, not contacted by this report" else "values hidden")
    if (state == "selected") selected <- TRUE
  }
  structure(list(provider = provider, configured = selected || any(vapply(steps, function(s) s$state == "unprobed", logical(1))), steps = steps, settings = .cloud_report_settings(ctx)), class = "lm15_auth_report")
}

cloud_credential_provider <- function(provider, ..., env = NULL, settings = list(), transport = NULL, run = NULL, clock = Sys.time) {
  .check_dots(...); provider <- canonical_provider(provider)
  ctx <- .cloud_context(provider, env, settings, transport, run, clock)
  cached <- NULL
  structure(function() {
    if (!is.null(cached) && !credential_expired(cached, now = clock())) return(cached)
    developer_failed <- FALSE
    visit <- function(rung) {
      if (rung$state == "absent") return(invisible(NULL))
      value <- tryCatch(rung$value %||% .cloud_acquire(ctx, rung$name), AuthError = function(e) {
        if (.definition(provider)$access$credential_policy == "azure-chain" && rung$name %in% c("az", "pwsh", "azd")) { developer_failed <<- TRUE; return(NULL) }
        stop(e)
      })
      if (is.null(value)) return(invisible(NULL))
      if (credential_expired(value, now = clock())) .auth_failure(provider, "Cloud source returned expired credentials; renew the selected source.")
      if (value$kind != "bearer_token" || !is.null(value$expires_at)) cached <<- value
      invokeRestart("cloud_credential_selected", value)
    }
    withRestarts({
      .cloud_rungs(ctx, on_rung = visit)
      if (developer_failed) .auth_failure(provider, "Azure developer credentials failed. Sign in with az, PowerShell, or azd.")
      .abort("No cloud credential source supplied a usable credential.", "not_configured", provider)
    }, cloud_credential_selected = identity)
  }, class = c("lm15_credential", "function"))
}
