.cloud_acquire <- function(ctx, rung) {
  policy <- .definition(ctx$provider)$access$credential_policy
  switch(policy, `aws-chain` = .cloud_aws_acquire(ctx, rung), `azure-chain` = .cloud_azure_acquire(ctx, rung), `gcp-chain` = .cloud_gcp_acquire(ctx, rung))
}
.cloud_azure_acquire <- function(ctx, rung) {
  env <- as.list(ctx$env); scope <- ctx$settings$scope %||% "https://ai.azure.com/.default"
  if (rung %in% c("environment", "workload-identity")) {
    input <- json_object(env = .json_object(env), settings = .json_object(ctx$settings))
    if (rung == "workload-identity") {
      input$client_assertion <- .cloud_read(ctx, .cloud_env(ctx, "AZURE_FEDERATED_TOKEN_FILE"))
      if (is.null(input$client_assertion)) .abort("Federated token file is unreadable.", "not_configured", ctx$provider)
    } else if (is.null(.cloud_env(ctx, "AZURE_CLIENT_SECRET"))) {
      pem <- .cloud_read(ctx, .cloud_env(ctx, "AZURE_CLIENT_CERTIFICATE_PATH"))
      if (is.null(pem)) .abort("Azure certificate file is unreadable. Convert PKCS#12 to PEM with openssl pkcs12 -nodes.", "not_configured", ctx$provider)
      input$certificate_pem <- pem; input$private_key_pem <- pem
      password <- .cloud_env(ctx, "AZURE_CLIENT_CERTIFICATE_PASSWORD")
      if (!is.null(password)) {
        .crypto()
        key <- tryCatch(openssl::read_key(pem, password = password), error = function(e) .auth_failure(ctx$provider))
        input$private_key_pem <- as.character(openssl::write_pem(key))
      }
    }
    return(.cloud_token(ctx, token_exchange_build(ctx$provider, rung, input, settings = ctx$settings, now = ctx$clock()), rung))
  }
  resource <- sub("/\\.default$", "", scope)
  if (rung == "managed-identity") return(.cloud_azure_managed(ctx, resource))
  if (rung == "az") {
    argv <- c("az", "account", "get-access-token", "--output", "json", "--scope", scope)
    tenant <- .cloud_env(ctx, "AZURE_TENANT_ID")
    if (!is.null(tenant)) argv <- c(argv, "--tenant", tenant)
    data <- .json_decode(.cloud_command(ctx, argv)); data$access_token <- data$accessToken
    if (is.null(data$access_token)) return(NULL)
    if (is.null(data$expires_on) && !is.null(data$expiresOn)) {
      expiry <- suppressWarnings(as.POSIXct(data$expiresOn, tz = ""))
      if (!is.na(expiry)) data$expires_on <- as.numeric(expiry)
    }
  } else if (rung == "pwsh") {
    script <- paste0("Get-AzAccessToken -ResourceUrl '", gsub("'", "''", resource, fixed = TRUE), "' -AsSecureString:$false | ConvertTo-Json -Compress")
    data <- .json_decode(.cloud_command(ctx, c("pwsh", "-NoProfile", "-NonInteractive", "-Command", script)))
    data$access_token <- data$Token
    if (is.null(data$access_token)) return(NULL)
  } else if (rung == "azd") {
    data <- .json_decode(.cloud_command(ctx, c("azd", "auth", "token", "--output", "json", "--scope", scope)))
    data$access_token <- data$token; data$expires_at <- data$expiresOn
    if (is.null(data$access_token)) return(NULL)
  } else .unsupported(ctx$provider, paste("Azure credential source", rung))
  token_exchange_parse(ctx$provider, rung, 200, data, now = ctx$clock())
}
.cloud_query <- function(url, params) paste0(url, if (grepl("?", url, fixed = TRUE)) "&" else "?", rawToChar(.form_body(params)))
.cloud_azure_managed <- function(ctx, resource) {
  env <- function(name) .cloud_env(ctx, name)
  client <- env("AZURE_CLIENT_ID"); endpoint <- env("IDENTITY_ENDPOINT"); msi <- env("MSI_ENDPOINT")
  headers <- list(); body <- raw(); method <- "GET"
  if (!is.null(endpoint) && !is.null(env("IDENTITY_HEADER")) && !is.null(env("IDENTITY_SERVER_THUMBPRINT"))) .abort("Service Fabric certificate pinning is not supported; use an explicit Azure credential.", "not_configured", ctx$provider)
  if (!is.null(endpoint) && !is.null(env("IDENTITY_HEADER"))) {
    params <- json_object(`api-version` = "2019-08-01", resource = resource)
    if (!is.null(client)) params$client_id <- client
    url <- .cloud_query(endpoint, params); headers <- list("X-IDENTITY-HEADER" = env("IDENTITY_HEADER"))
  } else if (!is.null(endpoint) && !is.null(env("IMDS_ENDPOINT"))) {
    url <- .cloud_query(endpoint, json_object(`api-version` = "2019-11-01", resource = resource))
    challenge <- .cloud_http(ctx, "GET", url, list(Metadata = "true"), metadata = TRUE)
    header <- challenge$headers[["www-authenticate"]] %||% ""
    if (challenge$status != 401 || !grepl("realm=", header, fixed = TRUE)) .auth_failure(ctx$provider)
    path <- gsub('^"|"$', "", trimws(sub("^.*realm=", "", header)))
    directory <- if (.Platform$OS.type == "windows") file.path(.cloud_env(ctx, "PROGRAMDATA", "C:/ProgramData"), "AzureConnectedMachineAgent", "Tokens") else "/var/opt/azcmagent/tokens"
    if (!endsWith(path, ".key") || !identical(normalizePath(dirname(path), mustWork = FALSE), normalizePath(directory, mustWork = FALSE)) || !identical(dirname(normalizePath(path, mustWork = FALSE)), normalizePath(directory, mustWork = FALSE))) .auth_failure(ctx$provider, "Azure Arc returned an invalid challenge location.")
    secret <- .cloud_read(ctx, path)
    if (is.null(secret) || nchar(secret, type = "bytes") > 4096) .auth_failure(ctx$provider)
    headers <- list(Metadata = "true", Authorization = paste("Basic", trimws(secret)))
  } else if (!is.null(msi) && !is.null(env("MSI_SECRET"))) {
    params <- json_object(`api-version` = "2017-09-01", resource = resource)
    if (!is.null(client)) params$clientid <- client
    url <- .cloud_query(msi, params); headers <- list(secret = env("MSI_SECRET"))
  } else if (!is.null(msi)) {
    url <- msi; method <- "POST"; body <- .form_body(json_object(resource = resource))
    headers <- list(Metadata = "true", "content-type" = "application/x-www-form-urlencoded")
  } else {
    params <- json_object(`api-version` = "2018-02-01", resource = resource)
    if (!is.null(client)) params$client_id <- client
    url <- .cloud_query("http://169.254.169.254/metadata/identity/oauth2/token", params)
    headers <- list(Metadata = "true")
    probe <- tryCatch(.cloud_http(ctx, "GET", url, headers, metadata = TRUE), error = function(e) NULL)
    if (is.null(probe) || probe$status %in% c(400, 404)) return(NULL)
    return(token_exchange_parse(ctx$provider, "managed-identity", probe$status, .decode_body(probe$body), now = ctx$clock()))
  }
  reply <- .cloud_http(ctx, method, url, headers, body, metadata = TRUE)
  token_exchange_parse(ctx$provider, "managed-identity", reply$status, .decode_body(reply$body), now = ctx$clock())
}

.cloud_gcp_info <- function(ctx, info, depth = 0L) {
  if (depth > 8L) .abort("GCP credential nesting is too deep.", "not_configured", ctx$provider)
  kind <- info$type %||% ""
  if (kind %in% c("authorized_user", "service_account")) return(.cloud_token(ctx, token_exchange_build(ctx$provider, "adc-env", json_object(credential_file = info), now = ctx$clock()), "adc-env"))
  if (kind == "impersonated_service_account") {
    if (!.is_object(info$source_credentials)) .abort("GCP impersonation requires source_credentials.", "not_configured", ctx$provider)
    base <- .cloud_gcp_info(ctx, info$source_credentials, depth + 1L)
    return(.cloud_gcp_impersonate(ctx, base, info$service_account_impersonation_url, info$delegates %||% list()))
  }
  if (kind != "external_account") .abort("Unsupported GCP credential file type; configure a service account, authorized user, external account, or impersonation.", "not_configured", ctx$provider)
  source <- .wire_object(info$credential_source); fmt <- .wire_object(source$format)
  if (!is.null(source$environment_id)) .abort("An AWS external-account source is not supported; use a file, URL, or executable credential source.", "not_configured", ctx$provider)
  subject <- NULL
  if (!is.null(source$file)) subject <- .cloud_read(ctx, source$file)
  else if (!is.null(source$url)) {
    reply <- .cloud_http(ctx, "GET", source$url, source$headers %||% list(), metadata = TRUE)
    if (reply$status < 200 || reply$status >= 300) .auth_failure(ctx$provider)
    subject <- rawToChar(reply$body)
  } else if (.is_object(source$executable)) {
    if (!identical(.cloud_env(ctx, "GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES"), "1")) .abort("Executable credential sources require GOOGLE_EXTERNAL_ACCOUNT_ALLOW_EXECUTABLES=1.", "not_configured", ctx$provider)
    data <- .json_decode(.cloud_command(ctx, .cloud_split_command(source$executable$command)))
    if (identical(data$success, FALSE)) .auth_failure(ctx$provider)
    subject <- data$id_token %||% data$saml_response; fmt <- json_object(type = "text")
  }
  if (is.null(subject)) .abort("External-account subject token is unavailable.", "not_configured", ctx$provider)
  subject <- trimws(subject)
  if (identical(fmt$type, "json")) subject <- .json_decode(subject)[[fmt$subject_token_field_name]]
  if (!is.character(subject) || length(subject) != 1L || !nzchar(subject)) .auth_failure(ctx$provider)
  req <- .token_request(info$token_url %||% "https://sts.googleapis.com/v1/token", json_object(grantType = "urn:ietf:params:oauth:grant-type:token-exchange", audience = info$audience, scope = "https://www.googleapis.com/auth/cloud-platform", requestedTokenType = "urn:ietf:params:oauth:token-type:access_token", subjectToken = subject, subjectTokenType = info$subject_token_type), "json")
  token <- .cloud_token(ctx, req, "adc-env")
  if (!is.null(info$service_account_impersonation_url)) token <- .cloud_gcp_impersonate(ctx, token, info$service_account_impersonation_url, list())
  token
}
.cloud_gcp_impersonate <- function(ctx, source, url, delegates) {
  req <- .token_request(url, json_object(delegates = delegates, scope = list("https://www.googleapis.com/auth/cloud-platform"), lifetime = "3600s"), "json", headers = list(authorization = paste("Bearer", source$value)))
  reply <- .token_http(req, ctx$transport)
  token_exchange_parse(ctx$provider, "adc-env", reply$status, reply$body, now = ctx$clock())
}
.cloud_gcp_acquire <- function(ctx, rung) {
  if (rung %in% c("adc-env", "adc-file")) {
    path <- if (rung == "adc-env") .cloud_env(ctx, "GOOGLE_APPLICATION_CREDENTIALS") else .cloud_adc_path(ctx)
    info <- .cloud_json(ctx, path, strict = TRUE)
    if (is.null(info)) .abort("Selected GCP credential file is unreadable.", "not_configured", ctx$provider)
    return(.cloud_gcp_info(ctx, info))
  }
  if (rung == "gcloud") {
    token <- trimws(.cloud_command(ctx, c("gcloud", "auth", "print-access-token")))
    return(if (nzchar(token)) bearer_token(token) else NULL)
  }
  if (rung == "metadata") {
    host <- .cloud_env(ctx, "GCE_METADATA_HOST", .cloud_env(ctx, "GCE_METADATA_ROOT", "metadata.google.internal"))
    reply <- tryCatch(.cloud_http(ctx, "GET", paste0("http://", host, "/computeMetadata/v1/instance/service-accounts/default/token"), list("Metadata-Flavor" = "Google"), metadata = TRUE), error = function(e) NULL)
    if (is.null(reply) || reply$status != 200) return(NULL)
    return(token_exchange_parse(ctx$provider, rung, reply$status, .decode_body(reply$body), now = ctx$clock()))
  }
  .unsupported(ctx$provider, paste("GCP credential source", rung))
}
