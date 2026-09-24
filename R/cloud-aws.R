.cloud_response <- function(ctx, reply) {
  if (reply$status < 200 || reply$status >= 300) .auth_failure(ctx$provider)
  tryCatch(.decode_body(reply$body), error = function(e) .auth_failure(ctx$provider))
}
.cloud_aws_response <- function(ctx, body) {
  expiry <- body$Expiration %||% body$expiration %||% body$expiresAt
  if (!is.null(expiry) && (is.numeric(expiry) || inherits(expiry, "lm15_json_number"))) expiry <- .utc_timestamp(.number(expiry, "expiry") / if (.number(expiry, "expiry") > 1e11) 1000 else 1)
  canonical <- json_object(AccessKeyId = body$AccessKeyId %||% body$accessKeyId, SecretAccessKey = body$SecretAccessKey %||% body$secretAccessKey, SessionToken = body$SessionToken %||% body$Token %||% body$sessionToken, Expiration = expiry)
  token_exchange_parse(ctx$provider, "imds", 200, canonical, now = ctx$clock())
}
.cloud_sts <- function(ctx, pairs, credential = NULL) {
  region <- ctx$settings$region %||% .cloud_env(ctx, "AWS_REGION", .cloud_env(ctx, "AWS_DEFAULT_REGION", "us-east-1"))
  if (!grepl("^[a-z0-9-]+$", region)) .abort("Invalid AWS region.", "not_configured", ctx$provider)
  url <- paste0("https://sts.", region, ".amazonaws.com/"); body <- .form_body(pairs)
  headers <- list("content-type" = "application/x-www-form-urlencoded")
  if (!is.null(credential)) headers <- sigv4_sign("POST", url, credential, region = region, service = "sts", headers = headers, body = body, now = ctx$clock())$headers
  reply <- .cloud_http(ctx, "POST", url, headers, body)
  if (reply$status < 200 || reply$status >= 300) .auth_failure(ctx$provider)
  if (!requireNamespace("xml2", quietly = TRUE)) .abort("STS credentials require the xml2 R package.", "not_configured", ctx$provider)
  fields <- tryCatch({
    root <- xml2::read_xml(reply$body, options = "NONET")
    node <- xml2::xml_find_first(root, "//*[local-name()='Credentials']")
    if (inherits(node, "xml_missing")) .auth_failure(ctx$provider)
    out <- json_object()
    for (name in c("AccessKeyId", "SecretAccessKey", "SessionToken", "Expiration")) {
      value <- xml2::xml_text(xml2::xml_find_first(node, paste0("./*[local-name()='", name, "']")))
      if (!is.na(value) && nzchar(value)) out[[name]] <- value
    }
    out
  }, error = function(e) .auth_failure(ctx$provider))
  .cloud_aws_response(ctx, fields)
}
.cloud_session_name <- function() { .crypto(); paste0("lm15-", paste(sprintf("%02x", as.integer(openssl::rand_bytes(6L))), collapse = "")) }
.cloud_assume_role <- function(ctx, section, seen = character()) {
  if (length(seen) > 5L) .abort("AWS role chain is too deep or cyclic.", "not_configured", ctx$provider)
  source <- NULL
  if (!is.null(section$source_profile)) {
    profile <- section$source_profile
    if (profile %in% seen) .abort("AWS role chain contains a cycle.", "not_configured", ctx$provider)
    f <- .cloud_aws_files(ctx)
    sub <- f$config[[if (profile == "default") profile else paste("profile", profile)]] %||% f$config[[profile]] %||% list()
    static <- f$credentials[[profile]]
    if (length(static)) sub[names(static)] <- static
    source <- if (!is.null(sub$role_arn)) .cloud_assume_role(ctx, sub, c(seen, profile)) else .cloud_aws_static(sub)
  } else {
    source <- switch(section$credential_source %||% "", Environment = .cloud_aws_static(list(aws_access_key_id = .cloud_env(ctx, "AWS_ACCESS_KEY_ID"), aws_secret_access_key = .cloud_env(ctx, "AWS_SECRET_ACCESS_KEY"), aws_session_token = .cloud_env(ctx, "AWS_SESSION_TOKEN"))), EcsContainer = .cloud_aws_acquire(ctx, "container"), Ec2InstanceMetadata = .cloud_aws_acquire(ctx, "imds"))
  }
  if (is.null(source)) .abort("AWS assume-role source has no usable credential.", "not_configured", ctx$provider)
  pairs <- json_object(Action = "AssumeRole", Version = "2011-06-15", RoleArn = section$role_arn, RoleSessionName = section$role_session_name %||% .cloud_session_name())
  if (!is.null(section$external_id)) pairs$ExternalId <- section$external_id
  if (!is.null(section$duration_seconds)) pairs$DurationSeconds <- section$duration_seconds
  .cloud_sts(ctx, pairs, source)
}
.cloud_login_cached <- function(ctx, section) {
  if (is.null(section$login_session)) return(NULL)
  directory <- .cloud_env(ctx, "AWS_LOGIN_CACHE_DIRECTORY", "~/.aws/login/cache")
  body <- .cloud_json(ctx, file.path(directory, paste0(.hex_hash(charToRaw(section$login_session)), ".json")))
  if (is.null(body$accessToken$accessKeyId)) return(NULL)
  .cloud_aws_response(ctx, body$accessToken)
}
.cloud_aws_acquire <- function(ctx, rung) {
  if (rung == "container") {
    url <- .cloud_container_url(ctx)
    if (is.null(url)) return(NULL)
    token <- .cloud_env(ctx, "AWS_CONTAINER_AUTHORIZATION_TOKEN") %||% .cloud_read(ctx, .cloud_env(ctx, "AWS_CONTAINER_AUTHORIZATION_TOKEN_FILE"))
    headers <- if (!is.null(token)) list(authorization = trimws(token)) else list()
    return(.cloud_aws_response(ctx, .cloud_response(ctx, .cloud_http(ctx, "GET", url, headers, metadata = TRUE))))
  }
  if (rung == "imds") {
    if (tolower(.cloud_env(ctx, "AWS_EC2_METADATA_DISABLED", "false")) == "true") return(NULL)
    base <- sub("/+$", "", .cloud_env(ctx, "AWS_EC2_METADATA_SERVICE_ENDPOINT", if (tolower(.cloud_env(ctx, "AWS_EC2_METADATA_SERVICE_ENDPOINT_MODE", "")) == "ipv6") "http://[fd00:ec2::254]" else "http://169.254.169.254"))
    token <- tryCatch(.cloud_http(ctx, "PUT", paste0(base, "/latest/api/token"), list("X-aws-ec2-metadata-token-ttl-seconds" = "21600"), metadata = TRUE), error = function(e) NULL)
    if (is.null(token) || token$status != 200) return(NULL)
    headers <- list("X-aws-ec2-metadata-token" = rawToChar(token$body))
    role <- .cloud_http(ctx, "GET", paste0(base, "/latest/meta-data/iam/security-credentials/"), headers, metadata = TRUE)
    if (role$status != 200 || !length(role$body)) return(NULL)
    name <- strsplit(trimws(rawToChar(role$body)), "\n", fixed = TRUE)[[1L]][[1L]]
    reply <- .cloud_http(ctx, "GET", paste0(base, "/latest/meta-data/iam/security-credentials/", .path_id(name)), headers, metadata = TRUE)
    if (reply$status != 200) return(NULL)
    return(.cloud_aws_response(ctx, .cloud_response(ctx, reply)))
  }
  f <- .cloud_aws_files(ctx); s <- f$section
  if (rung == "assume-role") return(.cloud_assume_role(ctx, s))
  if (rung == "web-identity") {
    file <- .cloud_env(ctx, "AWS_WEB_IDENTITY_TOKEN_FILE", s$web_identity_token_file)
    role <- .cloud_env(ctx, "AWS_ROLE_ARN", s$role_arn)
    token <- .cloud_read(ctx, file)
    if (is.null(token) || !nzchar(trimws(token))) .abort("Web identity token file is missing or empty.", "not_configured", ctx$provider)
    pairs <- json_object(Action = "AssumeRoleWithWebIdentity", Version = "2011-06-15", RoleArn = role, RoleSessionName = .cloud_env(ctx, "AWS_ROLE_SESSION_NAME", s$role_session_name) %||% .cloud_session_name(), WebIdentityToken = trimws(token))
    return(.cloud_sts(ctx, pairs))
  }
  if (rung == "credential_process") {
    body <- tryCatch(.json_decode(.cloud_command(ctx, .cloud_split_command(s$credential_process))), error = function(e) .auth_failure(ctx$provider))
    return(token_exchange_parse(ctx$provider, rung, 0, body, now = ctx$clock()))
  }
  if (rung == "login") {
    value <- .cloud_login_cached(ctx, s)
    if (!is.null(value) && !credential_expired(value, now = ctx$clock())) return(value)
    .abort("AWS login session requires renewal with `aws login`.", "not_configured", ctx$provider, credential_hint = "aws login")
  }
  if (rung == "sso") {
    .crypto()
    session <- if (!is.null(s$sso_session)) f$config[[paste("sso-session", s$sso_session)]] %||% list() else list()
    cfg <- session; cfg[names(s)] <- s
    cache_key <- as.character(openssl::sha1(charToRaw(s$sso_session %||% s$sso_start_url)))
    token <- .cloud_json(ctx, paste0("~/.aws/sso/cache/", cache_key, ".json"))
    if (is.null(token)) .abort("No cached SSO token. Run `aws sso login`.", "not_configured", ctx$provider)
    region <- cfg$sso_region %||% "us-east-1"
    if (!grepl("^[a-z0-9-]+$", region)) .abort("Invalid SSO region.", "not_configured", ctx$provider)
    access <- token$accessToken
    expired <- !is.null(token$expiresAt) && as.numeric(.parse_rfc3339(token$expiresAt)) <= as.numeric(ctx$clock()) + 300
    if (is.null(access) || expired) {
      if (any(vapply(token[c("refreshToken", "clientId", "clientSecret")], is.null, logical(1)))) .abort("SSO login must be renewed with `aws sso login`.", "not_configured", ctx$provider)
      reply <- .token_http(.token_request(paste0("https://oidc.", region, ".amazonaws.com/token"), json_object(clientId = token$clientId, clientSecret = token$clientSecret, grantType = "refresh_token", refreshToken = token$refreshToken), "json"), ctx$transport)
      if (reply$status < 200 || reply$status >= 300 || is.null(reply$body$accessToken)) .auth_failure(ctx$provider)
      access <- reply$body$accessToken
    }
    url <- paste0("https://portal.sso.", region, ".amazonaws.com/federation/credentials?role_name=", .path_id(cfg$sso_role_name), "&account_id=", .path_id(cfg$sso_account_id))
    body <- .cloud_response(ctx, .cloud_http(ctx, "GET", url, list("x-amz-sso_bearer_token" = access)))
    return(.cloud_aws_response(ctx, body$roleCredentials))
  }
  .unsupported(ctx$provider, paste("AWS credential source", rung))
}
