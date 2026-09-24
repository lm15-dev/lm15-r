.utc_timestamp <- function(seconds) format(as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
.token_request <- function(url, body, encoding = "form", headers = list()) {
  if (!is.character(url) || length(url) != 1L || !grepl("^https://[^/@?#]+(?:/[^#]*)?$", url, perl = TRUE)) .abort("Token exchange requires an HTTPS URL without userinfo or fragment.", "not_configured")
  headers[["content-type"]] <- if (encoding == "form") "application/x-www-form-urlencoded" else "application/json"
  structure(list(method = "POST", url = url, headers = .json_object(headers), body_encoding = encoding, body = body), class = "lm15_token_request")
}
print.lm15_token_request <- function(x, ...) { cat("<lm15 token exchange request: redacted>\n"); invisible(x) }
str.lm15_token_request <- function(object, ...) { print.lm15_token_request(object); invisible(NULL) }

# Pure request construction, shared by the cloud runtime and contract adapter.
token_exchange_build <- function(provider, rung, input, ..., now = Sys.time(), settings = list()) {
  .check_dots(...); provider <- canonical_provider(provider)
  fail <- function() .abort("Credential source is incomplete or malformed; secret fields are not shown.", "not_configured", provider)
  required <- function(x) { if (!is.character(x) || length(x) != 1L || !nzchar(x)) fail(); x }
  issued <- floor(as.numeric(now))
  integer_token <- function(n) .json_number(sprintf("%.0f", n))
  if (rung %in% c("adc-env", "adc-file", "service-account")) {
    f <- input$credential_file
    if (!.is_object(f)) fail()
    uri <- f$token_uri %||% "https://oauth2.googleapis.com/token"
    if (identical(f$type, "authorized_user")) return(.token_request(uri, json_object(grant_type = "refresh_token", client_id = required(f$client_id), client_secret = required(f$client_secret), refresh_token = required(f$refresh_token))))
    if (!identical(f$type, "service_account")) .unsupported(provider, "this credential file's token exchange")
    header <- json_object(alg = "RS256", typ = "JWT")
    if (!is.null(f$private_key_id)) header$kid <- f$private_key_id
    claims <- json_object(iat = integer_token(issued), exp = integer_token(issued + 3600), iss = required(f$client_email), aud = uri, scope = input$scope %||% "https://www.googleapis.com/auth/cloud-platform")
    assertion <- jwt_rs256(header, claims, required(f$private_key))
    return(.token_request(uri, json_object(grant_type = "urn:ietf:params:oauth:grant-type:jwt-bearer", assertion = assertion)))
  }
  if (rung %in% c("environment", "workload-identity")) {
    env <- input$env %||% list(); settings <- input$settings %||% settings
    tenant <- required(env$AZURE_TENANT_ID); client <- required(env$AZURE_CLIENT_ID)
    authority <- sub("/+$", "", settings$authority_host %||% env$AZURE_AUTHORITY_HOST %||% "https://login.microsoftonline.com")
    url <- paste0(authority, "/", .path_id(tenant), "/oauth2/v2.0/token")
    body <- json_object(client_id = client, scope = settings$scope %||% "https://ai.azure.com/.default")
    if (rung == "environment" && !is.null(env$AZURE_CLIENT_SECRET)) body$client_secret <- required(env$AZURE_CLIENT_SECRET)
    else {
      assertion <- input$client_assertion
      if (is.null(assertion)) {
        .crypto()
        cert <- tryCatch(openssl::read_cert(required(input$certificate_pem)), error = function(e) fail())
        der <- openssl::write_der(cert)
        header <- json_object(alg = "RS256", typ = "JWT", x5t = .base64url(openssl::sha1(der)))
        if (identical(tolower(env$AZURE_CLIENT_SEND_CERTIFICATE_CHAIN %||% ""), "true")) header$x5c <- list(.base64_encode(der))
        # Random per assertion in normal use; injectable for deterministic replay.
        jti <- input$jti %||% paste(sprintf("%02x", as.integer(openssl::rand_bytes(16L))), collapse = "")
        claims <- json_object(aud = url, iss = client, sub = client, exp = integer_token(issued + 600), iat = integer_token(issued), jti = jti)
        assertion <- jwt_rs256(header, claims, required(input$private_key_pem))
      }
      body$client_assertion_type <- "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
      body$client_assertion <- required(assertion)
    }
    body$grant_type <- "client_credentials"
    return(.token_request(url, body))
  }
  .unsupported(provider, paste("token exchange for", rung))
}

token_exchange_parse <- function(provider, rung, status, body, ..., now = Sys.time()) {
  .check_dots(...); provider <- canonical_provider(provider)
  fail <- function() .abort("Credential exchange failed or returned malformed credentials; response contents are not shown.", "auth", provider)
  required <- function(x) { if (!is.character(x) || length(x) != 1L || !nzchar(x)) fail(); x }
  if (!.is_object(body)) fail()
  if (rung == "credential_process") {
    version <- tryCatch(.number(body$Version, "version", TRUE), error = function(e) fail())
    if (status != 0 || version != 1L) fail()
  } else if (status < 200 || status >= 300) fail()
  if (rung %in% c("credential_process", "imds", "container", "assume-role", "web-identity")) {
    expiry <- body$Expiration
    if (!is.null(expiry)) expiry <- tryCatch(.utc_timestamp(as.numeric(.parse_rfc3339(expiry))), error = function(e) fail())
    return(aws_credentials(required(body$AccessKeyId), required(body$SecretAccessKey), session_token = body$SessionToken %||% body$Token, expires_at = expiry))
  }
  token <- required(body$access_token %||% body$accessToken)
  expiry <- body$expires_at %||% body$expireTime
  numeric_value <- function(x) {
    if (is.character(x) && !is.object(x)) x <- suppressWarnings(as.numeric(x))
    tryCatch(.number(x, "expiry"), error = function(e) fail())
  }
  if (!is.null(expiry)) expiry <- tryCatch(.utc_timestamp(as.numeric(.parse_rfc3339(expiry))), error = function(e) fail())
  else if (!is.null(body$expires_on)) expiry <- .utc_timestamp(numeric_value(body$expires_on))
  else if (!is.null(body$expires_in)) {
    duration <- numeric_value(body$expires_in)
    if (duration < 0) fail()
    expiry <- .utc_timestamp(as.numeric(now) + duration)
  }
  bearer_token(token, expires_at = expiry)
}
