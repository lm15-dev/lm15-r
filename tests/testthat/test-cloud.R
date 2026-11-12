test_that("AWS environment keys stop before malformed later sources", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  config <- file.path(f$home, "bad-config"); writeLines("not an INI document", config)
  env <- c(f$env, AWS_ACCESS_KEY_ID = "key", AWS_SECRET_ACCESS_KEY = "secret", AWS_CONFIG_FILE = config, AWS_EC2_METADATA_DISABLED = "true")
  provider <- cloud_credential_provider("bedrock-chat", env = env, transport = function(...) stop("must not contact network"))
  expect_identical(provider()$access_key_id, "key")
  env <- c(env, AWS_BEARER_TOKEN_BEDROCK = "bearer")
  provider <- cloud_credential_provider("bedrock-chat", env = env, transport = function(...) stop("must not contact network"))
  expect_identical(provider()$kind, "bearer_token")
})

test_that("GCP token caches are isolated, reused, and refreshed inside the skew", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  path <- file.path(f$home, "adc.json")
  writeLines('{"type":"authorized_user","client_id":"client","client_secret":"secret","refresh_token":"refresh"}', path)
  env <- c(f$env, GOOGLE_APPLICATION_CREDENTIALS = path, NO_GCE_CHECK = "1")
  now <- 1788436800; calls <- 0L
  transport <- function(wire) {
    calls <<- calls + 1L
    expect_identical(wire$url, "https://oauth2.googleapis.com/token")
    list(status = 200L, body = charToRaw(paste0('{"access_token":"token-', calls, '","expires_in":3600}')))
  }
  clock <- function() as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  a <- cloud_credential_provider("vertex", env = env, transport = transport, clock = clock)
  b <- cloud_credential_provider("vertex", env = env, transport = transport, clock = clock)
  expect_identical(a()$value, "token-1")
  expect_identical(a()$value, "token-1")
  expect_identical(b()$value, "token-2")
  now <- now + 3301
  expect_identical(a()$value, "token-3")
  expect_identical(calls, 3L)
})

test_that("configured Azure service principal failures do not fall back", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  env <- c(f$env, AZURE_TENANT_ID = "tenant", AZURE_CLIENT_ID = "client", AZURE_CLIENT_SECRET = "secret")
  calls <- 0L
  provider <- cloud_credential_provider("azure", env = env, transport = function(wire) {
    calls <<- calls + 1L
    list(status = 401L, body = charToRaw('{"error":"denied","error_description":"SECRET-SENTINEL-DO-NOT-PRINT"}'))
  })
  error <- tryCatch(provider(), error = identity)
  expect_s3_class(error, "AuthError")
  expect_identical(calls, 1L)
  expect_false(grepl("SECRET-SENTINEL-DO-NOT-PRINT", conditionMessage(error), fixed = TRUE))
})

test_that("container credential hosts cannot leak tokens to arbitrary HTTP servers", {
  provider <- cloud_credential_provider("bedrock-chat", env = c(AWS_CONTAINER_CREDENTIALS_FULL_URI = "http://evil.example/token", AWS_CONTAINER_AUTHORIZATION_TOKEN = "secret", AWS_EC2_METADATA_DISABLED = "true"), transport = function(...) stop("must not contact network"))
  expect_error(provider(), class = "NotConfiguredError")
})

test_that("new_lm uses cloud credentials at request time, not construction", {
  calls <- 0L
  env <- c(AWS_ACCESS_KEY_ID = "key", AWS_SECRET_ACCESS_KEY = "secret", AWS_REGION = "us-east-1", AWS_EC2_METADATA_DISABLED = "true")
  lm <- new_lm("bedrock-chat", env = env, transport = function(...) { calls <<- calls + 1L; stop("not sent") })
  wire <- build_request(lm, request("openai.gpt-oss-20b-1:0", list(message_user("hello"))))
  expect_match(wire$headers$authorization, "AWS4-HMAC-SHA256 Credential=key/", fixed = TRUE)
  expect_identical(calls, 0L)
})

# Google external_account with an AWS source (lm15-contract spec/auth.md AUTH-1 gcp-chain, amended
# 2026-11-12). The subject token is pinned byte for byte by auth/token-vectors.json (computed by
# google-auth); these walk the acquisition: the AWS identity, then STS, then impersonation.
aws_wif <- function(f, source = NULL) {
  source <- source %||% list(environment_id = "aws1", region_url = "http://169.254.169.254/latest/meta-data/placement/availability-zone",
    url = "http://169.254.169.254/latest/meta-data/iam/security-credentials",
    regional_cred_verification_url = "https://sts.{region}.amazonaws.com?Action=GetCallerIdentity&Version=2011-06-15")
  path <- file.path(f$home, "wif.json")
  writeLines(jsonlite::toJSON(list(type = "external_account", audience = "//iam.googleapis.com/projects/1/locations/global/workloadIdentityPools/p/providers/aws",
    subject_token_type = "urn:ietf:params:aws:token-type:aws4_request", token_url = "https://sts.googleapis.com/v1/token", credential_source = source,
    service_account_impersonation_url = "https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/sa@p.iam.gserviceaccount.com:generateAccessToken"), auto_unbox = TRUE), path)
  path
}
aws_answers <- function(extra = list()) c(extra, list(
  "https://sts.googleapis.com/" = '{"access_token":"federated","expires_in":3600,"token_type":"Bearer"}',
  "https://iamcredentials.googleapis.com/" = '{"accessToken":"sa-token","expireTime":"2026-09-20T01:00:00Z"}'))
aws_transport <- function(answers, calls) function(wire) {
  calls$all[[length(calls$all) + 1L]] <- wire
  for (prefix in names(answers)) if (startsWith(wire$url, prefix)) return(list(status = 200L, body = charToRaw(answers[[prefix]])))
  stop("unexpected request ", wire$url)
}
aws_signed <- function(calls) {
  sts <- Filter(function(w) startsWith(w$url, "https://sts.googleapis.com/"), calls$all)[[1L]]
  body <- sts$body; if (is.raw(body)) body <- jsonlite::fromJSON(rawToChar(body)) else if (is.character(body)) body <- jsonlite::fromJSON(body)
  expect_identical(body$subjectTokenType, "urn:ietf:params:aws:token-type:aws4_request")
  signed <- jsonlite::fromJSON(utils::URLdecode(body$subjectToken), simplifyVector = FALSE)
  list(url = signed$url, headers = stats::setNames(lapply(signed$headers, `[[`, "value"), vapply(signed$headers, `[[`, "", "key")))
}
aws_clock <- function() as.POSIXct(1789862400, origin = "1970-01-01", tz = "UTC")

test_that("GCP AWS source on ECS: the task role signs, then STS, then impersonation", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  calls <- new.env(); calls$all <- list()
  env <- c(f$env, GOOGLE_APPLICATION_CREDENTIALS = aws_wif(f), AWS_CONTAINER_CREDENTIALS_RELATIVE_URI = "/v2/credentials/abc", AWS_REGION = "us-east-2", NO_GCE_CHECK = "1")
  transport <- aws_transport(aws_answers(list("http://169.254.170.2/v2/credentials/abc" = '{"AccessKeyId":"ASIATASK","SecretAccessKey":"s","Token":"task-session","Expiration":"2026-09-20T06:00:00Z"}')), calls)
  expect_identical(cloud_credential_provider("vertex", env = env, transport = transport, clock = aws_clock, named = "workload")()$value, "sa-token")
  expect_identical(vapply(calls$all, `[[`, "", "url"), c("http://169.254.170.2/v2/credentials/abc", "https://sts.googleapis.com/v1/token",
    "https://iamcredentials.googleapis.com/v1/projects/-/serviceAccounts/sa@p.iam.gserviceaccount.com:generateAccessToken"))
  s <- aws_signed(calls)
  expect_identical(s$url, "https://sts.us-east-2.amazonaws.com?Action=GetCallerIdentity&Version=2011-06-15")
  expect_true(startsWith(s$headers$Authorization, "AWS4-HMAC-SHA256 Credential=ASIATASK/20260920/us-east-2/sts/aws4_request, SignedHeaders=host;x-amz-date;x-amz-security-token, "))
  expect_identical(s$headers[["x-amz-security-token"]], "task-session")
  expect_identical(s$headers[["x-goog-cloud-target-resource"]], "//iam.googleapis.com/projects/1/locations/global/workloadIdentityPools/p/providers/aws")
})

test_that("GCP AWS source on EC2: IMDSv2 once, the zone's region, the role's credentials", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  calls <- new.env(); calls$all <- list()
  source <- list(environment_id = "aws1", region_url = "http://169.254.169.254/latest/meta-data/placement/availability-zone",
    url = "http://169.254.169.254/latest/meta-data/iam/security-credentials", imdsv2_session_token_url = "http://169.254.169.254/latest/api/token",
    regional_cred_verification_url = "https://sts.{region}.amazonaws.com?Action=GetCallerIdentity&Version=2011-06-15")
  env <- c(f$env, GOOGLE_APPLICATION_CREDENTIALS = aws_wif(f, source), NO_GCE_CHECK = "1")
  transport <- aws_transport(aws_answers(list(
    "http://169.254.169.254/latest/api/token" = "imds-session",
    "http://169.254.169.254/latest/meta-data/placement/availability-zone" = "eu-west-1b",
    "http://169.254.169.254/latest/meta-data/iam/security-credentials/web" = '{"Code":"Success","AccessKeyId":"ASIAEC2","SecretAccessKey":"s","Token":"t"}',
    "http://169.254.169.254/latest/meta-data/iam/security-credentials" = "web\n")), calls)
  expect_identical(cloud_credential_provider("vertex", env = env, transport = transport, clock = aws_clock, named = "workload")()$value, "sa-token")
  expect_identical(sum(vapply(calls$all, function(w) w$method == "PUT", TRUE)), 1L)
  for (w in calls$all[2:4]) expect_identical(w$headers[["X-aws-ec2-metadata-token"]], "imds-session")
  expect_true(startsWith(aws_signed(calls)$url, "https://sts.eu-west-1.amazonaws.com?"))
})

test_that("GCP AWS source: a malformed source or no AWS identity is refused before any request", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  for (case in list(list(list(environment_id = "aws2", regional_cred_verification_url = "https://sts.{region}.amazonaws.com"), "only aws1"),
                    list(list(environment_id = "aws1"), "regional_cred_verification_url"))) {
    env <- c(f$env, GOOGLE_APPLICATION_CREDENTIALS = aws_wif(f, case[[1L]]), AWS_REGION = "us-east-1", AWS_ACCESS_KEY_ID = "a", AWS_SECRET_ACCESS_KEY = "s", NO_GCE_CHECK = "1")
    expect_error(cloud_credential_provider("vertex", env = env, transport = function(...) stop("no request"), named = "workload")(), case[[2L]])
  }
  env <- c(f$env, GOOGLE_APPLICATION_CREDENTIALS = aws_wif(f, list(environment_id = "aws1", regional_cred_verification_url = "https://sts.{region}.amazonaws.com")), AWS_REGION = "us-east-1", NO_GCE_CHECK = "1")
  expect_error(cloud_credential_provider("vertex", env = env, transport = function(...) stop("no request"), named = "workload")(), "AWS_ACCESS_KEY_ID nor a container endpoint")
})
