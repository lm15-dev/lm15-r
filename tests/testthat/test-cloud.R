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
