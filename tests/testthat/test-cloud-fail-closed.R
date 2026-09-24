test_that("malformed explicit GCP credentials cannot select an ambient identity", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  bad <- file.path(f$home, "broken.json"); writeLines("not json", bad)
  fallback <- file.path(f$home, ".config", "gcloud"); dir.create(fallback, recursive = TRUE)
  writeLines('{"type":"authorized_user","client_id":"other","client_secret":"other","refresh_token":"other"}', file.path(fallback, "application_default_credentials.json"))
  source <- cloud_credential_provider("vertex", env = c(f$env, GOOGLE_APPLICATION_CREDENTIALS = bad), transport = function(...) stop("No fallback request is allowed"))
  expect_error(source(), class = "NotConfiguredError")
})

test_that("partial AWS environment credentials cannot select profile keys", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  directory <- file.path(f$home, ".aws"); dir.create(directory)
  writeLines(c("[default]", "aws_access_key_id = other", "aws_secret_access_key = other"), file.path(directory, "credentials"))
  source <- cloud_credential_provider("bedrock-chat", env = c(f$env, AWS_ACCESS_KEY_ID = "incomplete", AWS_EC2_METADATA_DISABLED = "true"), transport = function(...) stop("must not run"))
  expect_error(source(), class = "NotConfiguredError")
})
