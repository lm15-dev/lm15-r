test_that("explicit environments control home-relative credential paths", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  env <- c(f$env, LM15_CREDENTIALS_PATH = "~/credentials.json")
  expect_identical(credentials_path(env = env), file.path(f$home, "credentials.json"))
  write_credentials(auth_body(expires = 9999999999999), path = "~/credentials.json", env = env)
  expect_identical(load_local_credential("xai", path = "~/credentials.json", env = env)$credential$value, "old-access")
  expect_error(load_local_credential("xai", path = "~/credentials.json", env = character()), class = "NotConfiguredError")
})

test_that("Azure endpoint variables respect the AUTH-10 full URL forms", {
  lm <- new_lm("azure", api_key = "test", env = c(AZURE_OPENAI_ENDPOINT = "https://example.openai.azure.com"))
  expect_identical(lm$base_url, "https://example.openai.azure.com/openai/v1")
  foundry <- new_lm("azure-anthropic", api_key = "test", env = c(ANTHROPIC_FOUNDRY_BASE_URL = "https://example.services.ai.azure.com/anthropic/"))
  expect_identical(foundry$base_url, "https://example.services.ai.azure.com/anthropic/v1")
  expect_error(new_lm("azure", api_key = "test", settings = list(resource = "https://user:secret@example.test"), env = character()), class = "NotConfiguredError")
})

test_that("cloud host settings read the selected profile rather than guessing residency", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  dir.create(file.path(f$home, ".aws"))
  writeLines(c("[profile work]", "region = eu-west-1"), file.path(f$home, ".aws", "config"))
  lm <- new_lm("bedrock-chat", api_key = "test", env = c(f$env, AWS_PROFILE = "work"))
  expect_match(lm$base_url, "eu-west-1", fixed = TRUE)
  expect_error(new_lm("bedrock-chat", api_key = "test", env = character()), class = "NotConfiguredError")
})
