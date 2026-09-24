browser_call <- function(action, id, payload = json_object()) {
  lm15:::.json_decode(browser_dispatch(action, id, as_json(payload)))
}

test_that("browser resources use the same generation and model codecs", {
  id <- "browser-model-test"
  on.exit(browser_call("dispose", id))
  prepared <- browser_call("resource_prepare", id, json_object(provider = "openai", api_key = "test", surface = "models", action = "list"))
  expect_true(prepared$ok)
  expect_identical(prepared$result$requests[[1]]$method, "GET")
  expect_identical(prepared$result$requests[[1]]$url, "https://api.openai.com/v1/models")
  payload <- charToRaw('{"data":[{"id":"gpt-test"}]}')
  parsed <- browser_call("resource_response", id, json_object(replies = list(json_object(status = 200L, headers = json_object(), body_b64 = lm15:::.base64_encode(payload)))))
  expect_true(parsed$ok)
  expect_identical(parsed$result$value[[1]]$id, "gpt-test")
})

test_that("browser resource errors redact the credential used for that request", {
  id <- "browser-error-test"; secret <- "SECRET-SENTINEL-DO-NOT-PRINT"
  on.exit(browser_call("dispose", id))
  browser_call("resource_prepare", id, json_object(provider = "openai", api_key = secret, surface = "models", action = "list"))
  body <- charToRaw(as_json(json_object(error = json_object(message = secret, code = "invalid_api_key"))))
  parsed <- browser_call("resource_response", id, json_object(replies = list(json_object(status = 401L, headers = json_object(), body_b64 = lm15:::.base64_encode(body)))))
  expect_false(parsed$ok)
  expect_identical(parsed$error$code, "auth")
  expect_false(grepl(secret, as_json(parsed), fixed = TRUE))
})
