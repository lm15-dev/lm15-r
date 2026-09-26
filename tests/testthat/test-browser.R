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

test_that("browser errors carry the diagnostics every language shows, never the credential", {
  id <- "browser-diagnostics-test"; secret <- "SECRET-SENTINEL-DO-NOT-PRINT"
  on.exit(browser_call("dispose", id))
  request <- as_dict(request("gpt-5-mini", list(message_user("hi"))))
  browser_call("prepare", id, json_object(provider = "openai", api_key = secret, request = request))
  body <- lm15:::.base64_encode(charToRaw('{"error": {"message": "offline quota", "type": "rate_limit_error"}}'))
  parsed <- browser_call("response", id, json_object(status = 429L, body_b64 = body,
    headers = json_object(`x-request-id` = "req-browser", `retry-after` = "2", `x-ratelimit-remaining-requests` = "0")))
  expect_false(parsed$ok)
  expect_identical(parsed$error$class, "RateLimitError")
  expect_identical(parsed$error$request_id, "req-browser")
  expect_identical(as.integer(unclass(parsed$error$status)), 429L)
  expect_match(parsed$error$display, "request req-browser", fixed = TRUE)
  expect_identical(parsed$error$rate_limit_headers$`x-ratelimit-remaining-requests`[[1L]], "0")
  expect_false(grepl(secret, as_json(parsed$error), fixed = TRUE))
})
