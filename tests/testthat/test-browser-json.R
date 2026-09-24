test_that("browser JSON-text methods preserve integers beyond JavaScript's range", {
  call <- function(action, payload = json_object()) lm15:::.json_decode(browser_dispatch(action, "exact-json-test", as_json(payload)))
  on.exit(call("dispose"))
  req <- '{"model":"gpt-test","messages":[{"role":"user","parts":[{"type":"text","text":"hi"}]}],"config":{"max_tokens":9007199254740993}}'
  prepared <- call("prepare", json_object(provider = "openai", api_key = "test", request = req, json_only = TRUE))
  expect_true(prepared$ok)
  wire <- rawToChar(jsonlite::base64_dec(prepared$result$body_b64))
  expect_match(wire, '"max_output_tokens":9007199254740993', fixed = TRUE)
  body <- '{"model":"gpt-test","output":[{"type":"message","content":[{"type":"output_text","text":"hi"}]}],"usage":{"input_tokens":9007199254740993,"output_tokens":2}}'
  reply <- call("response", json_object(status = 200L, body_b64 = lm15:::.base64_encode(charToRaw(body)), headers = json_object()))
  expect_true(reply$ok)
  expect_type(reply$result$response, "character")
  expect_match(reply$result$response, '"input_tokens":9007199254740993', fixed = TRUE)
  expect_match(reply$result$response, '"total_tokens":9007199254740995', fixed = TRUE)
})
