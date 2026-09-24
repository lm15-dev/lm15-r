test_that("migration routing preserves the Chat Completions door", {
  router <- new_router(env = character())
  expect_identical(resolve_openai_chat(router, "gpt-4.1-mini")$provider, "openai-chat")
  expect_identical(resolve(router, "gpt-4.1-mini")$provider, "openai")
  expect_identical(resolve_openai_chat(router, "openai:gpt-4.1-mini")$provider, "openai")
  expect_identical(openai_chat_model_string("groq/openai/gpt-oss-20b"), "groq:openai/gpt-oss-20b")
  expect_identical(openai_chat_model_string("azure/deployment"), "azure-chat:deployment")
  expect_error(openai_chat_model_string("bedrock/model"), class = "UnknownModelError")
})

test_that("migrated calls use shared keys and the destination's actual parser", {
  transport <- fake_transport(list(list(body = '{"model":"gpt-4.1-mini","choices":[{"message":{"content":"hello"},"finish_reason":"stop"}]}')))
  router <- new_router(api_keys = list(openai = "fake-key"), env = character(), transport = transport)
  body <- json_object(model = "openai/gpt-4.1-mini", messages = list(json_object(role = "user", content = "hi")))
  result <- complete_from_openai_chat(router, body)
  expect_identical(response_text(result), "hello")
  wire <- recorded_requests(transport)[[1L]]
  expect_identical(wire$url, "https://api.openai.com/v1/chat/completions")
  expect_identical(wire$headers$authorization, "Bearer fake-key")
  body$api_key <- "must-not-be-used"
  expect_error(route_openai_chat(router, body), class = "UnsupportedFeatureError")
})
