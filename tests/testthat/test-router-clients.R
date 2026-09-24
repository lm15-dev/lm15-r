test_that("routers reuse adapters without caching rotating credential results", {
  calls <- 0L
  router <- new_router(api_keys = list(openai = function() { calls <<- calls + 1L; paste0("key-", calls) }), env = character())
  first <- router_lm(router, "gpt-4.1-mini")
  second <- router_lm(router, "gpt-4.1")
  expect_identical(first, second)
  request <- request("gpt-4.1-mini", list(message_user("hi")))
  expect_identical(build_request(first, request)$headers$authorization, "Bearer key-1")
  expect_identical(build_request(second, request)$headers$authorization, "Bearer key-2")
})

test_that("copying or changing router configuration cannot reuse another identity", {
  first <- new_router(api_keys = list(openai = "one"), env = character())
  client <- router_lm(first, "gpt-4.1")
  second <- first; second$api_keys <- list(openai = api_key("two"))
  other <- router_lm(second, "gpt-4.1")
  request <- request("gpt-4.1", list(message_user("hi")))
  expect_identical(build_request(other, request)$headers$authorization, "Bearer two")
  expect_identical(build_request(router_lm(first, "gpt-4.1"), request)$headers$authorization, "Bearer one")
  expect_identical(build_request(client, request)$headers$authorization, "Bearer one")
})
