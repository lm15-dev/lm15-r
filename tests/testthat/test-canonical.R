test_that("opaque JSON keeps every empty shape and number type", {
  payload <- '{"type":"tool_call","id":"c","name":"f","input":{"null":null,"string":"","array":[],"object":{},"int":1,"float":1.0,"large":9007199254740993}}'
  expect_identical(as_json(from_json(payload, "part")), payload)
  call <- from_json(payload, "part")
  req <- request("model", list(message_assistant(call)))
  expect_identical(as_json(from_json(as_json(req), "request")), as_json(req))
  expect_identical(as_json(req$messages[[1]]$parts[[1]]), payload)
})

test_that("typed number declarations control their JSON form", {
  expect_identical(as_json(config(temperature = 1L, top_k = 2)), '{"temperature":1.0,"top_k":2}')
  expect_error(config(top_k = 2.5))
  expect_error(config(top_k = TRUE))
  expect_error(config(temperature = TRUE))
  expect_error(config(temperature = Inf))
  expect_error(config(top_k = NA_integer_))
  expect_error(from_json('{"top_k":9007199254740992.5}', "config"))
})

test_that("usage distinguishes unknown from zero and preserves reported totals", {
  expect_identical(as_json(usage()), '{}')
  expect_identical(as_json(usage(input_tokens = 0)), '{"input_tokens":0}')
  expect_identical(usage(input_tokens = 2, output_tokens = 3)$total_tokens, 5L)
  expect_identical(usage(input_tokens = 2, output_tokens = 3, total_tokens = 99)$total_tokens, 99L)
})

test_that("invalid assignments do not corrupt the original value", {
  x <- config(top_k = 3)
  expect_error(x$top_k <- -1)
  expect_identical(x$top_k, 3L)
  expect_error(x$top_kk <- 4)
  expect_error(x[["top_k"]] <- TRUE)
  expect_identical(x$top_k, 3L)
})

test_that("role and media invariants hold for constructors and JSON", {
  expect_error(message_user(tool_call_part("c", "f")))
  expect_error(message_assistant(tool_result_part("c", list(text("ok")))))
  expect_error(message("tool", list(text("not a tool result"))))
  expect_error(image_part(url = "https://example.test/i", data = "YQ=="))
  expect_error(image_part())
  expect_error(from_json('{"type":"image","url":"https://example.test/i"}', "part"))
  expect_error(from_json('{"role":"user","parts":[]}', "message"))
  expect_identical(as_json(text("")), '{"type":"text","text":""}')
})

test_that("JSON rejects malformed inputs without losing valid empty shapes", {
  for (bad in c('{', '[1,]', '{"a":1,}', '{"a":1,"a":2}', '01', 'true false', 'NaN'))
    expect_error(from_json(bad, "config"))
  expect_identical(as_json(json_object()), '{}')
  expect_identical(as_json(json_array()), '[]')
  expect_identical(as_json(json_object(x = NULL)), '{"x":null}')
})

test_that("legacy reading stays separate from strict construction", {
  expect_error(reasoning(effort = "off", thinking_budget = 10))
  expect_identical(as_json(from_json('{"effort":"off","thinking_budget":10}', "reasoning")), '{"effort":"off"}')
  expect_error(from_json('{"tool_choice":"none"}', "config"))
  expect_identical(as_json(from_json('{"type":"text"}', "part")), '{"type":"text","text":""}')
  expect_error(from_json('{"type":"future_part"}', "part"))
})
