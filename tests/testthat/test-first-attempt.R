# 2026-10-10: a budget alone fills effort (MAP-7 rule 3 read the other way); a DataPart answer reads through
# response_text and parse_json (types.md §Response convenience).

test_that("a thinking budget alone fills effort from the grading table", {
  got <- vapply(c(512, 1024, 2047, 2048, 8192, 16384, 24576, 32768, 1e6), function(b) reasoning(thinking_budget = b)$effort, "")
  expect_identical(got, c("minimal", "minimal", "minimal", "low", "medium", "high", "xhigh", "max", "max"))
  expect_identical(reasoning("high", thinking_budget = 1024)$effort, "high")
  expect_error(reasoning(), "needs effort")
  expect_error(reasoning("none"), "off")
})

test_that("a DataPart answer reads through response_text", {
  r <- lm15:::.new_value("Response", list(id = NULL, model = "m", message = lm15:::.new_value("Message", list(role = "assistant", parts = list(lm15:::.new_value("DataPart", list(value = json_object(ok = TRUE)))))), finish_reason = "stop", usage = lm15:::.new_value("Usage", list())))
  expect_identical(response_text(r), "{\"ok\":true}")
  expect_identical(parse_json(r)$ok, TRUE)
})
