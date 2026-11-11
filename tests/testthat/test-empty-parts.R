# MAP-24 (lm15-contract changes/2026-11-11-empty-parts.md, issue #3): a part with
# nothing for a wire writes nothing. The contract's vectors
# (mapping/empty-parts.json) and the history replay pin the bytes for the four
# dialects through the vet shim. These pin what they do not: nothing is recorded,
# the caller's request is not touched, the state comes back on its own dialect.

wire_body <- function(lm, req) lm15:::.json_decode(rawToChar(build_request(lm, req)$body))
plain <- function(x) jsonlite::fromJSON(lm15:::.json_encode(x), simplifyVector = FALSE)
canonical <- function(model, messages, ...) from_dict(c(list(model = model, messages = messages), list(...)), "request")
txt <- function(value) list(type = "text", text = value)
msg <- function(role, ...) list(role = role, parts = list(...))

# What lm15 reads from a Responses reasoning item with no summary (MAP-7 rules 9 and 11).
hidden <- list(type = "thinking", text = "",
               continuation = list(list(provider = "openai", kind = "reasoning_item",
                                        data = list(id = "rs_1", encrypted_content = "gAAAA"))))
call <- list(type = "tool_call", id = "call_1", name = "delete_program", input = structure(list(), names = character()))
result <- function(...) msg("tool", list(type = "tool_result", id = "call_1", name = "delete_program", content = list(...)))

# The history of issue #3: two steps answered by an OpenAI Responses model with
# hidden reasoning, then a new user message.
report <- function(model) canonical(model, list(
  msg("user", txt("Delete the test program now.")),
  msg("assistant", hidden, call),
  result(txt("ok")),
  msg("assistant", hidden, txt("Deleted.")),
  msg("user", txt("hello"))))

lm_for <- function(provider, ...) new_lm(provider, api_key = "k", env = character(), ...)

test_that("the report builds a body Claude accepts", {
  wire <- plain(wire_body(lm_for("anthropic"), report("claude-opus-5-5")))$messages
  expect_identical(vapply(wire, function(m) m$role, ""), c("user", "assistant", "user", "assistant", "user"))
  expect_identical(wire[[2]]$content, list(list(type = "tool_use", id = "call_1", name = "delete_program", input = structure(list(), names = character()))))
  expect_identical(wire[[4]]$content, list(list(type = "text", text = "Deleted.")))
  expect_false(grepl('"text":""', rawToChar(build_request(lm_for("anthropic"), report("claude-opus-5-5"))$body), fixed = TRUE))
})

test_that("nothing is recorded under any policy, and the caller's history is not touched", {
  for (pair in list(c("anthropic", "claude-opus-5-5"), c("gemini", "gemini-2.5-flash"), c("openai", "gpt-5.4"), c("openai-chat", "gpt-5.4"))) {
    req <- report(pair[[2]])
    before <- lm15:::.json_encode(as_dict(req))
    for (policy in c("note", "silent", "refuse")) {
      lm <- lm_for(pair[[1]], adaptations = policy)
      fields <- vapply(plan(lm, req), function(a) a$field, "")
      expect_false(any(startsWith(fields, "messages")), label = paste(pair[[1]], policy))
      expect_no_error(build_request(lm, req))  # "refuse" fails on any recorded deviation
    }
    expect_identical(lm15:::.json_encode(as_dict(req)), before)
  }
})

test_that("the state goes back natively on its own dialect", {
  wire_body(lm_for("anthropic"), report("claude-opus-5-5"))
  items <- plain(wire_body(lm_for("openai"), report("gpt-5.4")))$input
  reasoning <- Filter(function(i) identical(i$type, "reasoning"), items)
  one <- list(type = "reasoning", summary = list(), id = "rs_1", encrypted_content = "gAAAA")
  expect_length(reasoning, 2L)
  for (item in reasoning) expect_identical(item[sort(names(item))], one[sort(names(one))])
})

test_that("an empty thinking part with Claude's signature is still a thinking block; a signature with no value is not state", {
  signed <- function(signature) list(type = "thinking", text = "",
    continuation = list(list(provider = "anthropic", kind = "thinking_signature", data = list(signature = signature))))
  kept <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("hi")), msg("assistant", signed("sig"), call), result(txt("ok"))))))$messages
  expect_identical(kept[[2]]$content[[1]], list(type = "thinking", thinking = "", signature = "sig"))
  dropped <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("hi")), msg("assistant", signed(""), txt("Hello.")), msg("user", txt("again"))))))$messages
  expect_identical(dropped[[2]]$content, list(list(type = "text", text = "Hello.")))
})

test_that("a cache mark asked on a message left out goes to the one written before it", {
  # Not pinned by the contract (stated in the change entry): the reference's choice.
  wire <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("Here is the manual.")), msg("assistant", hidden), msg("user", txt("And the appendix."))),
    config = list(cache = list(mode = "auto", prefix_until_index = 1L)))))$messages
  expect_length(wire, 2L)
  expect_false(is.null(wire[[1]]$content[[1]]$cache_control))
  expect_null(wire[[2]]$content[[1]]$cache_control)
})

test_that("a tool result keeps its empty output; an empty block beside an image is left out", {
  image <- list(type = "image", media_type = "image/png", url = "https://example.com/receipt.png")
  alone <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("hi")), msg("assistant", call), result(txt(""))))))$messages[[3]]$content[[1]]
  expect_identical(alone$type, "tool_result")
  expect_identical(alone$content, "")
  beside <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("hi")), msg("assistant", call), result(txt(""), image)))))$messages[[3]]$content[[1]]
  expect_identical(beside$content, list(list(type = "image", source = list(type = "url", url = "https://example.com/receipt.png"))))
})

test_that("whitespace is content; a reply with citations or a data answer can be sent back to OpenAI", {
  wire <- plain(wire_body(lm_for("anthropic"), canonical("claude-opus-5-5", list(
    msg("user", txt("hi")), msg("assistant", txt("\n")), msg("user", txt("again"))))))$messages
  expect_identical(wire[[2]]$content, list(list(type = "text", text = "\n")))
  cited <- canonical("gpt-5.4", list(
    msg("user", txt("Capital of France?")),
    msg("assistant", txt("Paris."), list(type = "citation", url = "https://example.com/france", title = "France")),
    msg("user", txt("Sure?"))))
  for (provider in c("openai", "openai-chat")) expect_no_error(build_request(lm_for(provider), cited))
  chat <- plain(wire_body(lm_for("openai-chat"), cited))$messages
  expect_identical(chat[[2]], list(role = "assistant", content = "Paris."))
  data <- canonical("gpt-5.4", list(
    msg("user", txt("Paid?")), msg("assistant", list(type = "data", value = list(paid = TRUE))), msg("user", txt("Sure?"))))
  expect_identical(plain(wire_body(lm_for("openai-chat"), data))$messages[[2]], list(role = "assistant", content = '{"paid":true}'))
})
