# MAP-7 rule 7 (amended 2026-11-07): a summary asked for on Claude is
# thinking.display "summarized"; Opus 4.7, 4.8 and every Claude 5 hide the
# thinking without it. The contract pins the adaptive and budget wires
# (anthropic.reasoning_summary, anthropic.reasoning_summary_budget); these pin
# what its cases do not: no summary, no knob; other servers of the wire, no knob.

thinking_of <- function(lm, model, r) {
  req <- request(model, list(message_user("q")), config = config(reasoning = r))
  t <- lm15:::.json_decode(rawToChar(build_request(lm, req)$body))$thinking
  if (is.null(t)) NULL else unclass(t)
}
claude <- function() new_lm("anthropic", api_key = "k", env = character())

test_that("a summary asked for sends display summarized, adaptive or budget; none asked, none sent", {
  expect_identical(thinking_of(claude(), "claude-opus-5-5", reasoning("high", summary = "auto")),
                   list(type = "adaptive", display = "summarized"))
  expect_identical(thinking_of(claude(), "claude-opus-5-5", reasoning("high", summary = "detailed")),
                   list(type = "adaptive", display = "summarized"))
  budget <- thinking_of(claude(), "claude-haiku-4-5-20251001", reasoning("low", summary = "auto"))
  expect_identical(budget$type, "enabled")
  expect_identical(as.numeric(unclass(budget$budget_tokens)), 2048)
  expect_identical(budget$display, "summarized")
  expect_identical(thinking_of(claude(), "claude-opus-5-5", reasoning("high")), list(type = "adaptive"))
})

test_that("servers of the Messages wire with another thinking format get no display", {
  for (compat in c("deepseek", "meta", "moonshotai")) {
    model <- c(deepseek = "deepseek-v4-flash", meta = "muse-spark-1.3", moonshotai = "kimi-k3")[[compat]]
    lm <- new_lm("anthropic", api_key = "k", env = character(), compat = compat)
    expect_false(grepl("display", paste(deparse(thinking_of(lm, model, reasoning("high", summary = "auto"))), collapse = "")), label = compat)
  }
})
