# AUTH-10 backend settings (amended 2026-09-30) and MAP-7 rule 6's default
# max_tokens: lm15-contract changes/2026-09-30-claude-code-client-version.md.

refusal <- "Claude Code 2.1.170 does not support this model; version 2.1.280 or newer is required. Run 'claude update', or update the Claude desktop app, then try again."
ua <- function(client) build_request(client, request("claude-opus-5-5", list(message_user("hi"))))$headers[["user-agent"]]

test_that("claude-code's client_version is a backend setting (AUTH-10)", {
  access <- lm15:::.definition("claude-code")$access
  expect_identical(access$backend_options$client_version, "2.1.285")
  expect_identical(unlist(access$backend_settings[[1L]]$env), "LM15_CLAUDE_CODE_VERSION")
  expect_identical(unlist(lm15:::.definition("openai-codex")$access$backend_settings[[1L]]$env), "LM15_CODEX_CLIENT_VERSION")
  expect_identical(ua(new_lm("claude-code", api_key = "k")), "claude-cli/2.1.285")
  expect_identical(ua(new_lm("claude-code", api_key = "k", settings = list(client_version = "2.1.280"))), "claude-cli/2.1.280")
  old <- Sys.getenv("LM15_CLAUDE_CODE_VERSION", unset = NA)
  Sys.setenv(LM15_CLAUDE_CODE_VERSION = "9.9.9")
  on.exit(if (is.na(old)) Sys.unsetenv("LM15_CLAUDE_CODE_VERSION") else Sys.setenv(LM15_CLAUDE_CODE_VERSION = old), add = TRUE)
  # A client built by hand reads no environment for it; a router does.
  expect_identical(ua(new_lm("claude-code", api_key = "k")), "claude-cli/2.1.285")
  expect_identical(ua(new_lm("claude-code", api_key = "k", env = c(LM15_CLAUDE_CODE_VERSION = "2.1.282"))), "claude-cli/2.1.282")
  expect_identical(ua(new_lm("claude-code", api_key = "k", env = c(LM15_CLAUDE_CODE_VERSION = "2.1.282"),
    settings = list(client_version = "2.1.281"))), "claude-cli/2.1.281")
  codex <- new_lm("openai-codex", api_key = "k", account_id = "a", env = c(LM15_CODEX_CLIENT_VERSION = "0.151.0"))
  expect_identical(codex$definition$access$backend_options$client_version, "0.151.0")
  expect_error(new_lm("claude-code", api_key = "k", settings = list(version = "1")), "known: client_version", class = "NotConfiguredError")
  expect_error(new_lm("anthropic", api_key = "k", env = character(), settings = list(client_version = "1")), "this door takes no settings", class = "NotConfiguredError")
  report <- explain_auth("claude-code", env = c(LM15_CLAUDE_CODE_VERSION = "2.1.290"), path = "/nonexistent")
  expect_identical(report$settings$client_version, "2.1.290")
  expect_identical(attr(report$settings, "sources")$client_version$from, "env:LM15_CLAUDE_CODE_VERSION")
})

test_that("the minimum-version refusal names the setting", {
  body <- lm15:::.json_encode(json_object(type = "error", request_id = "req_1", error = json_object(type = "invalid_request_error", message = refusal)))
  err <- normalize_error(new_lm("claude-code", api_key = "k"), 400L, body)
  expect_s3_class(err, "InvalidRequestError")
  expect_identical(err$message, paste0(refusal, "\n\n  To fix:\n    - lm15 sends this version itself; updating Claude Code does not change it\n    - Set the claude-code setting client_version to 2.1.280 or newer (or LM15_CLAUDE_CODE_VERSION=2.1.280)\n"))
  expect_identical(normalize_error(new_lm("anthropic", api_key = "k", env = character()), 400L, body)$message, refusal)
})

test_that("an unset max_tokens is the model's ceiling (MAP-7 rule 6)", {
  client <- new_lm("anthropic", api_key = "k", env = character())
  rows <- list(
    list("claude-opus-5-5", NULL, 128000L, 128000L),
    list("claude-haiku-4-5", NULL, 64000L, 64000L),
    list("claude-sonnet-4-5", 32768L, 64000L, 64000L - 32768L),
    list("claude-haiku-4-5", 64000L, 64000L + 16384L, 16384L),
    list("anthropic.claude-haiku-4-5-20251001-v1:0", NULL, 64000L, 64000L),
    list("claude-3-5-haiku-20241022", NULL, 8192L, 8192L),
    list("deepseek-v4-flash", NULL, 16384L, 16384L))
  for (row in rows) {
    cfg <- if (is.null(row[[2L]])) config() else config(reasoning = reasoning("high", thinking_budget = row[[2L]]))
    req <- request(row[[1L]], list(message_user("hi")), config = cfg)
    body <- lm15:::.json_decode(rawToChar(build_request(client, req)$body))
    expect_equal(as.numeric(unclass(body$max_tokens)), row[[3L]], info = row[[1L]])
    record <- Filter(function(a) a$field == "config.max_tokens", plan(client, req))
    expect_length(record, 1L)
    expect_identical(record[[1L]]$action, "defaulted")
    expect_equal(as.numeric(unclass(record[[1L]]$applied)), row[[4L]], info = row[[1L]])
  }
})
