# new_router(providers = ): a provider lm15 does not list, declared for one
# router (lm15-python RouterConfig(providers=...), docs/using-the-router.md).

nebius <- function(...) provider_definition(
  "nebius",
  base_url = "https://api.tokenfactory.nebius.com/v1",
  env_keys = "NEBIUS_API_KEY",
  supports = c("complete", "stream", "models"),
  aliases = "tokenfactory",
  ...
)
body_of <- function(wire) lm15:::.json_decode(rawToChar(wire$body))
hi <- function(model) request(model, list(message_user("hi")))

test_that("a declared provider routes by id and alias and says it is declared", {
  router <- new_router(providers = list(nebius()), env = character())
  by_id <- resolve(router, "nebius:deepseek-ai/DeepSeek-R1-0528")
  expect_identical(by_id[c("provider", "model", "source")],
                   list(provider = "nebius", model = "deepseek-ai/DeepSeek-R1-0528", source = "prefix"))
  expect_true(by_id$declared)
  expect_identical(resolve(router, "tokenfactory:m")$provider, "nebius")
  expect_false(resolve(router, "groq:llama-3.3-70b-versatile")$declared)
  expect_false(resolve(router, "claude-haiku-4-5")$declared)
  # Nothing is global: another router does not know the name.
  expect_error(resolve(new_router(env = character()), "nebius:m"), class = "UnknownModelError")
})

test_that("its wire request goes to the declared address with the declared key", {
  router <- new_router(providers = list(nebius()), env = c(NEBIUS_API_KEY = "nb-env"))
  wire <- build_request(router, hi("nebius:deepseek-ai/DeepSeek-R1-0528"))
  expect_identical(wire$url, "https://api.tokenfactory.nebius.com/v1/chat/completions")
  expect_identical(wire$headers$authorization, "Bearer nb-env")
  expect_identical(body_of(wire)$model, "deepseek-ai/DeepSeek-R1-0528")
  # api_keys and base_urls are keyed by the declared id; an explicit key wins over the environment.
  router <- new_router(providers = list(nebius()), env = c(NEBIUS_API_KEY = "nb-env"),
                       api_keys = list(nebius = "nb-explicit"), base_urls = list(nebius = "http://localhost:9000/v1"))
  wire <- build_request(router, hi("tokenfactory:m"))
  expect_identical(wire$url, "http://localhost:9000/v1/chat/completions")
  expect_identical(wire$headers$authorization, "Bearer nb-explicit")
  # No key anywhere: the declared variable is named, nothing is sent.
  expect_error(build_request(new_router(providers = list(nebius()), env = character()), hi("nebius:m")),
               class = "NotConfiguredError")
})

test_that("compat knobs reach the wire; an unknown knob is an error, not ignored", {
  router <- new_router(providers = list(nebius(compat = list(max_tokens_field = "max_tokens"))), env = c(NEBIUS_API_KEY = "k"))
  wire <- build_request(router, request("nebius:m", list(message_user("hi")), config = config(max_tokens = 7L)))
  expect_identical(as.integer(body_of(wire)$max_tokens), 7L)
  expect_null(body_of(wire)$max_completion_tokens)
  # The dialect default (an empty compat) sends max_completion_tokens.
  wire <- build_request(new_router(providers = list(nebius()), env = c(NEBIUS_API_KEY = "k")),
                        request("nebius:m", list(message_user("hi")), config = config(max_tokens = 7L)))
  expect_identical(as.integer(body_of(wire)$max_completion_tokens), 7L)
  expect_error(nebius(compat = list(max_token_field = "max_tokens")), "Unknown openai-chat compat knob")
})

test_that("a declaration cannot shadow a built-in name, a litellm spelling or another declaration", {
  shadow <- function(id, ...) provider_definition(id, base_url = "https://example.test/v1", ...)
  expect_error(new_router(providers = list(shadow("groq"))), class = "NotConfiguredError")
  expect_error(new_router(providers = list(shadow("together-ai"))), class = "NotConfiguredError")  # litellm's together_ai/
  expect_error(new_router(providers = list(shadow("github-copilot"))), class = "NotConfiguredError")  # a managed-login route
  expect_error(new_router(providers = list(shadow("acme", aliases = "fireworks"))), class = "NotConfiguredError")
  expect_error(new_router(providers = list(shadow("acme"), shadow("other", aliases = "acme"))), class = "NotConfiguredError")
  expect_error(new_router(api_keys = list(nebius = "k")), class = "NotConfiguredError")  # not declared here
})

test_that("provider_definition refuses what a key-based declaration cannot be", {
  expect_error(provider_definition("Nebius", base_url = "https://x.test/v1"), "lower-case")
  expect_error(provider_definition("nebius"), "base_url")
  expect_error(provider_definition("nebius", base_url = "https://x.test/v1?key=1"), "base_url")
  expect_error(provider_definition("local", base_url = "http://localhost:8001/v1", env_keys = "K", placeholder_key = "EMPTY"), "placeholder_key")
  expect_error(provider_definition("nebius", base_url = "https://x.test/v1", supports = "chat"), "supports")
  expect_error(provider_definition("nebius", base_url = "https://x.test/v1", aliases = "nebius"), "aliases")
  expect_error(provider_definition("nebius", base_url = "https://x.test/v1", dialect = "gemini"))
})

test_that("a keyless local declaration sends its placeholder key", {
  local <- provider_definition("my-vllm", base_url = "http://gpu-box:8001/v1", placeholder_key = "EMPTY")
  wire <- build_request(new_router(providers = list(local), env = character()), hi("my-vllm:Qwen/Qwen3-8B"))
  expect_identical(wire$headers$authorization, "Bearer EMPTY")
  expect_identical(wire$url, "http://gpu-box:8001/v1/chat/completions")
})

test_that("the OpenAI-SDK/litellm door reads a declared provider's spellings", {
  router <- new_router(providers = list(nebius()), env = character())
  expect_identical(resolve_openai_chat(router, "nebius/deepseek-ai/DeepSeek-R1-0528")[c("provider", "model")],
                   list(provider = "nebius", model = "deepseek-ai/DeepSeek-R1-0528"))
  expect_identical(resolve_openai_chat(router, "tokenfactory/m")$provider, "nebius")
  expect_error(resolve_openai_chat(new_router(env = character()), "nebius/m"), class = "UnknownModelError")
})

test_that("new_lm() takes a definition directly", {
  client <- new_lm(nebius(), api_key = "k")
  wire <- build_request(client, hi("m"))
  expect_identical(wire$url, "https://api.tokenfactory.nebius.com/v1/chat/completions")
})

test_that("the built-in rules are the reference's table, copied", {
  rules <- new_router(env = character())$rules
  expect_identical(vapply(rules, `[[`, "", 1L), vapply(lm15:::.provider_tables()$default_rules, function(r) r$prefix, ""))
  expect_error(resolve(new_router(env = character()), "jevons-1"), class = "UnknownModelError")
})
