# Tinker (Thinking Machines): the two doors, the reasoning switch per base
# family, a checkpoint that follows its declared base model, URI model names,
# FastAPI error bodies (MAP-21), the stop sequence returned in the text
# (MAP-22) and the catalog's fine-tune origin (MAP-23). lm15-contract
# changes/2026-11-09-tinker-live.md; the harness pins the same rules through
# the corpus, these tests keep them visible here.

ck <- "tinker://e68146d2-1a2e-56e8-b42f-1f5957705501:train:0/sampler_weights/lm15-untrained"
tinker <- function(base_models = list()) new_lm("tinker", api_key = "k", env = character(), base_models = base_models)
wire_of <- function(lm, model, cfg = config()) lm15:::.json_decode(rawToChar(build_request(lm, request(model, list(message_user("hi")), config = cfg))$body))
effort_of <- function(lm, model, e) wire_of(lm, model, config(reasoning = reasoning(e)))$reasoning_effort

test_that("two doors, one key; a URI is one name; aliases route", {
  router <- new_router(env = character())
  for (case in list(list(ck, "tinker", ck), list(paste0("tinker:", ck), "tinker", ck),
                    list(paste0("tinker-anthropic:", ck), "tinker-anthropic", ck),
                    list(paste0("thinkingmachines:", ck), "tinker", ck),
                    list("thinkingmachines:Qwen/Qwen3-8B", "tinker", "Qwen/Qwen3-8B"))) {
    r <- resolve(router, case[[1L]])
    expect_identical(c(r$provider, r$model), c(case[[2L]], case[[3L]]), label = case[[1L]])
  }
  expect_error(resolve(router, "openai://x"), class = "UnknownModelError")
  expect_identical(openai_chat_model_string("thinkingmachines/Qwen/Qwen3-8B"), "tinker:Qwen/Qwen3-8B")
})

test_that("reasoning off per base family, and an unnamed checkpoint's default", {
  lm <- tinker()
  expected <- list("thinkingmachines/Inkling-Small" = "none", "Qwen/Qwen3.5-4B" = FALSE, "moonshotai/Kimi-K2.6" = FALSE,
                   "nvidia/NVIDIA-Nemotron-3.5-Lightning-30B-A3B-BF16" = FALSE, "Qwen/Qwen3.8-27B" = FALSE,
                   "deepseek-ai/DeepSeek-V4.1-Flash:peft:262144" = FALSE)
  expected[[ck]] <- FALSE
  for (model in names(expected)) expect_identical(effort_of(lm, model, "off"), expected[[model]], label = model)
  expect_identical(effort_of(lm, "Qwen/Qwen3.5-4B", "high"), TRUE)
  records <- plan(lm, request("Qwen/Qwen3.5-4B", list(message_user("hi")), config = config(reasoning = reasoning("high"))))
  expect_length(records, 1L)
  expect_identical(c(records[[1L]]$asked, records[[1L]]$applied), c("high", "on"))
  for (model in c("thinkingmachines/Inkling", "openai/gpt-oss-20b", ck)) expect_identical(effort_of(lm, model, "low"), "low", label = model)
})

test_that("a checkpoint follows its declared base model, directly and through the router", {
  expect_identical(effort_of(tinker(setNames(list("thinkingmachines/Inkling-Small"), ck)), ck, "off"), "none")
  expect_identical(effort_of(tinker(setNames(list("Qwen/Qwen3.5-4B"), ck)), ck, "medium"), TRUE)
  router <- new_router(env = c(TINKER_API_KEY = "k"), base_models = setNames(list("thinkingmachines/Inkling"), ck))
  expect_identical(effort_of(router_lm(router, ck), ck, "off"), "none")
  expect_error(new_router(base_models = setNames(list(""), ck)))
})

test_that("response_format is dropped and recorded", {
  for (fmt in list(list(type = "json_object"), list(type = "json_schema", name = "x", schema = list(type = "object")))) {
    req <- request("thinkingmachines/Inkling", list(message_user("hi")), config = config(response_format = fmt))
    expect_null(wire_of(tinker(), "thinkingmachines/Inkling", config(response_format = fmt))$response_format)
    expect_identical(vapply(plan(tinker(), req), function(a) a$field, ""), "config.response_format")
  }
})

test_that("MAP-22: the returned stop sequence is cut; other servers keep the dialect's rule", {
  req <- request("thinkingmachines/Inkling-Small", list(message_user("count")), config = config(stop = list(" 7")))
  reply <- '{"id":"x","object":"chat.completion","model":"m","choices":[{"index":0,"message":{"role":"assistant","content":"1 2 3 4 5 6 7"},"finish_reason":"stop"}],"usage":{"prompt_tokens":5,"completion_tokens":7,"total_tokens":12}}'
  out <- parse_response(tinker(), req, reply)
  expect_identical(out$message$parts[[1L]]$text, "1 2 3 4 5 6")
  expect_identical(as.numeric(unclass(out$usage$input_tokens)), 5)
  plain <- new_lm("openai-chat", api_key = "k", env = character())
  other <- request("gpt-4.1-mini", list(message_user("x")), config = config(stop = list("END")))
  body <- '{"id":"x","object":"chat.completion","model":"m","choices":[{"index":0,"message":{"role":"assistant","content":"a END b"},"finish_reason":"stop"}]}'
  expect_identical(parse_response(plain, other, body)$message$parts[[1L]]$text, "a END b")
})

test_that("MAP-21 and MAP-15: detail bodies keep their message and their class", {
  lm <- tinker()
  for (case in list(list(401L, '{"detail":"Unable to validate api-key"}', "auth"),
                    list(404L, paste0('{"detail":"Weights not found for model_path ', ck, '"}'), "unsupported_model"),
                    list(400L, '{"detail":"Sampling is not supported for x. (org)"}', "unsupported_model"),
                    list(400L, '{"detail":"model_path \'tinker://x\' is not a valid tinker path. Expected"}', "unsupported_model"),
                    list(422L, '{"detail":"model returned no tool call when tool_choice required one"}', "invalid_request"))) {
    err <- normalize_error(lm, case[[1L]], case[[2L]])
    expect_identical(err$code, case[[3L]], label = case[[2L]])
  }
  expect_identical(lm15:::.detail_message(lm15:::.json_decode('{"detail":"Validation Error","errors":[{"loc":"body -> messages -> 0 -> role","msg":"Input should be \'system\'"}]}')),
                   "Validation Error: body -> messages -> 0 -> role: Input should be 'system'")
})

test_that("MAP-23: the catalog names each checkpoint's base model", {
  body <- paste0('{"object":"list","data":[{"id":"', ck, '","object":"model","created":1,"owned_by":"x","base_model":"Qwen/Qwen3.5-4B"}]}')
  models <- parse_models_response(tinker(), body)
  expect_length(models, 1L)
  expect_identical(c(models[[1L]]$origin$type, models[[1L]]$origin$base_model), c("fine-tune", "Qwen/Qwen3.5-4B"))
})
