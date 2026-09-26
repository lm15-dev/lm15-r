note <- message_user("Ripe blackberry and cassis, toasty oak; will reward a decade in the cellar.")
wine <- judgments(
  quality = score("How good is this wine?", c(poor = "Faulty", ok = "Sound", great = "Outstanding")),
  style = choice("Which style?", c(fruit = "Fruit-forward", oak = "Oak-driven", other = NA)),
  ageing = yes_no("Will it improve with age?")
)
chat_reply <- function(content) list(status = 200L, body = as_json(json_object(
  id = "c1", model = "m", choices = json_array(json_object(index = 0L, finish_reason = "stop", message = json_object(role = "assistant", content = content))),
  usage = json_object(prompt_tokens = 1L, completion_tokens = 1L))))
wire_body <- function(wire) lm15:::.json_decode(rawToChar(wire$body))

test_that("the helpers emit the MAP-14 convention and it reads back", {
  found <- judgments_in_schema(wine$schema)
  expect_identical(names(found), c("quality", "style", "ageing"))
  expect_identical(found$quality$kind, "ordered")
  expect_identical(found$quality$keys, c("0", "1", "2"))
  expect_identical(found$quality$titles[["1"]], "ok")
  expect_identical(found$style$kind, "choice")
  expect_null(found$style$descriptions$other)
  expect_identical(found$ageing$keys, c("true", "false"))
  expect_identical(wine$schema$required, json_array("quality", "style", "ageing"))
  expect_error(score("x", "only one"), "at least two")
  expect_error(choice("x", c("a", "a")), "unique")
  plain <- json_object(type = "object", properties = json_object(note = json_object(type = "string")))
  expect_length(judgments_in_schema(plain), 0L)
})

test_that("data parts serialize their value always and their measurement on assistants only", {
  expect_identical(as_json(data_part(NULL)), '{"type":"data","value":null}')
  part <- data_part(json_object(ok = TRUE), probabilities = json_object(ok = json_object(true = 1, false = 0)), method = "provider_classification")
  expect_identical(as_json(part), '{"type":"data","value":{"ok":true},"probabilities":{"ok":{"true":1.0,"false":0.0}},"method":"provider_classification"}')
  expect_error(message_user(part), "INV-052")
  expect_error(data_part(1L, probabilities = json_object(ok = json_object(true = 1))), "iff")
  expect_s3_class(message_assistant(part), "lm15_Message")
  back <- from_json(as_json(part), "part")
  expect_identical(back$probabilities$ok$true, 1)
})

test_that("cloud wires answer a data part and record or refuse probabilities", {
  req <- request("gpt-5-mini", list(note), config = config(response_format = wine, probabilities = "if_available", max_tokens = 50L))
  transport <- fake_transport(list(chat_reply('{"quality": 2, "style": "oak", "ageing": true}')))
  chat <- new_lm("openai-chat", api_key = "k", transport = transport)
  r <- complete(chat, req)
  expect_identical(response_data(r)$style, "oak")
  expect_null(response_probabilities(r))
  expect_identical(vapply(r$adaptations, function(a) a$field, ""), "config.probabilities")
  strict <- request("gpt-5-mini", list(note), config = config(response_format = wine, probabilities = "required"))
  err <- tryCatch(plan(chat, strict), UnsupportedFeatureError = identity)
  expect_identical(err$feature, "config.probabilities")
  a <- wire_body(build_request(new_lm("anthropic", api_key = "k"), request("claude-haiku-4-5", list(note), config = config(response_format = wine, max_tokens = 50L))))
  expect_null(a$output_config$format$schema$properties$quality$type)
  expect_identical(a$output_config$format$schema$properties$quality$anyOf[[1]]$type, "integer")
  g <- wire_body(build_request(new_lm("gemini", api_key = "k"), request("gemini-2.5-flash", list(note), config = config(response_format = wine))))
  expect_identical(unlist(g$generationConfig$responseJsonSchema$properties$style$enum), c("fruit", "oak", "other"))
  o <- wire_body(build_request(new_lm("openai", api_key = "k"), request("gpt-5-mini", list(note), config = config(response_format = wine))))
  expect_identical(as_json(o$text$format$schema), as_json(wine$schema))
})

test_that("a data part is its compact JSON on a text-only wire", {
  req <- request("gpt-5-mini", list(message_user(data_part(json_object(note = "Ripe", price = 48L)))))
  body <- wire_body(build_request(new_lm("openai", api_key = "k"), req))
  expect_identical(body$input[[1]]$content[[1]]$text, '{"note":"Ripe","price":48}')
  replay <- request("gpt-5-mini", list(message_user("q"), message_assistant(data_part(json_object(ok = TRUE))), message_user("again")))
  body <- wire_body(build_request(new_lm("openai", api_key = "k"), replay))
  expect_identical(body$input[[2]]$content[[1]], json_object(type = "output_text", text = '{"ok":true}'))
})

# The token trie on a server that scores named tokens (MAP-14 section 4).
prefix_tokens <- c(1L, 2L, 3L)
paths <- list(fruit = c(10L, 99L), oak = c(11L, 99L), other = c(12L, 99L), true = c(20L, 99L), false = c(21L, 99L))
trie_transport <- function(logprobs, drop_ids = FALSE, chat = list()) {
  seen <- new.env(); seen$prompts <- NULL; seen$calls <- character()
  send <- function(wire, on_chunk = NULL) {
    payload <- wire_body(wire)
    seen$calls <- c(seen$calls, sub("^.*/", "/", wire$url))
    if (endsWith(wire$url, "/tokenize")) {
      answer <- payload$messages[[length(payload$messages)]]$content
      key <- trimws(sub("^Answer:", "", answer))
      tokens <- c(prefix_tokens, if (nzchar(key)) paths[[key]][1L])
      if (nzchar(key) && !isTRUE(payload$continue_final_message)) tokens <- c(tokens, paths[[key]][-1L], 7L)
      return(list(status = 200L, headers = list(), body = charToRaw(as_json(json_object(tokens = do.call(json_array, as.list(tokens)))))))
    }
    if (endsWith(wire$url, "/chat/completions")) { reply <- chat[[1L]]; chat <<- chat[-1L]; return(list(status = 200L, headers = list(), body = charToRaw(reply$body))) }
    seen$prompts <- payload$prompt
    ids <- vapply(payload$logprob_token_ids, function(t) as.integer(unclass(t)), 1L)
    choices <- lapply(seq_along(payload$prompt), function(i) {
      top <- if (drop_ids) json_object() else lm15:::.json_object(setNames(lapply(ids, function(t) logprobs[[as.character(t)]] %||% -9), paste0("token_id:", ids)))
      json_object(index = i - 1L, text = "x", finish_reason = "length", logprobs = json_object(top_logprobs = json_array(top)))
    })
    list(status = 200L, headers = list(), body = charToRaw(as_json(json_object(model = "m", choices = lm15:::.json_array(choices), usage = json_object(prompt_tokens = 40L, completion_tokens = length(choices))))))
  }
  structure(send, seen = seen)
}
`%||%` <- function(x, y) if (is.null(x)) y else x

test_that("the trie driver scores every key path in one batched call", {
  fmt <- judgments(style = choice("Which style?", c("fruit", "oak", "other")), ageing = yes_no("Will it age?"))
  transport <- trie_transport(list("10" = -0.1, "11" = -2.0, "12" = -3.0, "20" = -0.05, "21" = -3.0, "99" = -0.01))
  lm <- new_lm("openai-chat", api_key = "k", base_url = "http://vllm:8001/v1", compat = "vllm", transport = transport)
  r <- complete(lm, request("m", list(note), config = config(response_format = fmt, probabilities = "required")))
  expect_identical(response_method(r), "candidate_sequence_likelihood")
  expect_identical(response_data(r)$style, "fruit")
  expect_true(response_data(r)$ageing)
  expect_equal(response_probabilities(r)$style$fruit, 0.830, tolerance = 0.005)
  expect_identical(r$provider_data$judgments$nodes, 7L)
  expect_identical(r$provider_data$judgments$tokenize_calls, 12L)
  seen <- attr(transport, "seen")
  expect_length(seen$prompts, 7L)
  expect_identical(vapply(seen$prompts[[1L]], function(t) as.integer(unclass(t)), 1L), prefix_tokens)
  expect_identical(sum(seen$calls == "/completions"), 1L)
  expect_length(r$adaptations, 0L)
})

test_that("the trie driver detects a server that drops logprob_token_ids", {
  fmt <- judgments(ageing = yes_no("Will it age?"))
  transport <- trie_transport(list(), drop_ids = TRUE, chat = list(list(body = as_json(json_object(id = "c", model = "m",
    choices = json_array(json_object(index = 0L, finish_reason = "stop", message = json_object(role = "assistant", content = '{"ageing": true}'))),
    usage = json_object(prompt_tokens = 1L, completion_tokens = 1L))))))
  lm <- new_lm("openai-chat", api_key = "k", base_url = "http://vllm:8000/v1", compat = "vllm", transport = transport)
  r <- complete(lm, request("m", list(note), config = config(response_format = fmt, probabilities = "if_available")))
  expect_true(response_data(r)$ageing)
  expect_null(response_probabilities(r))
  expect_true("config.probabilities" %in% vapply(r$adaptations, function(a) a$field, ""))
  strict <- new_lm("openai-chat", api_key = "k", base_url = "http://vllm:8000/v1", compat = "vllm", transport = trie_transport(list(), drop_ids = TRUE))
  expect_error(complete(strict, request("m", list(note), config = config(response_format = fmt, probabilities = "required"))), class = "UnsupportedFeatureError")
})

test_that("jev routes to typesafe, which answers a distribution per judgment", {
  expect_identical(resolve(new_router(env = character()), "jev-latest")$provider, "typesafe")
  reply <- as_json(json_object(model = "jev-latest", answers = json_object(
    quality = json_object(type = "score", probabilities = json_object("0" = 0.1, "1" = 0.2, "2" = 0.7)),
    style = json_object(type = "choice", choice = "oak", probabilities = json_object(fruit = 0.1, oak = 0.8, other = 0.1)),
    ageing = json_object(type = "noul", noul = 0.9)), usage = json_object(input_tokens = 10L, output_tokens = 0L)))
  transport <- fake_transport(list(list(status = 200L, body = reply, headers = list("x-typesafe-request-id" = "r-1"))))
  lm <- new_lm("typesafe", api_key = "k", transport = transport)
  r <- complete(lm, request("jev-latest", list(note), config = config(response_format = wine)))
  expect_identical(response_data(r)$quality, 2L)
  expect_identical(response_data(r)$style, "oak")
  expect_true(response_data(r)$ageing)
  expect_equal(response_probabilities(r)$ageing$false, 0.1)
  expect_equal(response_expected(r, "quality"), 1.6)
  expect_identical(response_method(r), "provider_classification")
  expect_identical(r$id, "r-1")
  expect_error(stream(lm, request("jev-latest", list(note), config = config(response_format = wine)), function(e) NULL), class = "UnsupportedFeatureError")
  err <- normalize_error(lm, 400L, '{"detail": {"error_type": "api_usage_error", "message": "Unknown model: x"}}')
  expect_s3_class(err, "UnsupportedModelError")
  err <- normalize_error(lm, 422L, '{"detail": [{"type": "missing", "loc": ["body", "questions", "q"], "msg": "Field required"}]}')
  expect_match(conditionMessage(err), "questions.q: Field required", fixed = TRUE)
})
