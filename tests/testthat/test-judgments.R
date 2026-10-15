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

test_that("images reach jev as assets named in the state, with the opt-in header", {
  # lm15-contract changes/2026-10-14-jev-images.md D1-D4
  lm <- new_lm("typesafe", api_key = "k", transport = fake_transport(list()))
  fmt <- config(response_format = judgments(circle = yes_no("Is there a red circle in the image?")))
  png <- "iVBORw0KGgo="; jpg <- "/9j/4AAQ"
  build <- function(parts) build_request(lm, request("jev-latest", list(message_user(parts)), config = fmt))
  body <- function(w) jsonlite::fromJSON(rawToChar(w$body), simplifyVector = FALSE)
  w <- build(list(image_part(data = png, media_type = "image/png")))
  b <- body(w)
  expect_identical(names(b), c("model", "state", "assets", "questions"))
  expect_identical(b$state, "<lm15-image-1>")
  expect_identical(b$assets, list("<lm15-image-1>" = list(type = "image", format = "png", data_base64 = png)))
  expect_identical(w$headers[["typesafe-danger-zone-images"]], "true")
  expect_length(w$adaptations, 0L)
  b <- body(build(list(text_part("A red ball."), image_part(data = png, media_type = "image/png"), data_part(list(k = 1L)),
                       image_part(data = jpg, media_type = "image/jpeg"))))
  expect_identical(b$state, list("A red ball.", "<lm15-image-1>", list(k = 1L), "<lm15-image-2>"))
  expect_identical(b$assets[["<lm15-image-2>"]]$format, "jpg")
  plain <- build(list(text_part("no picture")))
  expect_null(plain$headers[["typesafe-danger-zone-images"]])
  expect_null(body(plain)$assets)
  bad <- list(
    list(image_part(url = "https://example.com/a.png"), "not a URL"),
    list(image_part(file_id = "file_1"), "not a file id"),
    list(image_part(data = "UklGRg==", media_type = "image/webp"), "PNG or JPEG"),
    list(audio_part(data = "UklGRg=="), "no slot on the systemone wire"))
  for (case in bad) {
    err <- tryCatch(build(list(text_part("x"), case[[1L]])), error = function(e) e)
    expect_s3_class(err, "UnsupportedFeatureError")
    expect_identical(err$feature, "messages[0].parts[1]")
    expect_match(conditionMessage(err), case[[2L]], fixed = TRUE)
  }
})

test_that("a bounding box is a judgment read by its shape, with no keys and no bounds", {
  # lm15-contract changes/2026-10-15-bounding-box.md D1, D4
  prop <- bounding_box("the coffee mug")
  expect_identical(prop$type, "object")
  expect_identical(prop$description, "the coffee mug")
  expect_identical(names(prop$properties), c("top", "bottom", "left", "right"))
  expect_identical(unlist(prop$required), c("top", "bottom", "left", "right"))
  expect_false(prop$additionalProperties)
  for (edge in prop$properties) {
    expect_identical(edge$type, "number")
    expect_match(edge$description, "fraction", fixed = TRUE)
    expect_null(edge$minimum); expect_null(edge$maximum)  # Anthropic answers 400 on bounds (receipted)
  }
  find <- judgments(mug = bounding_box("the coffee mug"), mug_visible = yes_no("Is there a coffee mug in the image?"))
  found <- judgments_in_schema(find$schema)
  expect_identical(found$mug$kind, "box")
  expect_length(found$mug$keys, 0L)
  expect_identical(found$mug$instruction, "the coffee mug")
  expect_identical(names(lm15:::.measurable_judgments(found)), "mug_visible")
  edge <- json_object(type = "number")
  all4 <- function(s) json_object(top = s, bottom = s, left = s, right = s)
  shapes <- json_object(
    hand_written = json_object(properties = json_object(left = edge, top = edge, right = edge, bottom = edge)),
    bounded = json_object(type = "object", properties = all4(json_object(type = "number", minimum = 0L, maximum = 1L))),
    extra_edge = json_object(type = "object", properties = json_object(top = edge, bottom = edge, left = edge, right = edge, label = json_object(type = "string"))),
    integer_edges = json_object(type = "object", properties = all4(json_object(type = "integer"))),
    pixels = json_object(type = "object", properties = json_object(x = edge, y = edge, w = edge, h = edge)))
  read <- judgments_in_schema(json_object(type = "object", properties = shapes))
  expect_identical(names(read), c("hand_written", "bounded"))
  expect_identical(unname(vapply(read, function(j) j$kind, "")), c("box", "box"))
  # The wire rewrites leave a box untouched; the schema goes verbatim.
  a <- wire_body(build_request(new_lm("anthropic", api_key = "k"), request("claude-haiku-4-5", list(note), config = config(response_format = find, max_tokens = 50L))))
  expect_identical(as_json(a$output_config$format$schema$properties$mug), as_json(find$schema$properties$mug))
  g <- wire_body(build_request(new_lm("gemini", api_key = "k"), request("gemini-2.5-flash", list(note), config = config(response_format = find))))
  expect_identical(as_json(g$generationConfig$responseJsonSchema$properties$mug), as_json(find$schema$properties$mug))
  # The probabilities policy concerns only keyed judgments.
  only <- judgments(mug = bounding_box("the coffee mug"))
  chat <- new_lm("openai-chat", api_key = "k", transport = fake_transport(list()))
  for (policy in c("if_available", "required"))
    expect_length(plan(chat, request("gpt-5-mini", list(note), config = config(response_format = only, probabilities = policy))), 0L)
  err <- tryCatch(plan(chat, request("gpt-5-mini", list(note), config = config(response_format = find, probabilities = "required"))), UnsupportedFeatureError = identity)
  expect_identical(err$feature, "config.probabilities")
})

test_that("jev asks a box question with the opt-in header and answers it verbatim", {
  # lm15-contract changes/2026-10-15-bounding-box.md D2, D3
  find <- judgments(mug = bounding_box("the coffee mug"), mug_visible = yes_no("Is there a coffee mug in the image?"))
  only <- judgments(mug = bounding_box("the coffee mug"))
  photo <- list(message_user(list(image_part(data = "iVBORw0KGgo=", media_type = "image/png"))))
  lm <- new_lm("typesafe", api_key = "k", transport = fake_transport(list()))
  w <- build_request(lm, request("jev-preview", photo, config = config(response_format = find)))
  b <- jsonlite::fromJSON(rawToChar(w$body), simplifyVector = FALSE)
  expect_identical(b$questions$mug, list(type = "box", instructions = "the coffee mug"))
  expect_identical(w$headers[["typesafe-danger-zone-images"]], "true")
  w <- build_request(lm, request("jev-preview", list(message_user("a kitchen")), config = config(response_format = only)))
  expect_identical(w$headers[["typesafe-danger-zone-images"]], "true")
  expect_null(jsonlite::fromJSON(rawToChar(w$body), simplifyVector = FALSE)$assets)
  answer <- function(answers) list(status = 200L, body = as_json(json_object(model = "jev-1.14.0", answers = answers)), headers = list())
  box <- json_object(top = 0.25, bottom = 0.82, left = 0.44, right = 1L)
  answers <- json_object(mug = json_object(type = "box", box = box, confidence = 0.92), mug_visible = json_object(type = "noul", noul = 0.97))
  r <- complete(new_lm("typesafe", api_key = "k", transport = fake_transport(list(answer(answers)))),
                request("jev-preview", photo, config = config(response_format = find, probabilities = "required")))
  expect_identical(as_json(response_data(r)$mug), '{"top":0.25,"bottom":0.82,"left":0.44,"right":1}')
  expect_true(response_data(r)$mug_visible)
  expect_identical(names(response_probabilities(r)), "mug_visible")
  expect_equal(as.numeric(unclass(r$provider_data$typesafe$answers$mug$confidence)), 0.92)
  r <- complete(new_lm("typesafe", api_key = "k", transport = fake_transport(list(answer(json_object(mug = answers$mug))))),
                request("jev-preview", photo, config = config(response_format = only)))
  expect_null(response_probabilities(r))
  bad <- list(
    list(json_object(top = 0.1, bottom = 0.2, left = 0.3), "answers.mug.box"),
    list(json_object(top = 0.1, bottom = 1.2, left = 0.3, right = 0.4), "answers.mug.box.bottom"),
    list(json_object(top = "0.1", bottom = 0.2, left = 0.3, right = 0.4), "answers.mug.box.top"),
    list(json_array(0.1, 0.2, 0.3, 0.4), "answers.mug.box"))
  for (case in bad) {
    err <- tryCatch(complete(new_lm("typesafe", api_key = "k", transport = fake_transport(list(answer(json_object(mug = json_object(type = "box", box = case[[1L]])))))),
                             request("jev-preview", photo, config = config(response_format = only))), error = function(e) e)
    expect_match(conditionMessage(err), case[[2L]], fixed = TRUE)
  }
})

test_that("the trie measures the yes/no and the generated call answers the box", {
  fmt <- judgments(mug = bounding_box("the coffee mug"), ageing = yes_no("Will it age?"))
  generated <- as_json(json_object(id = "c", model = "m", choices = json_array(json_object(index = 0L, finish_reason = "stop",
    message = json_object(role = "assistant", content = '{"mug": {"top": 0.2, "bottom": 0.6, "left": 0.55, "right": 0.85}, "ageing": false}'))),
    usage = json_object(prompt_tokens = 1L, completion_tokens = 1L)))
  transport <- trie_transport(list("20" = -0.05, "21" = -3.0, "99" = -0.01), chat = list(list(body = generated)))
  lm <- new_lm("openai-chat", api_key = "k", base_url = "http://vllm:8001/v1", compat = "vllm", transport = transport)
  r <- complete(lm, request("m", list(note), config = config(response_format = fmt, probabilities = "required")))
  expect_identical(as_json(response_data(r)$mug), '{"top":0.2,"bottom":0.6,"left":0.55,"right":0.85}')
  expect_true(response_data(r)$ageing)  # measured, overriding the generated pick
  expect_identical(names(response_probabilities(r)), "ageing")
  expect_identical(sum(attr(transport, "seen")$calls == "/completions"), 2L)
})
