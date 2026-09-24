live_fixture <- function(frames) {
  sent <- list(); closes <- 0L
  connect <- function(...) list(
    send = function(frame) sent[[length(sent) + 1L]] <<- lm15:::.json_decode(frame),
    receive = function(timeout) { if (!length(frames)) return(NULL); out <- frames[[1L]]; frames <<- frames[-1L]; out },
    close = function() closes <<- closes + 1L)
  list(connect = connect, sent = function() sent, closes = function() closes)
}

test_that("ordinary OpenAI realtime completion selects a socket and materializes text", {
  socket <- live_fixture(list('{"type":"response.output_text.delta","delta":"hello"}', '{"type":"response.done","response":{"status":"completed","usage":{"input_tokens":2,"output_tokens":1}}}'))
  lm <- new_lm("openai", api_key = "fake", env = character(), live_connect = socket$connect, transport = function(...) stop("HTTP must not be used"))
  response <- complete(lm, request("gpt-realtime", list(message_user("hi"))))
  expect_identical(response_text(response), "hello")
  expect_identical(response$usage$total_tokens, 3L)
  expect_identical(socket$closes(), 1L)
  expect_identical(vapply(socket$sent(), function(frame) frame$type, ""), c("session.update", "conversation.item.create", "response.create"))
})

test_that("repeated final arguments do not duplicate a realtime tool call", {
  frames <- list(
    '{"type":"response.output_item.added","item":{"type":"function_call","id":"item-1","call_id":"call-1","name":"lookup","arguments":""}}',
    '{"type":"response.function_call_arguments.delta","item_id":"item-1","delta":"{\\"x\\":1}"}',
    '{"type":"response.function_call_arguments.done","call_id":"call-1","arguments":"{\\"x\\":1}"}',
    '{"type":"response.output_item.done","item":{"type":"function_call","id":"item-1","call_id":"call-1","name":"lookup","arguments":"{\\"x\\":1}"}}',
    '{"type":"response.done","response":{"status":"completed"}}')
  socket <- live_fixture(frames)
  lm <- new_lm("openai", api_key = "fake", env = character(), live_connect = socket$connect)
  result <- complete(lm, request("gpt-realtime", list(message_user("hi"))))
  expect_length(tool_calls(result), 1L)
  expect_identical(as_json(tool_calls(result)[[1L]]$input), '{"x":1}')
  expect_identical(result$finish_reason, "tool_call")
})

test_that("live requests refuse silent config loss before connecting", {
  lm <- new_lm("openai", api_key = "fake", env = character(), live_connect = function(...) stop("must not connect"))
  expect_error(complete(lm, request("gpt-realtime", list(message_user("hi")), config = config(top_k = 3L))), class = "UnsupportedFeatureError")
})

test_that("Gemini live completion waits for setup and reads native audio transcripts", {
  socket <- live_fixture(list('{"setupComplete":{}}', '{"serverContent":{"outputTranscription":{"text":"hello"},"turnComplete":true},"usageMetadata":{"promptTokenCount":2,"responseTokenCount":3}}'))
  lm <- new_lm("gemini", api_key = "fake", env = character(), live_connect = socket$connect)
  result <- complete(lm, request("gemini-live-preview", list(message_user("hi"))))
  expect_identical(response_text(result), "hello")
  expect_identical(result$usage$total_tokens, 5L)
  expect_identical(socket$sent()[[2L]]$realtimeInput$text, "hi")
  expect_identical(socket$closes(), 1L)
})

test_that("truncated realtime responses are not reported as complete", {
  socket <- live_fixture(list('{"type":"response.output_text.delta","delta":"partial"}'))
  lm <- new_lm("openai", api_key = "fake", env = character(), live_connect = socket$connect)
  error <- tryCatch(complete(lm, request("gpt-realtime", list(message_user("hi")))), error = identity)
  expect_s3_class(error, "StreamAssemblyError")
  expect_identical(response_text(error$partial), "partial")
  expect_identical(socket$closes(), 1L)
})
