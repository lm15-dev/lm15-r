test_that("live codec emits one tool call, not one per duplicate wire frame", {
  lm <- new_lm("openai", api_key = "test", env = character())
  arguments <- '{"type":"response.function_call_arguments.done","call_id":"c","name":"f","arguments":"{}"}'
  item <- '{"type":"response.output_item.done","item":{"type":"function_call","call_id":"c","name":"f","arguments":"{}"}}'
  done <- '{"type":"response.done","response":{"output":[{"type":"function_call"}],"usage":{"input_tokens":2,"output_tokens":3}}}'
  expect_length(live_decode(lm, arguments), 0L)
  expect_identical(live_decode(lm, item)[[1]]$type, "tool_call")
  events <- live_decode(lm, done)
  expect_length(events, 1L)
  expect_identical(events[[1]]$type, "usage")
  expect_identical(events[[1]]$usage$total_tokens, 5L)
})

test_that("live cancellation preserves usage before interruption", {
  lm <- new_lm("openai", api_key = "test", env = character())
  events <- live_decode(lm, '{"type":"response.done","response":{"status":"cancelled","usage":{"input_tokens":9}}}')
  expect_identical(vapply(events, function(e) e$type, ""), c("usage", "interrupted"))
  expect_length(live_decode(lm, '{"type":"error","error":{"code":"response_cancel_not_active"}}'), 0L)
})

test_that("sessions share the tested codec and close exactly once", {
  lm <- new_lm("openai", api_key = "test", env = character())
  cfg <- live_config("gpt-realtime")
  sent <- list(); closed <- 0L
  frames <- list('{"type":"response.output_text.delta","delta":"hi"}', '{"type":"response.done","response":{}}')
  connector <- function(url, headers, timeout, max_queue, max_frame_bytes) {
    expect_match(url, "^wss://api.openai.com/v1/realtime\\?model=gpt-realtime$")
    expect_identical(headers$authorization, "Bearer test")
    list(send = function(frame) sent[[length(sent) + 1L]] <<- frame,
      receive = function(timeout) { if (!length(frames)) return(NULL); out <- frames[[1L]]; frames <<- frames[-1L]; out },
      close = function() closed <<- closed + 1L)
  }
  session <- live(lm, cfg, connect = connector)
  expect_identical(sent[[1]], as_json(live_setup_frames(lm, cfg)[[1]]))
  events <- session$turn("hello")
  expect_identical(vapply(events, function(e) e$type, ""), c("text", "turn_end"))
  expect_length(sent, 3L)
  session$close(); session$close()
  expect_identical(closed, 1L)
  expect_null(session$next_event())
  expect_error(session$send(live_client_text_event("too late")), class = "TransportError")
})

test_that("Gemini waits for setup and closes on setup failure", {
  lm <- new_lm("gemini", api_key = "test", env = character()); closes <- 0L
  connector <- function(...) list(send = function(frame) NULL, receive = function(timeout) '{"error":{"message":"bad setup"}}', close = function() closes <<- closes + 1L)
  expect_error(live(lm, live_config("gemini-live"), connect = connector), class = "InvalidRequestError")
  expect_identical(closes, 1L)
})
