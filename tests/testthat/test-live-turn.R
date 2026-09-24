turn_session <- function(events) {
  closes <- 0L; reads <- 0L
  structure(list(next_event = function(wait = 120) {
    reads <<- reads + 1L
    if (!length(events)) return(NULL)
    event <- events[[1L]]; events <<- events[-1L]; event
  }, close = function() closes <<- closes + 1L,
  reads = function() reads, closes = function() closes), class = "lm15_live_session")
}

test_that("turn results retain events, independent audio chunks and unknown usage", {
  events <- list(live_server_text_event("hel"), live_server_audio_event("YQ==", media_type = "audio/pcm"),
    live_server_audio_event("Yg==", media_type = "audio/pcm"), live_server_text_event("lo"),
    live_server_usage_event(usage = usage(input_tokens = 1L, output_tokens = 2L)),
    live_server_turn_end_event(usage = usage(input_tokens = 3L)))
  result <- materialize_turn(events)
  expect_true(result$ok)
  expect_identical(result$text, "hello")
  expect_identical(rawToChar(result$audio), "ab")
  expect_identical(result$usage$input_tokens, 4L)
  expect_null(result$usage$output_tokens)
  expect_null(result$usage$total_tokens)
  expect_identical(result$events, events)
})

test_that("result stops at a yielded tool call without reading its continuation", {
  session <- turn_session(list(live_server_text_event("checking"), live_server_tool_call_event("c", "lookup"), live_server_turn_end_event()))
  view <- turn(session)
  expect_identical(view$next_event()$type, "text")
  expect_identical(view$next_event()$type, "tool_call")
  answer <- result(view)
  expect_identical(answer$ended_by, "tool_call")
  expect_false(answer$ok)
  expect_identical(answer$tool_calls[[1L]]$name, "lookup")
  expect_identical(session$reads(), 2L)
  expect_identical(result(view), answer)
  expect_identical(result(turn(session))$ended_by, "turn_end")
})

test_that("closing a view does not close the session or fabricate success", {
  session <- turn_session(list(live_server_text_event("partial")))
  view <- turn(session); view$next_event(); view$close()
  expect_identical(view$snapshot()$ended_by, "incomplete")
  expect_error(result(view), class = "TransportError")
  expect_identical(session$closes(), 0L)
})

test_that("turn collectors refuse mixed media types and memory overflow", {
  expect_error(materialize_turn(list(live_server_audio_event("YQ==", media_type = "audio/pcm"), live_server_audio_event("Yg==", media_type = "audio/wav"))), "different audio")
  session <- turn_session(list(live_server_text_event("too large")))
  expect_error(result(turn(session, max_bytes = 1L)), class = "TransportError")
})
