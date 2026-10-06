# INV-056: no default SSE size limit; linear line splitting. The former
# defaults (1 MiB per line, 8 MiB per event here; 64 KiB / 1 MiB in the
# other SDKs) refused real streams: Gemini sends a 4K image as one 29.7 MB
# line (lm15-contract receipts/2026-10-06-sse-long-lines).

sse_frames <- function(body, size, ...) {
  frames <- list()
  sse <- lm15:::.new_sse(function(data, event) frames[[length(frames) + 1L]] <<- list(event = event, data = data), ...)
  bytes <- if (is.raw(body)) body else charToRaw(body)
  starts <- seq.int(1L, max(1L, length(bytes)), by = size)
  for (s in starts) if (length(bytes)) sse$feed(bytes[s:min(length(bytes), s + size - 1L)])
  sse$finish()
  frames
}

test_that("a line over every former limit parses by default", {
  text <- strrep("x", 9 * 1024^2)
  body <- paste0("event: response.completed\ndata: {\"text\": \"", text, "\"}\n\n")
  frames <- sse_frames(body, 16L * 1024L)
  expect_length(frames, 1L)
  expect_identical(frames[[1L]]$event, "response.completed")
  expect_identical(nchar(frames[[1L]]$data), nchar(text) + 12L)
})

test_that("caps are opt-in and still refuse, also while a line arrives", {
  expect_error(sse_frames("data: too long\n", 64L, max_line_bytes = 4), class = "TransportError")
  expect_error(sse_frames("data: 1\ndata: 2\n", 64L, max_event_bytes = 8), class = "TransportError")
  sse <- lm15:::.new_sse(function(data, event) NULL, max_line_bytes = 1024)
  sse$feed(as.raw(rep(97L, 512L)))
  sse$feed(as.raw(rep(97L, 512L)))
  expect_error(sse$feed(as.raw(rep(97L, 512L))), class = "TransportError")
})

test_that("chunked feeding matches one-shot parsing for any chunking", {
  pieces <- c("a", "\n", "bc", "\r\n", "\n\n", "data: {}\n", "data: x\n\n", strrep("z", 300))
  seed <- 56
  rand_below <- function(n) { seed <<- (seed * 1103515245 + 12345) %% 2147483648; seed %% n }
  for (trial in 1:200) {
    body <- ""
    for (k in seq_len(rand_below(40))) body <- paste0(body, pieces[rand_below(length(pieces)) + 1L])
    want <- sse_frames(body, max(1L, nchar(body, type = "bytes")))
    expect_identical(sse_frames(body, 1L + rand_below(7)), want)
  }
})

test_that("a 30 MB line in 16 KiB reads splits in linear time", {
  skip_on_cran()  # tens of MB and a timing bound: CRAN machines are shared
  body <- c(charToRaw("data: "), as.raw(rep(97L, 30 * 1024^2)), charToRaw("\n\n"))
  elapsed <- system.time(frames <- sse_frames(body, 16L * 1024L))[["elapsed"]]
  expect_length(frames, 1L)
  expect_identical(nchar(frames[[1L]]$data), 30L * 1024L * 1024L)
  # The old splitter re-concatenated and rescanned the pending line on every
  # read (about 1,900 reads x 15 MB here): minutes.
  expect_lt(elapsed, 10)
})

test_that("base64 validation keeps its rule", {
  ok <- c("QUJD", "QUI=", "QQ==", "data:image/png;base64,QUJD", " QU JD \n")
  bad <- c("", "QUJ", "QQ===", "Q===", "QU*D", "=QUJ", "QUJD\u00e9")
  for (x in ok) expect_identical(lm15:::.base64(x, "data"), x)
  for (x in bad) expect_error(lm15:::.base64(x, "data"), "base64|non-empty")
})

test_that("the JSON check fails exactly where encoding fails", {
  values <- list(json_object(a = 1L), list(1, 2), NA, c(1, 2), Inf, json_object(a = NA), 1.5, "x", TRUE, Sys.Date(),
                 structure(list(1, 2), names = c("a", "a")), lm15:::.json_number("1e400"))
  outcome <- function(f, x) tryCatch({ f(x); "ok" }, error = function(e) conditionMessage(e))
  for (x in values) expect_identical(outcome(lm15:::.json_check, x), outcome(lm15:::.json_encode, x))
})

test_that("a 30 MB generated image replays in seconds", {
  skip_on_cran()  # tens of MB and a timing bound: CRAN machines are shared
  image <- jsonlite::base64_enc(as.raw(rep_len(0:255, 22500000L)))
  image <- gsub("\n", "", image, fixed = TRUE)
  body <- paste0('data: {"candidates":[{"content":{"role":"model","parts":[{"inlineData":{"mimeType":"image/png","data":"', image,
                 '"}}]},"finishReason":"STOP","index":0}],"usageMetadata":{"promptTokenCount":3,"candidatesTokenCount":2,"totalTokenCount":5}}\r\n\r\n')
  router <- new_router(api_keys = list(gemini = "k"))
  req <- request("gemini:gemini-3-pro-image", list(message_user("x")), config = config(extensions = list(output = "image")))
  lm <- lm15:::.route(router, req)$lm
  elapsed <- system.time(out <- replay_stream(lm, req, body))[["elapsed"]]
  parts <- Filter(function(p) identical(p$type, "image"), out$response$message$parts)
  expect_identical(parts[[1L]]$data, image)
  # Before: about 20 s (a POSIX regex and a full JSON encoding on every check).
  expect_lt(elapsed, 15)
})
