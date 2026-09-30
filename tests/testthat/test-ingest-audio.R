# MAP-12 rule 4 (amended 2026-09-29): each input_audio format reads as its true
# media type; an unknown format is malformed; a builder with no audio slot
# refuses at send (MAP-10).

audio_body <- function(format) list(model = "gemini-3.8-flash", messages = list(list(role = "user", content = list(
  list(type = "text", text = "Transcribe."),
  list(type = "input_audio", input_audio = list(data = "T2dnUw==", format = format))
))))

test_that("input_audio reads its true media type", {
  expected <- c(
    wav = "audio/wav", mp3 = "audio/mpeg", mpeg = "audio/mpeg", ogg = "audio/ogg", opus = "audio/opus",
    flac = "audio/flac", aac = "audio/aac", aiff = "audio/aiff", webm = "audio/webm"
  )
  for (format in names(expected)) {
    part <- request_from_openai_chat(audio_body(format))$messages[[1]]$parts[[2]]
    expect_s3_class(part, "lm15_AudioPart")
    expect_identical(part$media_type, expected[[format]], info = format)
    expect_identical(part$data, "T2dnUw==", info = format)
  }
  expect_error(request_from_openai_chat(audio_body("midi")), "must be one of")
  expect_error(request_from_openai_chat(audio_body(1)), "must be one of")
  expect_error(request_from_openai_chat(audio_body(c("ogg", "wav"))), "must be one of")
})

test_that("an ogg clip reaches Gemini inline and the chat wire refuses it", {
  req <- request_from_openai_chat(audio_body("ogg"))
  sent <- lm15:::.json_decode(rawToChar(build_request(new_lm("gemini", api_key = "k"), req)$body))
  expect_identical(sent$contents[[1]]$parts[[2]]$inlineData$mimeType, "audio/ogg")
  expect_identical(sent$contents[[1]]$parts[[2]]$inlineData$data, "T2dnUw==")
  expect_error(build_request(new_lm("openai-chat", api_key = "k"), req), class = "UnsupportedFeatureError")
})
