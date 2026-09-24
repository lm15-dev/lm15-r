test_that("applications can implement the client protocol with S3 methods", {
  registerS3method("complete", "lm15_test_backend", function(lm, request, ...) {
    response(request$model, message_assistant("custom"), "stop")
  }, envir = asNamespace("lm15"))
  client <- structure(list(), class = "lm15_test_backend")
  answer <- complete(client, request("model", list(message_user("hi"))))
  expect_identical(response_text(answer), "custom")
  expect_error(complete(structure(list(), class = "unknown_backend"), request("model", list(message_user("hi")))), "No completion method")
})
