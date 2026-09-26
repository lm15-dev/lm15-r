test_that("callback paths, state and duplicate parameters are checked", {
  parse <- lm15:::.oauth_callback_reply
  expect_identical(parse("/wrong?code=secret", "/callback", "state")$status, 404L)
  expect_identical(parse("/callback?state=wrong&code=secret", "/callback", "state")$status, 400L)
  expect_identical(parse("/callback?state=state&code=a&code=b", "/callback", "state")$status, 400L)
  response <- parse("/callback?state=state&code=private%2Bcode", "/callback", "state")
  expect_identical(response$result$code, "private+code")
  expect_false(grepl("private", response$body, fixed = TRUE))
  expect_false(grepl("private", paste(capture.output(str(response$result)), collapse = ""), fixed = TRUE))
  # AUTH-18: an error return is accepted only with this attempt's state, and
  # a return with both a code and an error is invalid.
  expect_true(parse("/callback?state=state&error=denied&error_description=private", "/callback", "state")$failed)
  expect_false(parse("/callback?error=denied", "/callback", "state")$failed)
  expect_identical(parse("/callback?error=denied", "/callback", "state")$status, 400L)
  expect_identical(parse("/callback?state=state&code=a&error=denied", "/callback", "state")$status, 400L)
  expect_error(oauth_callback_listener(host = "0.0.0.0"), "127.0.0.1", fixed = TRUE)
})

test_that("a real loopback callback ignores bad redirects and accepts the correct one", {
  skip_if_not_installed("httpuv"); skip_if_not_installed("later"); skip_if_not_installed("callr")
  listener <- oauth_callback_listener(expected_state = "expected")
  on.exit(listener$close())
  child <- callr::r_bg(function(url) {
    statuses <- integer()
    for (suffix in c("?state=wrong&code=bad", "?state=expected", "?state=expected&code=private")) statuses <- c(statuses, curl::curl_fetch_memory(paste0(url, suffix))$status_code)
    statuses
  }, args = list(listener$redirect_uri))
  on.exit(child$kill(), add = TRUE)
  result <- listener$wait(timeout = 10)
  child$wait(10000)
  expect_identical(child$get_result(), c(400L, 400L, 200L))
  expect_identical(result$code, "private")
  listener$close(); listener$close()
})

test_that("callback waiting times out without claiming a login failure", {
  skip_if_not_installed("httpuv"); skip_if_not_installed("later")
  listener <- oauth_callback_listener(expected_state = "expected")
  on.exit(listener$close())
  expect_error(listener$wait(timeout = 0), class = "TimeoutError")
})
