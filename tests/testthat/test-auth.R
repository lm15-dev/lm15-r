test_that("stored refresh is private, atomic, and preserves unrelated data", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  write_credentials(auth_body(), path = f$path, env = f$env)
  count <- 0L
  transport <- function(wire) {
    count <<- count + 1L
    expect_identical(wire$url, "https://auth.x.ai/oauth2/token")
    expect_match(rawToChar(wire$body), "refresh_token=old-refresh", fixed = TRUE)
    list(status = 200L, body = charToRaw('{"access_token":"new-access","refresh_token":"new-refresh","expires_in":3600}'), headers = list())
  }
  clock <- as.POSIXct("2026-09-03 12:00:00", tz = "UTC")
  got <- load_local_credential("xai", path = f$path, env = f$env, now = clock, transport = transport)
  expect_identical(got$credential$value, "new-access")
  expect_identical(got$refresh_token, "new-refresh")
  expect_identical(count, 1L)
  expect_identical(load_local_credential("xai", path = f$path, env = f$env, now = clock, transport = transport)$credential$value, "new-access")
  expect_identical(count, 1L)
  saved <- lm15:::.read_auth_file(f$path)
  expect_identical(as_json(saved$other), '{"preserve":null}')
  if (.Platform$OS.type != "windows") expect_identical(as.integer(file.info(f$path)$mode), 384L)
  expect_length(list.files(f$home, pattern = "^\\.lm15-", all.files = TRUE), 0L)
})

test_that("failed refresh never switches to a paid ambient key or damages the store", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  write_credentials(auth_body(), path = f$path, env = f$env)
  before <- readBin(f$path, "raw", n = file.info(f$path)$size)
  transport <- function(wire) stop("SECRET-SENTINEL-DO-NOT-PRINT")
  error <- tryCatch(new_lm("xai", env = c(f$env, XAI_API_KEY = "paid-key"), credentials_path = f$path, transport = transport), error = identity)
  expect_s3_class(error, "AuthError")
  expect_false(grepl("SECRET-SENTINEL-DO-NOT-PRINT", conditionMessage(error), fixed = TRUE))
  expect_identical(readBin(f$path, "raw", n = file.info(f$path)$size), before)
})

test_that("offline reports recognize renewable credentials without refreshing", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  write_credentials(auth_body(), path = f$path, env = f$env)
  report <- explain_auth("xai", path = f$path, env = c(f$env, XAI_API_KEY = "paid"))
  expect_true(report$configured)
  expect_identical(report$steps[[2]]$state, "selected")
  expect_identical(report$steps[[3]]$state, "shadowed")
  expect_false(grepl("old-refresh|old-access", paste(capture.output(print(report)), collapse = "\n")))
})

test_that("credential providers are called once per request and failures keep their type", {
  calls <- 0L
  client <- new_lm("openai", api_key = function() { calls <<- calls + 1L; paste0("key-", calls) }, env = character())
  req <- request("gpt-test", list(message_user("hi")))
  expect_identical(build_request(client, req)$headers$authorization, "Bearer key-1")
  expect_identical(build_request(client, req)$headers$authorization, "Bearer key-2")
  locked <- new_lm("openai", api_key = function() stop(lm15_error("busy", code = "lock_timeout")), env = character())
  expect_error(build_request(locked, req), class = "LockTimeoutError")
})

test_that("PKCE matches RFC 7636 Appendix B", {
  expect_identical(pkce_challenge("dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk"), "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM")
  expect_error(pkce_challenge("short"))
  pair <- generate_pkce()
  expect_identical(pair$challenge, pkce_challenge(pair$verifier))
  expect_false(grepl(pair$verifier, paste(capture.output(str(pair)), collapse = ""), fixed = TRUE))
})

test_that("device polling honors pending, slow-down, and rotated credentials", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  now <- 1788436800; delays <- numeric(); polls <- 0L
  clock <- function() as.POSIXct(now, origin = "1970-01-01", tz = "UTC")
  transport <- function(wire) {
    if (endsWith(wire$url, "/device/code")) return(list(status = 200L, body = charToRaw('{"device_code":"private-device","user_code":"PUBLIC","verification_uri":"https://auth.x.ai/activate","expires_in":100,"interval":2}')))
    polls <<- polls + 1L
    body <- switch(polls, '{"error":"authorization_pending"}', '{"error":"slow_down"}', '{"access_token":"fresh","refresh_token":"rotated","expires_in":3600}')
    list(status = if (polls < 3L) 400L else 200L, body = charToRaw(body))
  }
  device <- start_device_login(transport = transport, clock = clock)
  result <- poll_device_login(device, path = f$path, env = f$env, transport = transport, clock = clock, sleep = function(n) { delays <<- c(delays, n); now <<- now + n })
  expect_identical(delays, c(2, 2, 7))
  expect_identical(result$credential$value, "fresh")
  expect_false(grepl("private-device", paste(capture.output(print(device)), collapse = ""), fixed = TRUE))
})

test_that("unowned login flows refuse without any network activity", {
  expect_error(login("claude-code", transport = function(...) stop("must not run")), class = "UnsupportedFeatureError")
})
