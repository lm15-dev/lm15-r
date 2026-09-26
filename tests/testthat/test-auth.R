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

test_that("a managed device login honors pending, slow-down, and saves before it returns", {
  now <- 1788436800; delays <- numeric(); polls <- 0L; seen <- character()
  http <- function(wire) {
    seen <<- c(seen, wire$url)
    if (endsWith(wire$url, "/device/code")) return(list(status = 200L, headers = list("content-type" = "application/json"), body = charToRaw('{"device_code":"private-device","user_code":"PUBLIC","verification_uri":"https://auth.x.ai/activate","expires_in":100,"interval":2}')))
    polls <<- polls + 1L
    body <- switch(polls, '{"error":"authorization_pending"}', '{"error":"slow_down"}', '{"access_token":"fresh","refresh_token":"rotated","expires_in":3600}')
    list(status = if (polls < 3L) 400L else 200L, headers = list("content-type" = "application/json"), body = charToRaw(body))
  }
  notices <- list()
  ui <- list(notify = function(n) notices[[length(notices) + 1L]] <<- n, prompt = function(p) stop("no prompt expected"))
  auth <- new_auth(memory_store(), clock = function() now, monotonic = function() now, http = http, sleep = function(n) { delays <<- c(delays, n); now <<- now + n })
  connection <- login("xai", "device", auth = auth, ui = ui)
  expect_identical(delays, c(2, 2, 7))
  expect_identical(connection$identity_generation, "1")
  expect_identical(request_auth("xai", auth = auth)$credential$value, "fresh")
  expect_identical(notices[[1L]]$user_code, "PUBLIC")
  printed <- paste(capture.output(print(connection), print(status("xai", auth = auth))), collapse = "")
  expect_false(grepl("fresh|rotated|private-device", printed))
  expect_error(login("xai", "device", auth = auth, ui = ui), class = "AuthOperationError")
})

test_that("unverified and unknown login methods refuse without any network activity", {
  auth <- new_auth(memory_store(), http = function(...) stop("must not run"))
  err <- tryCatch(login("claude-code", "browser", auth = auth), error = identity)
  expect_s3_class(err, "AuthOperationError")
  expect_identical(err$reason, "method_unavailable")
  expect_identical(tryCatch(login("nope-provider", auth = auth), error = function(e) e$reason), "method_unavailable")
  expect_identical(tryCatch(connect(auth = auth), error = function(e) e$reason), "interaction_required")
})

test_that("a bound client pins its connection and refuses another model or identity", {
  sent <- list()
  transport <- function(wire, ...) {
    sent[[length(sent) + 1L]] <<- wire
    list(status = 200L, headers = list(), body = charToRaw('{"id":"r","model":"gpt-5-mini","output":[{"type":"message","role":"assistant","content":[{"type":"output_text","text":"hi"}]}],"usage":{"input_tokens":1,"output_tokens":1}}'))
  }
  auth <- memory_auth()
  first <- set_api_key("openai", "key-one", auth = auth)
  client <- bind_model("openai", "gpt-5-mini", auth = auth, transport = transport)
  expect_identical(response_text(complete(client, "Say hi.")), "hi")
  expect_identical(sent[[1L]]$headers$authorization, "Bearer key-one")
  expect_identical(client$request("x")$model, "openai:gpt-5-mini")
  expect_identical(tryCatch(complete(client, request("gpt-4o", list(message_user("x")))), error = function(e) e$reason), "selection_mismatch")
  set_api_key("openai", "key-two", replace = first$id, auth = auth)
  expect_identical(tryCatch(complete(client, "again"), error = function(e) e$reason), "connection_changed")
  expect_length(sent, 1L)
  router <- new_router(auth = auth, transport = transport)
  complete(router, request("openai:gpt-5-mini", list(message_user("x"))))
  expect_identical(sent[[2L]]$headers$authorization, "Bearer key-two")
})

test_that("a managed router never reads an ambient key, and logout keeps it blocked", {
  auth <- memory_auth()
  router <- new_router(auth = auth, env = c(OPENAI_API_KEY = "ambient"), transport = function(...) stop("must not send"))
  err <- tryCatch(complete(router, request("openai:gpt-5-mini", list(message_user("x")))), error = identity)
  expect_identical(err$reason, "login_required")
  report <- explain_auth("openai", env = c(OPENAI_API_KEY = "ambient"), auth = auth)
  expect_false(report$configured)
  expect_identical(vapply(report$steps, function(s) s$state, ""), c("absent", "absent", "shadowed"))
})

test_that("named cloud identities are checked before any walk", {
  expect_error(new_router(credentials = list(azure = "someone")), class = "NotConfiguredError")
  expect_error(new_router(credentials = list(openai = "cli")), class = "NotConfiguredError")
  expect_error(new_router(api_keys = list(azure = "k"), credentials = list(azure = "cli")), class = "NotConfiguredError")
  report <- explain_auth("bedrock-chat", env = c(HOME = tempdir(), AWS_EC2_METADATA_DISABLED = "true"), credential = "platform")
  expect_identical(vapply(report$steps, function(s) s$kind, ""), c("api_keys", "container", "imds"))
})
