# MAP-18 (lm15-contract spec/auth-failed.json, 2026-10-10): a provider's "this
# key is not valid" is an auth error whatever the status. AUTH-1/AUTH-5
# (amended 2026-10-10): a key put where a cloud identity name goes is never
# repeated. The error direction of the harness pins the classes; these tests
# pin the table against the contract and the refusal's text.

sentinel <- "SECRET-SENTINEL-DO-NOT-PRINT"

test_that("the MAP-18 forms are the contract's", {
  spec <- file.path(Sys.getenv("LM15_CONTRACT_DIR", file.path(test_path(), "..", "..", "..", "lm15-contract")), "spec", "auth-failed.json")
  skip_if_not(file.exists(spec), "contract checkout not beside this package")
  pinned <- jsonlite::fromJSON(spec, simplifyVector = FALSE)$forms
  pinned <- lapply(pinned, function(f) f[setdiff(names(f), c("providers", "evidence"))])
  expect_identical(lapply(pinned, function(f) f[order(names(f))]), lapply(lm15:::.auth_failed_forms, function(f) f[order(names(f))]))
})

test_that("a reason counts only from a google.rpc.ErrorInfo detail", {
  help <- list(details = list(list(`@type` = "type.googleapis.com/google.rpc.Help", reason = "API_KEY_INVALID")))
  expect_length(lm15:::.google_error_reasons(help), 0L)
  expect_false(lm15:::.pinned_auth_failure("INVALID_ARGUMENT", "API key not valid.", lm15:::.google_error_reasons(help)))
})

test_that("a key given as a named credential is never shown", {
  for (provider in c("anthropic", "vertex")) {
    err <- tryCatch(lm15:::.check_named(provider, sentinel), error = function(e) e)
    expect_s3_class(err, "error")
    expect_false(grepl(sentinel, conditionMessage(err), fixed = TRUE))
    expect_match(conditionMessage(err), "may be a key", fixed = TRUE)
  }
  err <- tryCatch(new_router(credentials = setNames(list(sentinel), "vertex")), error = function(e) e)
  expect_false(grepl(sentinel, conditionMessage(err), fixed = TRUE))
})
