test_that("two R processes rotate a stored refresh token only once", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  write_credentials(auth_body(), path = f$path, env = f$env)
  count_path <- file.path(f$home, "refreshes")
  root <- getNamespaceInfo(asNamespace("lm15"), "path")
  worker <- function(root, path, env, count_path) {
    if (file.exists(file.path(root, "src", "store.c"))) pkgload::load_all(root, quiet = TRUE)
    else library(lm15, lib.loc = dirname(root))
    transport <- function(wire) {
      cat("refresh\n", file = count_path, append = TRUE)
      Sys.sleep(0.15)
      list(status = 200L, body = charToRaw('{"access_token":"renewed","refresh_token":"rotated","expires_in":3600}'))
    }
    lm15::load_local_credential("xai", path = path, env = env, transport = transport)$credential$value
  }
  args <- list(root, f$path, f$env, count_path)
  first <- callr::r_bg(worker, args = args)
  on.exit(first$kill(), add = TRUE)
  second <- callr::r_bg(worker, args = args)
  on.exit(second$kill(), add = TRUE)
  first$wait(30000); second$wait(30000)
  expect_identical(first$get_result(), "renewed")
  expect_identical(second$get_result(), "renewed")
  expect_identical(readLines(count_path), "refresh")
})

test_that("cross-process contention is retryable, not a bad-login error", {
  f <- auth_fixture(); on.exit(unlink(f$home, recursive = TRUE))
  write_credentials(auth_body(), path = f$path, env = f$env)
  lock_path <- lm15:::.lock_path(f$path, f$env)
  ready <- file.path(f$home, "ready")
  child <- callr::r_bg(function(path, ready) {
    lock <- filelock::lock(path)
    on.exit(filelock::unlock(lock))
    writeLines("ready", ready)
    Sys.sleep(10)
  }, args = list(lock_path, ready))
  on.exit(child$kill(), add = TRUE)
  deadline <- proc.time()[["elapsed"]] + 5
  while (!file.exists(ready) && proc.time()[["elapsed"]] < deadline) Sys.sleep(0.01)
  expect_true(file.exists(ready))
  error <- tryCatch(load_local_credential("xai", path = f$path, env = f$env, lock_timeout = 0, transport = function(...) stop("must not run")), error = identity)
  expect_s3_class(error, "LockTimeoutError")
  expect_false(inherits(error, "AuthError"))
  expect_true(retryable(error))
  expect_identical(error$lock_path, lock_path)
})
