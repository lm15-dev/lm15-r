credential_store <- function(..., path = credentials_path(env = env), env = NULL, lock_timeout = 30) {
  .check_dots(...); path <- .auth_expand(path, env)
  read <- function() {
    if (!file.exists(path)) return(json_object())
    value <- .read_auth_file(path)
    if (is.null(value)) .abort("Credential store is unreadable or malformed; contents are not shown.", "not_configured")
    value
  }
  get <- function(provider) { .string(provider, "provider"); read()[[provider]] }
  mutate <- function(provider, update) {
    .string(provider, "provider")
    if (!is.function(update)) stop("update must be a function accepting the current entry.", call. = FALSE)
    .with_credential_lock(path, function() {
      current <- read()
      value <- tryCatch(update(current[[provider]]), error = function(e) .abort("Credential update callback failed; diagnostics are suppressed.", "auth"))
      if (is.null(value)) return(invisible(NULL))
      if (!.is_object(value)) stop("A credential update must return a JSON object or NULL to leave it unchanged.", call. = FALSE)
      current[[provider]] <- value
      .write_credentials_unlocked(path, current)
      invisible(NULL)
    }, env, lock_timeout)
  }
  remove <- function(provider) {
    .string(provider, "provider")
    .with_credential_lock(path, function() {
      current <- read()
      if (!provider %in% names(current)) return(invisible(NULL))
      current[[provider]] <- NULL; .write_credentials_unlocked(path, current)
      invisible(NULL)
    }, env, lock_timeout)
  }
  structure(list(read = read, get = get, mutate = mutate, remove = remove), class = "lm15_credential_store")
}
print.lm15_credential_store <- function(x, ...) { cat("<lm15 credential store: contents hidden>\n"); invisible(x) }
str.lm15_credential_store <- function(object, ...) { print.lm15_credential_store(object); invisible(NULL) }
