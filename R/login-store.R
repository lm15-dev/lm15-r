# The managed credential store (AUTH-25), version 1: the layout every lm15
# SDK shares (lm15-contract auth/managed/store-layout.md). One JSON document
# per scope: provider entries keyed by route (secret, provider-private
# shape) plus one non-secret `_lm15` block of slot records. It is the same
# file the legacy xAI login writes, so one entry has one owner.
#
# Every commit is one serialized read-modify-write under the AUTH-4 lock on
# the canonical path (the lock every SDK takes), written privately and
# atomically. An unreadable or unrecognised document is a typed error, never
# an empty store, and is never overwritten.

.store_meta <- "_lm15"
.store_version <- 1L

.storage_error <- function(message, reason = "storage_unavailable", stage = "persistence")
  .abort_auth(message, reason = reason, stage = stage, commit_state = "not_committed", recovery = "repair_storage", operation = "store")

.validate_document <- function(data, where) {
  if (!.is_object(data)) .storage_error(paste0("Credential store at ", where, " is not a JSON object; not touching it."))
  meta <- data[[.store_meta]]
  if (!is.null(meta)) {
    if (!.is_object(meta)) .storage_error(paste0("Credential store at ", where, " has a malformed _lm15 block; not touching it."))
    version <- .json_int(meta$version)
    if (is.null(version) || is.logical(meta$version) || version != .store_version)
      .storage_error(paste0("Credential store at ", where, " is a managed-store version this lm15 does not read (it reads version ", .store_version, "). Upgrade lm15 or point LM15_CREDENTIALS_PATH at another file."), reason = "unsupported_store_version")
    slots <- meta$slots %||% json_object()
    if (!.is_object(slots) || !all(vapply(slots, .is_object, logical(1)))) .storage_error(paste0("Credential store at ", where, " has malformed slot metadata; not touching it."))
  }
  for (key in setdiff(names(data), .store_meta)) if (!.is_object(data[[key]]))
    .storage_error(paste0("Credential store at ", where, ": an entry is not an object; not touching it."))
  data
}

file_store <- function(path = NULL, ..., lock_timeout = 30) {
  .check_dots(...)
  .number(lock_timeout, "lock_timeout")
  # Anchor the absolute path now (AUTH-14): a later setwd() must not move the
  # store. Nothing is read or created here.
  chosen <- if (is.null(path)) credentials_path() else .auth_expand(.string(path, "path"), NULL)
  if (!grepl("^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)", chosen)) chosen <- file.path(getwd(), chosen)
  load <- function() {
    if (!file.exists(chosen)) return(json_object())
    text <- tryCatch({
      size <- file.info(chosen)$size
      if (is.na(size)) stop("unreadable")
      con <- file(chosen, "rb"); on.exit(close(con), add = TRUE)
      rawToChar(readBin(con, "raw", n = size))
    }, error = function(e) .storage_error(paste0("Could not read the credential store at ", chosen, ".")))
    if (!validUTF8(text)) .storage_error(paste0("Credential store at ", chosen, " is not UTF-8 text; not touching it."))
    data <- tryCatch(.json_decode(text), error = function(e) .storage_error(paste0("Credential store at ", chosen, " is not valid JSON; not touching it.")))
    .validate_document(data, chosen)
  }
  transaction <- function(fn) .with_credential_lock(chosen, function() {
    write <- function(document) {
      .validate_document(document, chosen)
      tryCatch(.write_credentials_unlocked(chosen, document), LM15Error = function(e) .storage_error(paste0("Could not write the credential store at ", chosen, ".")))
      invisible(NULL)
    }
    fn(list(read = load, write = write))
  }, NULL, lock_timeout)
  reserve <- function() {
    ok <- dir.exists(dirname(chosen)) || dir.create(dirname(chosen), recursive = TRUE, showWarnings = FALSE, mode = "0700")
    if (!ok) .storage_error(paste0("Cannot create ", dirname(chosen), " for the credential store."), stage = "reservation")
    if (file.exists(chosen) && file.access(chosen, 2L) != 0L) .storage_error(paste0("Credential store at ", chosen, " is not writable."), stage = "reservation")
    .with_credential_lock(chosen, load, NULL, lock_timeout)
    invisible(NULL)
  }
  .new_store(read = load, transaction = transaction, reserve = reserve, description = chosen)
}

memory_store <- function() {
  state <- new.env(parent = emptyenv()); state$document <- json_object()
  transaction <- function(fn) fn(list(read = function() state$document, write = function(document) { state$document <- .validate_document(document, "memory"); invisible(NULL) }))
  .new_store(read = function() state$document, transaction = transaction, reserve = function() invisible(NULL), description = "memory")
}

.new_store <- function(read, transaction, reserve, description) {
  # Serialized read-modify-write: `fn` gets the current document and returns
  # the replacement, or NULL to leave the store untouched.
  mutate <- function(fn) transaction(function(txn) {
    current <- txn$read()
    replacement <- fn(current)
    if (is.null(replacement)) return(current)
    txn$write(replacement)
    replacement
  })
  structure(list(read = read, transaction = transaction, mutate = mutate, reserve = reserve, description = description), class = "lm15_store")
}
print.lm15_store <- function(x, ...) { cat("<lm15 credential store: ", x$description, "; contents hidden>\n", sep = ""); invisible(x) }
str.lm15_store <- function(object, ...) { print.lm15_store(object); invisible(NULL) }
