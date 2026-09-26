# Auth: one scope's connections and their lifecycle (AUTH-14, 17, 19, 20, 24).
#
# A slot is one provider route in this scope (v1: instance "public"). Its
# record in `_lm15.slots` carries an identity generation (bumped on every new
# connection and on logout, never reused) and a credential revision (bumped
# on every renewal). A pinned caller holds (connection id, generation); a
# managed router consults the slot's current connection on every request.
#
# A provider entry without a record (an xAI login written before managed
# auth, or by Pi) reads as generation 1 with id "legacy-<provider>"; the
# record is written on the first managed commit, never on a read.

.renewal_lead_s <- 300  # AUTH-20.3: min(300 s, lifetime / 10)
.legacy_methods <- c(xai = "device", "claude-code" = "external:claude-code-cli", "openai-codex" = "external:codex-cli")

.iso_seconds <- function(seconds) format(as.POSIXct(floor(seconds), origin = "1970-01-01", tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
.token_urlsafe <- function(n) .random_urlsafe(n)
.radix_sort <- function(x) sort(x, method = "radix")

.slot <- function(provider, generation = 0, connection_id = NULL, revision = 0, kind = "account", method_id = "", instance_id = "public",
                  label = "", account_label = NULL, created_at = "", routes = character(), settings = list(), state = "ready",
                  renewal = "refresh_token", logged_out = FALSE, renewal_in_flight = NULL, attempt = NULL, verification = NULL,
                  previous_ids = character(), legacy = FALSE)
  list(provider = provider, generation = generation, connection_id = connection_id, revision = revision, kind = kind, method_id = method_id,
    instance_id = instance_id, label = label, account_label = account_label, created_at = created_at, routes = routes, settings = settings,
    state = state, renewal = renewal, logged_out = logged_out, renewal_in_flight = renewal_in_flight, attempt = attempt,
    verification = verification, previous_ids = previous_ids, legacy = legacy)

.slot_from_record <- function(provider, r) {
  s <- function(key, default = "") { v <- r[[key]]; if (is.null(v)) default else v }
  int <- function(key) { v <- .num(if (is.character(r[[key]]) && !inherits(r[[key]], "lm15_json_number")) as.numeric(r[[key]]) else r[[key]]); if (is.null(v)) 0 else v }
  .slot(provider, generation = int("generation"), connection_id = .json_str(r$connection_id), revision = int("revision"), kind = s("kind", "account"),
    method_id = s("method_id"), instance_id = s("instance_id", "public"), label = s("label"), account_label = .json_str(r$account_label),
    created_at = s("created_at"), routes = as.character(unlist(s("routes", list()))), settings = as.list(unclass(s("settings", json_object()))),
    state = s("state", "ready"), renewal = s("renewal", "refresh_token"), logged_out = isTRUE(r$logged_out), renewal_in_flight = r$renewal_in_flight,
    attempt = r$attempt, verification = r$verification, previous_ids = as.character(unlist(s("previous_ids", list()))))
}
.slot_record <- function(slot) {
  out <- json_object(generation = format(slot$generation, scientific = FALSE), connection_id = slot$connection_id, revision = format(slot$revision, scientific = FALSE),
    kind = slot$kind, method_id = slot$method_id, instance_id = slot$instance_id, label = slot$label, created_at = slot$created_at,
    routes = .json_array(as.list(slot$routes)), settings = .json_object(slot$settings), state = slot$state, renewal = slot$renewal)
  if (!is.null(slot$account_label) && nzchar(slot$account_label)) out$account_label <- slot$account_label
  if (isTRUE(slot$logged_out)) out$logged_out <- TRUE
  if (length(slot$renewal_in_flight)) out$renewal_in_flight <- slot$renewal_in_flight
  if (length(slot$attempt)) out$attempt <- slot$attempt
  if (length(slot$verification)) out$verification <- slot$verification
  if (length(slot$previous_ids)) out$previous_ids <- .json_array(as.list(utils::tail(slot$previous_ids, 8L)))
  out
}
.slot_connection <- function(slot) {
  if (is.null(slot$connection_id)) return(NULL)
  structure(list(id = slot$connection_id, provider = slot$provider, instance_id = slot$instance_id, kind = slot$kind, method_id = slot$method_id,
    routes = if (length(slot$routes)) slot$routes else slot$provider, label = if (nzchar(slot$label)) slot$label else slot$provider,
    created_at = slot$created_at, identity_generation = format(slot$generation, scientific = FALSE), credential_revision = format(slot$revision, scientific = FALSE),
    settings = slot$settings, account_label = slot$account_label), class = "lm15_connection")
}
print.lm15_connection <- function(x, ...) { cat("<lm15 connection ", x$id, ": ", x$provider, ", ", x$kind, " via ", x$method_id, ", \"", x$label, "\">\n", sep = ""); invisible(x) }

new_auth <- function(store, ..., clock = function() as.numeric(Sys.time()), monotonic = function() proc.time()[["elapsed"]], http = NULL, sleep = NULL) {
  .check_dots(...)
  if (!inherits(store, "lm15_store")) stop("new_auth() takes a store from file_store() or memory_store().", call. = FALSE)
  for (f in list(clock, monotonic)) if (!is.function(f)) stop("clock and monotonic must be functions.", call. = FALSE)
  if (!is.null(http) && !is.function(http)) stop("http must be a function taking a wire request.", call. = FALSE)
  if (!is.null(sleep) && !is.function(sleep)) stop("sleep must be a function of seconds.", call. = FALSE)
  self <- new.env(parent = emptyenv())
  self$store <- store; self$active <- new.env(parent = emptyenv()); self$closed <- FALSE

  check_open <- function() if (self$closed) .abort_auth("This Auth was closed.", reason = "storage_unavailable", stage = "resolution", recovery = "operator_action")
  descriptor <- function(provider) {
    .string(provider, "provider")
    e <- .login_entry(provider)
    if (is.null(e)) .abort_auth(paste0("'", provider, "' is not a provider lm15 can connect; see login_providers()."), reason = "method_unavailable", stage = "discovery", recovery = "choose_method", provider = provider)
    e$descriptor
  }
  view <- function(document, provider) {
    record <- document[[.store_meta]]$slots[[provider]]
    material <- document[[provider]]
    if (!.is_object(material)) material <- NULL
    if (!is.null(record)) return(list(slot = .slot_from_record(provider, record), material = material))
    if (is.null(material)) return(list(slot = .slot(provider), material = NULL))
    oauth <- identical(material$type, "oauth")
    list(slot = .slot(provider, generation = 1, connection_id = paste0("legacy-", provider), revision = 1, kind = if (oauth) "account" else "api_key",
      method_id = unname(.legacy_methods[provider]) %|NA|% "api_key", label = paste(provider, "(existing login)"), routes = provider, legacy = TRUE,
      renewal = if (oauth) "refresh_token" else "none"), material = material)
  }
  put <- function(document, slot, material) {
    meta <- document[[.store_meta]] %||% json_object(version = .store_version, slots = json_object())
    if (is.null(meta$version)) meta$version <- .store_version
    slots <- meta$slots %||% json_object()
    slots[[slot$provider]] <- .slot_record(slot)
    meta$slots <- .json_object(slots)
    document[[.store_meta]] <- meta
    if (is.null(material)) document[[slot$provider]] <- NULL else document[[slot$provider]] <- material
    .json_object(document)
  }
  now_iso <- function() .iso_seconds(clock())
  lead_ms <- function(lifetime) (if (is.null(lifetime)) .renewal_lead_s else min(.renewal_lead_s, lifetime / 10)) * 1000
  renewable <- function(slot, material) !slot$renewal %in% c("none", "recipe") && !is.null(.nonempty(material$refresh))
  usability <- function(slot, material) {
    if (slot$state == "needs_login") return(list("needs_login", NULL))
    if (slot$state == "indeterminate" || length(slot$renewal_in_flight)) return(list("indeterminate", NULL))
    if (is.null(material)) return(list("needs_login", NULL))
    flow <- .flow_for_material(slot$provider, material)
    expiry <- flow$expiry(material)
    if (identical(expiry, "never")) return(list("ready", "never"))
    if (is.null(expiry)) return(if (identical(material$type, "external")) list("ready", "unknown") else list("unknown", "unknown"))
    iso <- .iso_seconds(expiry / 1000)
    if (clock() * 1000 >= expiry - lead_ms(flow$lifetime(material))) return(list(if (renewable(slot, material)) "renewal_due" else "needs_login", iso))
    list("ready", iso)
  }

  providers <- function() lapply(.login_provider_ids(), function(p) .login_entry(p)$descriptor)
  methods <- function(provider) descriptor(provider)$methods
  connections <- function() {
    document <- store$read(); ids <- .login_provider_ids(); found <- list()
    for (key in .radix_sort(names(document))) {
      if (key == .store_meta || !key %in% ids) next
      c <- .slot_connection(view(document, key)$slot)
      if (!is.null(c)) found[[length(found) + 1L]] <- c
    }
    slots <- document[[.store_meta]]$slots
    for (key in names(slots)) {
      if (key %in% names(document)) next
      c <- .slot_connection(.slot_from_record(key, slots[[key]]))
      if (!is.null(c)) found[[length(found) + 1L]] <- c
    }
    found
  }
  status <- function(provider) {
    provider <- descriptor(provider)$id
    v <- view(store$read(), provider); slot <- v$slot
    verification <- if (length(slot$verification)) list(result = slot$verification$result, checked_at = slot$verification$checked_at, check = slot$verification$check, detail = slot$verification$detail)
    connection <- .slot_connection(slot)
    if (is.null(connection)) return(structure(list(provider = provider, presence = "absent", usability = "unknown", connection = NULL, expires_at = NULL,
      logged_out = isTRUE(slot$logged_out), verification = verification, detail = if (isTRUE(slot$logged_out)) "signed out; sign in again or pass a key explicitly"), class = "lm15_connection_status"))
    u <- usability(slot, v$material)
    structure(list(provider = provider, presence = "saved", usability = u[[1L]], connection = connection, expires_at = u[[2L]], logged_out = FALSE,
      verification = verification), class = "lm15_connection_status")
  }

  choose_method <- function(d, method, ui, allow_unverified) {
    fail <- function(message, reason = "method_unavailable", ...) .abort_auth(paste0(d$id, ": ", message), reason = reason, stage = "discovery", recovery = "choose_method", provider = d$id, ...)
    if (!is.null(method)) {
      .string(method, "method")
      chosen <- Filter(function(m) m$id == method, d$methods)
      if (!length(chosen)) fail(paste0("no login method '", method, "'; see login_methods()"))
      chosen <- chosen[[1L]]
      if (chosen$availability == "unavailable") fail(paste0("method '", method, "' is unavailable: ", chosen$reason), method_id = method)
      if (chosen$availability == "unverified" && !isTRUE(allow_unverified)) fail(paste0("method '", method, "' has no live receipt yet (", chosen$reason, "); pass allow_unverified = TRUE to try it knowing that"), method_id = method)
      return(chosen)
    }
    candidates <- Filter(function(m) m$availability == "supported" || (isTRUE(allow_unverified) && m$availability == "unverified"), d$methods)
    if (!length(candidates)) fail("no selectable login method here")
    if (length(candidates) == 1L) return(candidates[[1L]])
    if (is.null(ui)) .abort_auth(paste0(d$id, ": choose a login method (several are available) or supply a UI."), reason = "interaction_required", stage = "interaction", recovery = "choose_method", provider = d$id)
    options <- lapply(candidates, function(m) .option(m$id, m$label, m$billing_note %||% m$reason))
    answer <- tryCatch(ui$prompt(.prompt("select", "method", paste0("How do you want to connect to ", d$label, "?"), options = options)),
      interrupt = function(e) stop(.login_cancelled("login cancelled at the method prompt")))
    for (m in candidates) if (identical(m$id, answer)) return(m)
    .abort_auth(paste0(d$id, ": the UI answered an option that was not offered."), reason = "invalid_login_state", stage = "interaction", recovery = "choose_method", provider = d$id)
  }
  reserve <- function(provider, attempt_id, replace, lifetime) {
    now <- clock()
    document <- store$mutate(function(document) {
      v <- view(document, provider); slot <- v$slot; pending <- slot$attempt
      if (length(pending) && !identical(pending$id, attempt_id)) {
        started <- .num(pending$started_at_s) %||% 0; budget <- .num(pending$lifetime_s) %||% .attempt_lifetime_s
        if (now - started < budget) .abort_auth(paste0(provider, ": another sign-in is already in progress in this scope; finish it or cancel it (cancel_login())."),
          reason = "login_in_progress", stage = "reservation", recovery = "inspect_attempt", provider = provider, attempt_id = .json_str(pending$id))
      }
      if (!is.null(slot$connection_id) && is.null(replace)) .abort_auth(paste0(provider, ": a connection is already saved (", slot$connection_id, "); pass replace = that id to replace it, or logout first."),
        reason = "connection_exists", stage = "reservation", recovery = "select_connection", provider = provider, connection_id = slot$connection_id)
      if (!is.null(replace) && !identical(slot$connection_id, replace)) .abort_auth(paste0(provider, ": replace does not name the current connection; select again."),
        reason = "connection_changed", stage = "reservation", recovery = "select_connection", provider = provider, connection_id = slot$connection_id)
      slot$attempt <- json_object(id = attempt_id, expected_generation = format(slot$generation, scientific = FALSE), started_at_s = as.double(now), lifetime_s = as.double(lifetime))
      put(document, slot, v$material)
    })
    view(document, provider)$slot$generation
  }
  release <- function(provider, attempt_id) tryCatch(store$mutate(function(document) {
    v <- view(document, provider)
    if (!length(v$slot$attempt) || !identical(v$slot$attempt$id, attempt_id)) return(NULL)
    v$slot$attempt <- NULL
    put(document, v$slot, v$material)
  }), LM15Error = function(e) NULL)  # releasing must not mask the real failure
  commit <- function(provider, attempt_id, expected, method, result, settings) {
    created <- now_iso(); connection_id <- paste0("cn_", .token_urlsafe(12L))
    routes <- descriptor(provider)$routes
    if (!length(routes)) routes <- provider
    document <- tryCatch(store$mutate(function(document) {
      v <- view(document, provider); slot <- v$slot
      if (!length(slot$attempt) || !identical(slot$attempt$id, attempt_id)) .abort_auth(paste0(provider, ": this sign-in was cancelled before it could be saved."),
        reason = "invalid_login_state", stage = "persistence", commit_state = "not_committed", recovery = "restart_login", provider = provider, attempt_id = attempt_id)
      if (slot$generation != expected) .abort_auth(paste0(provider, ": the saved connection changed while you were signing in; select again."),
        reason = "connection_changed", stage = "persistence", commit_state = "not_committed", recovery = "select_connection", provider = provider, attempt_id = attempt_id)
      merged <- if (!is.null(slot$connection_id)) slot$settings else list()
      for (src in list(settings, result$settings)) for (k in names(src)) merged[[k]] <- src[[k]]
      new <- .slot(provider, generation = slot$generation + 1, connection_id = connection_id, revision = 1, kind = method$kind, method_id = method$id,
        label = result$label, account_label = result$account_label, created_at = created, routes = routes, settings = merged, state = "ready",
        renewal = result$renewal, previous_ids = c(slot$previous_ids, slot$connection_id))
      put(document, new, result$material)
    }), error = function(e) {
      if (inherits(e, "AuthOperationError") || !inherits(e, "LM15Error")) stop(e)
      # A grant may exist at the provider; nothing usable is returned and
      # nothing else is revoked as compensation (AUTH-19).
      .abort_auth(paste0(provider, ": signed in, but the credential could not be saved; repair the store and sign in again."), reason = "storage_unavailable",
        stage = "persistence", commit_state = "not_committed", recovery = "repair_storage", provider = provider, attempt_id = attempt_id)
    })
    .slot_connection(view(document, provider)$slot)
  }

  login <- function(provider, method = NULL, ui = NULL, settings = list(), answers = list(), replace = NULL, lifetime = .attempt_lifetime_s, allow_unverified = FALSE) {
    check_open()
    d <- descriptor(provider); provider <- d$id
    .number(lifetime, "lifetime")
    if (lifetime <= 0) stop("lifetime must be a positive number of seconds.", call. = FALSE)
    chosen <- choose_method(d, method, ui, allow_unverified)
    answers <- as.list(answers %||% list()); settings <- as.list(settings %||% list())
    flag <- new.env(parent = emptyenv()); flag$cancelled <- FALSE
    ctx <- .login_context(ui, monotonic() + lifetime, provider, monotonic, clock, http, sleep, function() flag$cancelled)
    attempt_id <- paste0("at_", .token_urlsafe(16L))
    # Reservation (AUTH-17/18): storage proven writable, one attempt per
    # slot, generation observed -- before any browser opens.
    store$reserve()
    expected <- reserve(provider, attempt_id, replace, lifetime)
    assign(provider, flag, envir = self$active)
    on.exit(if (exists(provider, envir = self$active, inherits = FALSE)) rm(list = provider, envir = self$active), add = TRUE)
    done <- FALSE
    on.exit(if (!done) release(provider, attempt_id), add = TRUE)  # any exit without a saved connection ends the attempt
    for (f in chosen$fields) if (is.null(answers[[f$id]])) {
      answers[[f$id]] <- if (!f$required && f$type != "select") ctx$prompt(.prompt("text", f$id, f$label))
        else if (f$type == "secret") ctx$prompt(.prompt("secret", f$id, f$label))
        else if (f$type == "select") ctx$prompt(.prompt("select", f$id, f$label, options = f$options))
        else ctx$prompt(.prompt("text", f$id, f$label))
    }
    flow <- .flow_for_method(provider, chosen$id)
    result <- tryCatch(flow$login(ctx, chosen, settings, answers), error = function(e) {
      if (inherits(e, "lm15_login_expired")) .abort_auth(paste0(provider, ": the sign-in was not completed within ", lifetime %/% 60, " minutes; start again."),
        reason = "login_expired", stage = "polling", recovery = "restart_login", provider = provider, attempt_id = attempt_id, method_id = chosen$id)
      if (inherits(e, "lm15_login_denied")) .abort_auth(paste0(provider, ": ", conditionMessage(e)), reason = "login_denied", stage = e$stage, recovery = "restart_login",
        provider = provider, attempt_id = attempt_id, method_id = chosen$id, status = e$status, provider_code = e$provider_code)
      if (inherits(e, "TransportError") && isTRUE(e$exchange_uncertain)) .abort_auth(paste0(provider, ": the network failed after the authorization code may have been sent; the code is one-use, so sign in again rather than retry."),
        reason = "indeterminate", stage = "exchange", commit_state = "not_committed", recovery = "restart_login", provider = provider, attempt_id = attempt_id, method_id = chosen$id)
      stop(e)
    })
    connection <- commit(provider, attempt_id, expected, chosen, result, settings)
    done <- TRUE
    connection
  }
  cancel_login <- function(provider) {
    provider <- descriptor(provider)$id
    outcome <- "none"
    store$mutate(function(document) {
      v <- view(document, provider)
      if (!length(v$slot$attempt)) { outcome <<- if (!is.null(v$slot$connection_id)) "complete" else "none"; return(NULL) }
      v$slot$attempt <- NULL; outcome <<- "cancelled"
      put(document, v$slot, v$material)
    })  # the durable record first (AUTH-19), then the running attempt
    flag <- self$active[[provider]]
    if (!is.null(flag)) flag$cancelled <- TRUE
    outcome
  }
  configure <- function(provider, method, answers = list(), settings = list(), replace = NULL) {
    check_open()
    d <- descriptor(provider); provider <- d$id
    .string(method, "method")
    chosen <- Filter(function(m) m$id == method, d$methods)
    if (!length(chosen)) .abort_auth(paste0(provider, ": no setup method '", method, "'; see login_methods()."), reason = "method_unavailable", stage = "discovery", recovery = "choose_method", provider = provider)
    chosen <- chosen[[1L]]
    if (!chosen$flow %in% c("form", "source_recipe")) .abort_auth(paste0(provider, ": '", method, "' is an interactive login; use login()."), reason = "method_unavailable", stage = "discovery", recovery = "choose_method", provider = provider)
    answers <- as.list(answers %||% list()); settings <- as.list(settings %||% list())
    for (f in chosen$fields) if (f$required && !nzchar(answers[[f$id]] %||% "")) .abort_auth(paste0(provider, ": '", method, "' needs '", f$id, "'."), reason = "interaction_required", stage = "interaction", recovery = "provide_input", provider = provider)
    attempt_id <- paste0("at_", .token_urlsafe(16L))
    store$reserve()
    expected <- reserve(provider, attempt_id, replace, 60)
    ctx <- .login_context(NULL, monotonic() + 60, provider, monotonic, clock, http, sleep)
    result <- tryCatch(.flow_for_method(provider, method)$login(ctx, chosen, settings, answers),
      error = function(e) {
        release(provider, attempt_id)
        if (inherits(e, "lm15_login_denied")) .abort_auth(paste0(provider, ": ", conditionMessage(e)), reason = "login_denied", stage = "interaction", recovery = "provide_input", provider = provider)
        stop(e)
      })
    commit(provider, attempt_id, expected, chosen, result, settings)
  }
  set_api_key <- function(provider, key, replace = NULL) {
    if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(trimws(key))) .abort_auth("set_api_key: the key is empty.", reason = "interaction_required", stage = "interaction", recovery = "provide_input", provider = provider)
    configure(provider, "api_key", answers = list(key = key), replace = replace)
  }
  resolve_target <- function(target) {
    .string(target, "target")
    if (startsWith(target, "cn_") || startsWith(target, "legacy-")) {
      for (c in connections()) if (identical(c$id, target)) return(list(provider = c$provider, id = c$id))
      for (key in names(store$read()[[.store_meta]]$slots)) {
        record <- store$read()[[.store_meta]]$slots[[key]]
        if (target %in% unlist(record$previous_ids)) return(list(provider = key, id = target))  # an old id: a no-op, never a newer id's removal
      }
      .abort_auth("No saved connection has that id.", reason = "attempt_unavailable", stage = "resolution", recovery = "select_connection")
    }
    list(provider = descriptor(target)$id, id = NULL)
  }
  logout <- function(target) {
    check_open()
    t <- resolve_target(target); provider <- t$provider
    outcome <- list(forgot = FALSE, generation = 0, routes = character())
    store$mutate(function(document) {
      v <- view(document, provider); slot <- v$slot
      if (!is.null(t$id) && !identical(slot$connection_id, t$id)) { outcome <<- list(forgot = FALSE, generation = slot$generation, routes = slot$routes); return(NULL) }
      if (is.null(slot$connection_id) && !length(slot$attempt)) { outcome <<- list(forgot = FALSE, generation = slot$generation, routes = slot$routes); return(NULL) }
      new <- .slot(provider, generation = slot$generation + 1, connection_id = NULL, revision = 0, kind = slot$kind, method_id = slot$method_id,
        routes = if (length(slot$routes)) slot$routes else provider, settings = list(), state = "ready", renewal = "none", logged_out = TRUE,
        previous_ids = c(slot$previous_ids, slot$connection_id))
      outcome <<- list(forgot = TRUE, generation = new$generation, routes = new$routes)
      flag <- self$active[[provider]]
      if (!is.null(flag)) flag$cancelled <- TRUE
      put(document, new, NULL)  # material removed; another tool's file is never touched
    })
    structure(list(provider = provider, forgot = outcome$forgot, routes = if (length(outcome$routes)) outcome$routes else provider,
      identity_generation = format(outcome$generation, scientific = FALSE)), class = "lm15_forget_result")
  }

  check_selected <- function(provider, slot, material, pinned) {
    if (!is.null(pinned) && (!identical(slot$connection_id, pinned[[1L]]) || !identical(format(slot$generation, scientific = FALSE), pinned[[2L]]))) {
      if (is.null(slot$connection_id)) .abort_auth(paste0(provider, ": the connection this client was bound to was signed out; connect again."), reason = "login_required", stage = "resolution", recovery = "restart_login", provider = provider, connection_id = pinned[[1L]])
      .abort_auth(paste0(provider, ": the saved connection was replaced after this client was bound; connect again."), reason = "connection_changed", stage = "resolution", recovery = "select_connection", provider = provider, connection_id = pinned[[1L]])
    }
    if (is.null(slot$connection_id) || is.null(material))
      .abort_auth(if (isTRUE(slot$logged_out)) paste0(provider, ": signed out; sign in again (login()) or pass a key explicitly (api_keys).") else paste0(provider, ": no saved connection in this scope; sign in with login() or connect()."),
        reason = "login_required", stage = "resolution", recovery = "restart_login", provider = provider)
    if (slot$state == "needs_login") .abort_auth(paste0(provider, ": the saved credential was rejected by the provider; sign in again."), reason = "login_required", stage = "resolution", recovery = "restart_login", provider = provider, connection_id = slot$connection_id)
    if (slot$state == "indeterminate" || length(slot$renewal_in_flight))
      .abort_auth(paste0(provider, ": a credential renewal was interrupted and its outcome is unknown; sign in again rather than reuse a possibly consumed token."),
        reason = "indeterminate", stage = "resolution", commit_state = "unknown", recovery = "restart_login", provider = provider, connection_id = slot$connection_id)
  }
  auth_from <- function(provider, flow, material, slot) tryCatch(flow$request_auth(material, slot$settings),
    lm15_login_denied = function(e) .abort_auth(paste0(provider, ": ", conditionMessage(e)), reason = "login_required", stage = "resolution", recovery = "restart_login", provider = provider, connection_id = slot$connection_id))
  renew <- function(provider, pinned) {
    ctx <- .login_context(NULL, monotonic() + 60, provider, monotonic, clock, http, sleep)
    store$transaction(function(txn) {
      document <- txn$read(); v <- view(document, provider); slot <- v$slot; material <- v$material
      check_selected(provider, slot, material, pinned)
      flow <- .flow_for_material(provider, material); expiry <- flow$expiry(material)
      if (identical(expiry, "never") || is.null(expiry) || clock() * 1000 < expiry - lead_ms(flow$lifetime(material)))
        return(auth_from(provider, flow, material, slot))  # a sibling renewed while we waited
      mark <- function(state, drop = FALSE, keep_marker = FALSE) {
        slot$state <- state
        if (!keep_marker) slot$renewal_in_flight <- NULL
        txn$write(put(document, slot, if (drop) NULL else material))
      }
      if (!renewable(slot, material)) {
        mark("needs_login", drop = TRUE)
        .abort_auth(paste0(provider, ": the saved credential expired and cannot be renewed; sign in again."), reason = "credential_rejected", stage = "renewal", commit_state = "committed", recovery = "restart_login", provider = provider, connection_id = slot$connection_id)
      }
      # A durable in-flight marker before the possibly rotating exchange.
      slot$renewal_in_flight <- json_object(started_at = now_iso(), revision = format(slot$revision, scientific = FALSE))
      txn$write(put(document, slot, material))
      # One handler that dispatches: in R, an error raised inside one of
      # several tryCatch handlers is caught by the handlers listed after it.
      result <- tryCatch(flow$renew(ctx, material, slot$settings), error = function(e) {
        if (inherits(e, "lm15_login_denied")) {
          mark("needs_login", drop = TRUE)
          .abort_auth(paste0(provider, ": renewal failed (", conditionMessage(e), "); sign in again."), reason = "credential_rejected", stage = "renewal", commit_state = "committed", recovery = "restart_login",
            provider = provider, connection_id = slot$connection_id, status = e$status, provider_code = e$provider_code)
        }
        if (inherits(e, "RateLimitError") || inherits(e, "ServerError")) { mark("ready"); stop(e) }  # known safe: keep the credential
        if (inherits(e, "TransportError") && !isTRUE(e$exchange_uncertain)) { mark("ready"); stop(e) }
        mark("indeterminate", keep_marker = TRUE)
        if (inherits(e, "TransportError")) .abort_auth(paste0(provider, ": the renewal exchange timed out after it may have reached the provider; a rotated token cannot be spent twice, so sign in again."),
          reason = "indeterminate", stage = "renewal", commit_state = "unknown", recovery = "restart_login", provider = provider, connection_id = slot$connection_id)
        stop(e)
      }, interrupt = function(e) { mark("indeterminate", keep_marker = TRUE); stop(.login_cancelled("renewal interrupted")) })
      slot$renewal_in_flight <- NULL; slot$revision <- slot$revision + 1; slot$state <- "ready"
      if (!is.null(result$account_label)) slot$account_label <- result$account_label
      txn$write(put(document, slot, result$material))
      auth_from(provider, flow, result$material, slot)
    })
  }
  request_auth <- function(provider, pinned = NULL) {
    provider <- descriptor(provider)$id
    if (!is.null(pinned) && (!is.character(pinned) || length(pinned) != 2L)) stop("pinned must be c(connection_id, identity_generation).", call. = FALSE)
    v <- view(store$read(), provider); slot <- v$slot; material <- v$material
    # A marker under a live renewal: only the lock can tell a sibling at
    # work from a dead process; wait for it, re-read, reuse (AUTH-20.4).
    if (length(slot$renewal_in_flight) && slot$state != "indeterminate") return(renew(provider, pinned))
    check_selected(provider, slot, material, pinned)
    flow <- .flow_for_material(provider, material); expiry <- flow$expiry(material)
    if (identical(expiry, "never") || is.null(expiry) || clock() * 1000 < expiry - lead_ms(flow$lifetime(material))) return(auth_from(provider, flow, material, slot))
    renew(provider, pinned)
  }
  credential_provider <- function(provider, pinned = NULL) {
    provider <- descriptor(provider)$id
    structure(function() request_auth(provider, pinned)$credential, class = c("lm15_credential", "function"))
  }
  verify <- function(provider, router = NULL) {
    check_open()
    provider <- descriptor(provider)$id
    d <- tryCatch(.definition(provider), error = function(e) NULL)
    if (is.null(d) || !isTRUE(d$access$supports$models)) return(list(result = "unverified", checked_at = NULL, check = "models", detail = "this route has no safe non-inference check"))
    checked <- now_iso()
    r <- router %||% new_router(auth = auth)
    result <- tryCatch({ list_models(router_lm(r, paste0(provider, ":verify"))); list(result = "valid", checked_at = checked, check = "models", detail = NULL) },
      AuthError = function(e) list(result = "rejected", checked_at = checked, check = "models", detail = e$code))
    tryCatch(store$mutate(function(document) {
      v <- view(document, provider)
      if (is.null(v$slot$connection_id)) return(NULL)
      v$slot$verification <- json_object(result = result$result, checked_at = result$checked_at, check = result$check, detail = result$detail)
      put(document, v$slot, v$material)
    }), LM15Error = function(e) NULL)
    result
  }
  close <- function() {
    self$closed <- TRUE
    for (name in ls(self$active)) self$active[[name]]$cancelled <- TRUE
    invisible(NULL)
  }
  auth <- structure(list(store = store, providers = providers, methods = methods, descriptor = descriptor, connections = connections, status = status,
    login = login, cancel_login = cancel_login, configure = configure, set_api_key = set_api_key, logout = logout, request_auth = request_auth,
    credential_provider = credential_provider, verify = verify, close = close), class = "lm15_auth")
  auth
}
print.lm15_auth <- function(x, ...) { cat("<lm15 Auth: ", x$store$description, ">\n", sep = ""); invisible(x) }
str.lm15_auth <- function(object, ...) { print.lm15_auth(object); invisible(NULL) }
print.lm15_connection_status <- function(x, ...) {
  cat("<lm15 connection status for ", x$provider, ": ", x$presence, ", ", x$usability, if (!is.null(x$expires_at)) paste0(", expires ", x$expires_at), if (isTRUE(x$logged_out)) ", signed out", ">\n", sep = "")
  invisible(x)
}
print.lm15_forget_result <- function(x, ...) { cat("<lm15 logout of ", x$provider, ": ", if (x$forgot) "forgotten" else "nothing to forget", ">\n", sep = ""); invisible(x) }

local_auth <- function(path = NULL, ...) new_auth(file_store(path), ...)
memory_auth <- function(...) new_auth(memory_store(), ...)

# The managed operations as functions. The provider (or target) comes first;
# `auth` is the scope, the private local file unless another is given.
# Constructing local_auth() reads and writes nothing (AUTH-14).
.auth_arg <- function(auth) { if (!inherits(auth, "lm15_auth")) stop("auth must come from local_auth(), memory_auth() or new_auth().", call. = FALSE); auth }
login <- function(provider, method = NULL, ..., auth = local_auth(), ui = NULL, settings = list(), answers = list(), replace = NULL,
                  lifetime = 15 * 60, allow_unverified = FALSE) {
  .check_dots(...)
  auth <- .auth_arg(auth)
  if (is.null(ui) && .interactive_terminal()) ui <- terminal_ui()
  auth$login(provider, method, ui = ui, settings = settings, answers = answers, replace = replace, lifetime = lifetime, allow_unverified = allow_unverified)
}
login_providers <- function(..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$providers() }
login_methods <- function(provider, ..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$methods(provider) }
connections <- function(..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$connections() }
status <- function(provider, ..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$status(provider) }
cancel_login <- function(provider, ..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$cancel_login(provider) }
configure <- function(provider, method, ..., answers = list(), settings = list(), replace = NULL, auth = local_auth()) { .check_dots(...); .auth_arg(auth)$configure(provider, method, answers, settings, replace) }
set_api_key <- function(provider, key, ..., replace = NULL, auth = local_auth()) { .check_dots(...); .auth_arg(auth)$set_api_key(provider, key, replace) }
logout <- function(target, ..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$logout(target) }
verify <- function(provider, ..., auth = local_auth()) { .check_dots(...); .auth_arg(auth)$verify(provider) }
request_auth <- function(provider, ..., pinned = NULL, auth = local_auth()) { .check_dots(...); .auth_arg(auth)$request_auth(provider, pinned) }
