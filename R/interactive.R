# The interactive layer (AUTH-16, AUTH-23): a terminal UI, model choices,
# connect() and the bound client it returns.
#
# connect() is for a person at a console. Without a console and without a
# `ui`, it fails before reading any secret or touching the network. It
# never sends a prompt, sets a process-wide default, changes another router
# or falls back to another account when one fails.

.interactive_terminal <- function() interactive() && !isTRUE(getOption("lm15.noninteractive"))

# A terminal adapter: notices go to stderr; answers come from readline().
# A secret is read without echo when the askpass package is available (it
# comes with openssl); otherwise the terminal refuses rather than echo it.
# A browser opens only when asked (open_browser = TRUE), and only for https.
terminal_ui <- function(..., open_browser = FALSE) {
  .check_dots(...)
  say <- function(...) { cat(..., "\n", sep = "", file = stderr()); invisible(NULL) }
  ask <- function(label) {
    if (!interactive()) stop(.login_cancelled("no console to answer the prompt"))
    readline(label)
  }
  open <- function(url) if (isTRUE(open_browser) && startsWith(url, "https://")) try(utils::browseURL(url), silent = TRUE)
  notify <- function(notice) {
    switch(notice$type,
      auth_url = { say("\nOpen this link to sign in:\n  ", notice$url, "\n", notice$instructions); open(notice$url) },
      device_code = { say("\nOpen ", notice$verification_url, "\nand enter this code:  ", notice$user_code, "\n(the code is valid for about ", notice$expires_in_s %/% 60, " minutes)"); open(notice$verification_url) },
      progress = say("... ", notice$message),
      info = say(notice$message))
  }
  prompt <- function(prompt) {
    switch(prompt$type,
      secret = {
        if (!requireNamespace("askpass", quietly = TRUE)) .abort_auth("Reading a secret without echoing it needs the askpass package; install it, or pass the value with answers =.", reason = "interaction_required", stage = "interaction", recovery = "provide_input")
        value <- askpass::askpass(paste0(prompt$label, ": "))
        if (is.null(value)) stop(.login_cancelled("login cancelled at the prompt"))
        value
      },
      select = {
        say("\n", prompt$label)
        for (i in seq_along(prompt$options)) {
          o <- prompt$options[[i]]
          say("  ", i, ". ", o$label, if (!is.null(o$description)) paste0("  -- ", o$description))
        }
        repeat {
          raw <- trimws(ask("Choose a number: "))
          number <- suppressWarnings(as.integer(raw))
          if (!is.na(number) && number >= 1L && number <= length(prompt$options)) return(prompt$options[[number]]$id)
          for (o in prompt$options) if (identical(raw, o$id)) return(o$id)
          say("Not one of the choices.")
        }
      },
      manual_code = ask(paste0(prompt$label, "\n> ")),
      ask(paste0(prompt$label, ": ")))
  }
  structure(list(notify = notify, prompt = prompt), class = "lm15_ui")
}
print.lm15_ui <- function(x, ...) { cat("<lm15 sign-in UI>\n"); invisible(x) }

.capabilities <- c("reasoning", "vision", "structured-output")
.capability_state <- function(info, name) {
  inference <- info$inference
  if (is.null(inference)) return("unknown")
  if (name == "reasoning") return(if (isTRUE(inference$supports_reasoning)) "supported" else "unsupported")
  if (name == "vision") return(if ("image" %in% unlist(inference$input_modalities)) "supported" else "unsupported")
  "unknown"  # structured output is not recorded in model metadata; say so
}

# The models the saved connection on `provider` can select (AUTH-23). Each
# choice says where it came from: "application" (a registry you supplied),
# "provider" (the account's own list, fetched now with the saved credential:
# refresh = TRUE; no inference). With `capability`, only choices known to
# support it are returned unless include_unknown = TRUE.
model_choices <- function(provider, ..., auth = local_auth(), refresh = FALSE, capability = NULL, include_unknown = FALSE, registry = NULL, transport = NULL) {
  .check_dots(...); auth <- .auth_arg(auth)
  if (!is.null(capability) && !capability %in% .capabilities) stop(paste("capability must be one of", paste(.capabilities, collapse = ", ")), call. = FALSE)
  connection <- auth$status(provider)$connection
  if (is.null(connection)) .abort_auth(paste0(provider, ": no saved connection to list models for."), reason = "login_required", stage = "catalog", recovery = "restart_login", provider = provider)
  infos <- list(); source <- "application"; fetched <- NULL
  if (isTRUE(refresh)) {
    router <- new_router(auth = auth, transport = transport)
    infos <- list_models(router_lm(router, paste0(connection$provider, ":catalog")))
    source <- "provider"; fetched <- .iso_seconds(as.numeric(Sys.time()))
  } else if (!is.null(registry)) infos <- Filter(function(i) i$provider == connection$provider, registry$list())
  out <- list()
  for (info in infos) {
    caps <- if (!is.null(capability)) setNames(list(.capability_state(info, capability)), capability) else list()
    if (!is.null(capability) && (caps[[1L]] == "unsupported" || (caps[[1L]] == "unknown" && !isTRUE(include_unknown)))) next
    out[[length(out) + 1L]] <- structure(list(provider = connection$provider, model = info$id, connection_id = connection$id, source = source, fetched_at = fetched, capabilities = caps), class = "lm15_model_choice")
  }
  out
}
print.lm15_model_choice <- function(x, ...) { cat("<lm15 model choice ", x$provider, ":", x$model, " (", x$source, ")>\n", sep = ""); invisible(x) }

# The same scope, with every request-time resolution checked against one
# (connection id, generation). Shares the store; owns nothing.
.pinned_auth <- function(auth, pin) {
  out <- unclass(auth)
  out$request_auth <- function(provider, pinned = NULL) auth$request_auth(provider, pin)
  out$credential_provider <- function(provider, pinned = NULL) auth$credential_provider(provider, pin)
  structure(out, class = "lm15_auth")
}

# One connection, one model (AUTH-23). It follows that connection's
# renewals and nothing else: a replacement or logout makes it fail
# connection_changed / login_required instead of quietly switching who pays.
# It keeps no conversation, runs no tool loop and retries nothing.
bind_model <- function(provider, model, ..., auth = local_auth(), transport = NULL, adaptations = "note") {
  .check_dots(...); auth <- .auth_arg(auth)
  .string(model, "model")
  connection <- auth$status(provider)$connection
  if (is.null(connection)) .abort_auth(paste0(provider, ": no saved connection to bind; sign in first."), reason = "login_required", stage = "resolution", recovery = "restart_login", provider = provider)
  selection <- list(provider = connection$provider, model = model, connection_id = connection$id, identity_generation = connection$identity_generation, instance_id = connection$instance_id)
  router <- new_router(auth = .pinned_auth(auth, c(connection$id, connection$identity_generation)), transport = transport, adaptations = adaptations)
  routed <- paste0(selection$provider, ":", model)
  build <- function(messages, ..., system = NULL, tools = list(), config = NULL) {
    .check_dots(...)
    if (is.character(messages)) messages <- list(message_user(messages))
    if (inherits(messages, "lm15_Message")) messages <- list(messages)
    request(routed, messages, system = system, tools = tools, config = config %||% .new_value("Config", list()))
  }
  coerce <- function(x, ...) {
    if (inherits(x, "lm15_Request")) {
      if (length(list(...))) .abort_auth("Pass either a Request or messages with fields, not both.", reason = "selection_mismatch", stage = "dispatch", provider = selection$provider)
      if (!x$model %in% c(routed, model)) .abort_auth(paste0("This client is bound to ", routed, "; the Request names ", x$model, "."), reason = "selection_mismatch", stage = "dispatch", provider = selection$provider)
      if (x$model != routed) x$model <- routed
      return(x)
    }
    build(x, ...)
  }
  structure(list(provider = selection$provider, model = model, selection = selection, auth = auth, router = router, request = build, coerce = coerce), class = "lm15_bound")
}
print.lm15_bound <- function(x, ...) { cat("<lm15 bound client ", x$provider, ":", x$model, " via connection ", x$selection$connection_id, ">\n", sep = ""); invisible(x) }
str.lm15_bound <- function(object, ...) { print.lm15_bound(object); invisible(NULL) }
complete.lm15_bound <- function(lm, request, ...) complete(lm$router, lm$coerce(request, ...))
stream.lm15_bound <- function(lm, request, on_event, ...) stream(lm$router, lm$coerce(request, ...), on_event)

.ask <- function(ui, prompt) tryCatch(ui$prompt(prompt),
  LoginCancelled = function(e) .abort_auth("Cancelled.", reason = "interaction_required", stage = "interaction", recovery = "restart_login"),
  interrupt = function(e) .abort_auth("Cancelled.", reason = "interaction_required", stage = "interaction", recovery = "restart_login"))

# Choose (or make) a connection and a model, and return a bound client.
# `provider` and `model` skip their pickers. Saved subscriptions are offered
# first; an ambient environment key is offered only as an explicit choice,
# never taken silently. A completed login is saved before the model picker:
# cancel the picker and the login stays.
connect <- function(provider = NULL, model = NULL, ..., auth = local_auth(), ui = NULL, capability = NULL, open_browser = FALSE, allow_unverified = FALSE, transport = NULL) {
  .check_dots(...); auth <- .auth_arg(auth)
  if (is.null(ui)) {
    if (!.interactive_terminal()) .abort_auth("connect() needs a person: this R session is not interactive and no ui = was supplied. On a server, use new_router(auth = ...) with saved connections instead.",
      reason = "interaction_required", stage = "interaction", recovery = "provide_input")
    ui <- terminal_ui(open_browser = open_browser)
  }
  ui$notify(.notice("info", message = paste0("Connections are saved privately in ", auth$store$description, ".")))
  connection <- .choose_connection(auth, ui, provider, allow_unverified)
  if (is.null(model)) model <- .choose_model(auth, ui, connection, capability, transport)
  ui$notify(.notice("info", message = paste0("Ready: ", connection$provider, ":", model, " through ", connection$label, ".")))
  bind_model(connection$provider, model, auth = auth, transport = transport)
}

.choose_connection <- function(auth, ui, provider, allow_unverified) {
  target <- if (!is.null(provider)) auth$descriptor(provider)$id
  saved <- Filter(function(c) is.null(target) || c$provider == target, auth$connections())
  saved <- saved[order(vapply(saved, function(c) c$kind != "account", logical(1)), vapply(saved, function(c) c$provider, ""))]  # subscriptions first
  usable <- Filter(function(c) auth$status(c$provider)$usability %in% c("ready", "renewal_due", "unknown"), saved)
  if (!is.null(target) && length(usable) == 1L) return(usable[[1L]])
  if (!length(usable)) return(.new_connection(auth, ui, target, allow_unverified))
  options <- c(lapply(usable, function(c) .option(c$id, c$label, paste(c$provider, "- saved"))), list(.option("__new__", "Connect another account or API key")))
  answer <- .ask(ui, .prompt("select", "connection", "Use a saved connection, or connect another?", options = options))
  if (answer == "__new__") return(.new_connection(auth, ui, target, allow_unverified))
  for (c in usable) if (c$id == answer) return(c)
  .abort_auth("The UI answered with an unknown connection.", reason = "invalid_login_state", stage = "interaction", recovery = "select_connection")
}

.new_connection <- function(auth, ui, provider, allow_unverified) {
  if (is.null(provider)) {
    ds <- Filter(function(d) any(vapply(d$methods, function(m) m$availability != "unavailable", logical(1))), auth$providers())
    subscription <- vapply(ds, function(d) any(vapply(d$methods, function(m) isTRUE(m$subscription) && m$availability == "supported", logical(1))), logical(1))
    ds <- ds[order(!subscription, tolower(vapply(ds, function(d) d$label, "")))]
    provider <- .ask(ui, .prompt("select", "provider", "Which provider?", options = lapply(ds, function(d) .option(d$id, d$label, if (d$service != d$label) d$service))))
  }
  d <- auth$descriptor(provider)
  existing <- auth$status(d$id)$connection
  method <- .choose_login_method(ui, d, allow_unverified)
  replace <- NULL
  if (!is.null(existing)) {
    keep <- .ask(ui, .prompt("select", "replace", paste0(d$label, " already has a saved connection (", existing$label, ")."), options = list(.option("keep", "Keep it"), .option("replace", "Replace it"))))
    if (keep == "keep") return(existing)
    replace <- existing$id
  }
  if (method$flow %in% c("form", "source_recipe")) {
    answers <- list()
    for (f in method$fields) answers[[f$id]] <- if (f$type == "select" && length(f$options) == 1L) f$options[[1L]]$id
      else if (f$type == "select") .ask(ui, .prompt("select", f$id, f$label, options = f$options))
      else .ask(ui, .prompt(f$type, f$id, f$label))
    return(auth$configure(d$id, method$id, answers = answers, replace = replace))
  }
  tryCatch(auth$login(d$id, method$id, ui = ui, replace = replace, allow_unverified = allow_unverified),
    LoginCancelled = function(e) .abort_auth("Sign-in cancelled.", reason = "interaction_required", stage = "interaction", recovery = "restart_login", provider = d$id))
}

.choose_login_method <- function(ui, d, allow_unverified) {
  methods <- Filter(function(m) m$availability == "supported" || (isTRUE(allow_unverified) && m$availability == "unverified"), d$methods)
  if (!length(methods)) .abort_auth(paste0(d$id, ": no login method is available here."), reason = "method_unavailable", stage = "discovery", recovery = "choose_method", provider = d$id)
  methods <- methods[order(!vapply(methods, function(m) isTRUE(m$subscription), logical(1)), vapply(methods, function(m) m$kind != "account", logical(1)))]
  options <- list()
  for (m in methods) {
    note <- m$billing_note
    if (m$availability == "unverified") note <- paste("UNVERIFIED --", m$reason)
    if (m$id == "env") {
      set <- Filter(function(o) nzchar(Sys.getenv(o$id)), m$fields[[1L]]$options)
      if (!length(set)) next  # nothing to offer
      note <- paste0("$", set[[1L]]$id, " is set in this environment; using it is your explicit choice")
    }
    options[[length(options) + 1L]] <- .option(m$id, m$label, note)
  }
  chosen <- if (length(options) == 1L) options[[1L]]$id else .ask(ui, .prompt("select", "method", paste0("How do you want to connect to ", d$label, "?"), options = options))
  Filter(function(m) m$id == chosen, d$methods)[[1L]]
}

.choose_model <- function(auth, ui, connection, capability, transport) {
  choices <- tryCatch(model_choices(connection$provider, auth = auth, refresh = TRUE, capability = capability, transport = transport), error = function(e) {
    if (inherits(e, "AuthOperationError")) stop(e)
    ui$notify(.notice("info", message = paste0("Could not list models for ", connection$provider, " (", class(e)[[1L]], "); type a model id.")))
    list()
  })
  if (!is.null(capability) && !length(choices)) ui$notify(.notice("info", message = paste0("No listed model is known to support '", capability, "'; you can still type one.")))
  options <- c(lapply(choices, function(c) .option(c$model, c$model, "listed by your account just now")), list(.option("__manual__", "Type a model id (not verified against your account)")))
  answer <- .ask(ui, .prompt("select", "model", paste0("Which ", connection$provider, " model?"), options = options))
  if (answer == "__manual__") {
    answer <- trimws(.ask(ui, .prompt("text", "model", "Model id")))
    if (!nzchar(answer)) .abort_auth("No model id given.", reason = "interaction_required", stage = "interaction", recovery = "provide_input", provider = connection$provider)
  }
  answer
}
