# The vet shim's managed_run op (lm15-contract harness/PROTOCOL.md, managed):
# one scripted program against the public managed-auth API with every seam
# injected -- the store file the harness created, a fake wall and monotonic
# clock (waits advance them), a scripted auth server and a scripted UI. It
# returns one outcome per step, the ordered trace and the store file
# afterwards. The harness compares; this only reports.

.vet_managed_run <- function(msg) {
  events <- list()
  record <- function(event) events[[length(events) + 1L]] <<- event
  start_s <- .num(msg$clock_ms) / 1000; elapsed <- 0
  wall <- function() start_s + elapsed
  mono <- function() elapsed
  sleep <- function(seconds) { elapsed <<- elapsed + seconds; record(json_object(sleep_ms = round(seconds * 1000))) }
  script <- msg$http %||% list()
  server <- function(wire) {
    ctype <- wire$headers[["content-type"]]
    ctype <- if (is.null(ctype)) NULL else trimws(sub(";.*$", "", ctype))
    headers <- json_object()
    for (name in names(wire$headers)) {
      key <- tolower(name)
      if (key %in% c("accept", "accept-encoding", "connection", "content-length", "content-type", "host")) next
      value <- wire$headers[[name]]
      if (key == "user-agent" && startsWith(value, "lm15/")) value <- "lm15"
      headers[[key]] <- value
    }
    body <- NULL
    if (length(wire$body)) {
      text <- rawToChar(wire$body)
      body <- if (identical(ctype, "application/x-www-form-urlencoded")) .json_object(setNames(lapply(.parse_query(text), function(p) p[[2L]]), vapply(.parse_query(text), function(p) p[[1L]], "")))
        else tryCatch(.json_decode(text), error = function(e) text)
    }
    http <- json_object(method = wire$method, url = wire$url, content_type = ctype, headers = headers)
    http["body"] <- list(body)
    record(json_object(http = http))
    refused <- function() { e <- lm15_error("refused", code = "transport"); e$exchange_uncertain <- FALSE; stop(e) }
    if (!length(script)) refused()
    reply <- script[[1L]]; script <<- script[-1L]
    if (!is.null(reply$delay_ms)) Sys.sleep(.num(reply$delay_ms) / 1000)  # real time: lets another process race this exchange
    if (identical(reply$network, "timeout")) { e <- lm15_error("timed out", code = "transport"); e$exchange_uncertain <- TRUE; stop(e) }
    if (identical(reply$network, "refused")) refused()
    status <- .json_int(reply$status) %||% 200L
    if ("json" %in% names(reply)) return(list(status = status, headers = list("content-type" = "application/json"), body = charToRaw(enc2utf8(.json_encode(reply$json)))))
    list(status = status, headers = list("content-type" = reply$content_type %||% "text/plain"), body = charToRaw(enc2utf8(reply$text %||% "")))
  }
  answers <- msg$ui %||% list(); last_url <- NULL
  query_of <- function(url) { p <- .url_parts(url %||% ""); q <- if (is.null(p)) list() else .parse_query(p$query); setNames(lapply(q, function(x) x[[2L]]), vapply(q, function(x) x[[1L]], "")) }
  ui <- list(
    notify = function(notice) {
      if (notice$type == "auth_url") last_url <<- notice$url
      event <- switch(notice$type,
        auth_url = json_object(type = "auth_url", url = notice$url),
        device_code = json_object(type = "device_code", user_code = notice$user_code, verification_url = notice$verification_url, expires_in_s = notice$expires_in_s, interval_s = notice$interval_s),
        progress = json_object(type = "progress", stage = notice$stage),
        json_object(type = notice$type))
      record(json_object(notice = event))
    },
    prompt = function(prompt) {
      event <- json_object(type = prompt$type, field_id = prompt$field_id)
      if (prompt$type == "select") event$options <- lapply(prompt$options, function(o) o$id)
      record(json_object(prompt = event))
      if (!length(answers)) stop(.login_cancelled("the script has no more answers"))
      answer <- answers[[1L]]; answers <<- answers[-1L]
      if (is.character(answer)) return(answer)
      if (isTRUE(answer$cancel)) stop(.login_cancelled("the script cancels here"))
      q <- query_of(last_url); state <- q$state %||% ""
      if (!is.null(answer$paste)) return(paste0(answer$paste, "#", state))
      if (!is.null(answer$paste_wrong_state)) return(paste0(answer$paste_wrong_state, "#not-the-state-of-this-attempt"))
      if (!is.null(answer$paste_url)) return(paste0(q$redirect_uri %||% "", "?", .form_encode(list(code = answer$paste_url, state = state))))
      stop("unknown scripted answer", call. = FALSE)
    })

  env <- unlist(msg$env %||% list())
  saved <- Sys.getenv()
  do.call(Sys.unsetenv, list(names(saved)))
  if (length(env)) do.call(Sys.setenv, as.list(env))
  on.exit({ do.call(Sys.unsetenv, list(names(Sys.getenv()))); do.call(Sys.setenv, as.list(saved)) }, add = TRUE)

  auth <- new_auth(file_store(msg$store_path), clock = wall, monotonic = mono, http = server, sleep = sleep)
  steps <- list()
  resolve_refs <- function(step) {
    value <- function(n) {
      outcome <- steps[[n + 1L]]
      if (!isTRUE(outcome$ok) || !.is_object(outcome$value)) stop("the referenced step returned no connection", call. = FALSE)
      outcome$value
    }
    for (key in names(step)) {
      item <- step[[key]]
      if (.is_object(item) && !is.null(item$id_of_step)) step[[key]] <- value(.json_int(item$id_of_step))$id
      else if (.is_object(item) && !is.null(item$of_step)) { c <- value(.json_int(item$of_step)); step[[key]] <- c(c$id, c$identity_generation) }
    }
    step
  }
  for (i in seq_along(msg$steps)) {
    record(json_object(step = i - 1L))
    outcome <- tryCatch(json_object(ok = TRUE, value = .vet_managed_step(auth, resolve_refs(msg$steps[[i]]), ui, env, msg$sentinel, function(ms) elapsed <<- elapsed + ms / 1000)),
      LoginCancelled = function(e) json_object(ok = FALSE, error = json_object(type = "cancelled")),
      error = function(e) {
        err <- if (inherits(e, "AuthOperationError")) json_object(type = "AuthOperationError", code = e$code, reason = e$reason, stage = e$stage, commit_state = e$commit_state, recovery = e$recovery)
          else if (inherits(e, "LM15Error")) json_object(type = class(e)[[1L]], code = e$code)
          else json_object(type = "ValueError")
        json_object(ok = FALSE, error = err)
      })
    if (isTRUE(outcome$ok)) outcome["value"] <- list(outcome$value)
    steps[[i]] <- outcome
  }
  store <- NULL
  if (file.exists(msg$store_path)) {
    text <- rawToChar(readBin(msg$store_path, "raw", n = file.info(msg$store_path)$size))
    document <- tryCatch(.json_decode(text), error = function(e) tryCatch(.lenient_json(text), error = function(e) NULL))
    store <- if (is.null(document)) json_object(raw = text) else json_object(document = document)
  }
  out <- json_object(steps = steps, events = events)
  out["store"] <- list(store)
  out
}

.vet_connection <- function(c) {
  if (is.null(c)) return(NULL)
  out <- json_object(id = c$id, provider = c$provider, instance_id = c$instance_id, kind = c$kind, method_id = c$method_id, routes = .json_array(as.list(c$routes)),
    label = c$label, created_at = c$created_at, identity_generation = c$identity_generation, credential_revision = c$credential_revision, settings = .json_object(c$settings))
  if (!is.null(c$account_label)) out$account_label <- c$account_label
  out
}
.vet_managed_step <- function(auth, step, ui, env, sentinel, advance) {
  opt <- function(x) if (is.null(x)) list() else x
  switch(step$do,
    advance = { advance(.num(step$ms)); NULL },
    login = .vet_connection(auth$login(step$provider, step$method, ui = ui, settings = opt(step$settings), answers = opt(step$answers), replace = step$replace, allow_unverified = isTRUE(step$allow_unverified))),
    configure = .vet_connection(auth$configure(step$provider, step$method, answers = opt(step$answers), settings = opt(step$settings), replace = step$replace)),
    set_api_key = .vet_connection(auth$set_api_key(step$provider, step$key, replace = step$replace)),
    status = {
      s <- auth$status(step$provider)
      out <- json_object(provider = s$provider, presence = s$presence, usability = s$usability)
      out["connection"] <- list(.vet_connection(s$connection)); out["expires_at"] <- list(s$expires_at); out$logged_out <- isTRUE(s$logged_out)
      out["verification"] <- list(if (!is.null(s$verification)) json_object(result = s$verification$result, check = s$verification$check))
      out
    },
    connections = lapply(auth$connections(), .vet_connection),
    logout = { r <- auth$logout(step$target); json_object(provider = r$provider, forgot = r$forgot, routes = .json_array(as.list(r$routes)), identity_generation = r$identity_generation) },
    cancel_login = auth$cancel_login(step$provider),
    request_auth = {
      r <- auth$request_auth(step$provider, pinned = if (!is.null(step$pinned)) as.character(unlist(step$pinned)))
      credential <- if (is.null(r$credential)) NULL else if (r$credential$kind == "bearer_token") json_object(kind = "bearer", value = r$credential$value)
        else if (r$credential$kind == "api_key") json_object(kind = "api_key", value = r$credential$value) else json_object(kind = r$credential$kind)
      out <- json_object(); out["credential"] <- list(credential); out$headers <- .json_object(as.list(r$headers)); out["base_url"] <- list(r$base_url); out["account_id"] <- list(r$account_id); out["named"] <- list(r$named)
      out
    },
    methods = lapply(auth$methods(step$provider), function(m) json_object(id = m$id, kind = m$kind, flow = m$flow, availability = m$availability, subscription = isTRUE(m$subscription),
      delivery = .json_array(as.list(m$delivery)), fields = .json_array(lapply(m$fields, function(f) json_object(id = f$id, type = f$type, required = isTRUE(f$required), options = .json_array(lapply(f$options, function(o) o$id))))))),
    providers = as.list(.radix_sort(vapply(auth$providers(), function(d) d$id, ""))),
    explain = {
      keys <- setNames(lapply(opt(step$api_keys), function(p) api_key(paste0(sentinel, "-explicit"))), unlist(opt(step$api_keys)))
      report <- explain_auth(step$provider, env = if (length(env)) env else character(), api_keys = keys, auth = auth)
      json_object(configured = report$configured, steps = lapply(report$steps, function(s) json_object(kind = s$kind, state = s$state)))
    },
    stop("unknown managed step", call. = FALSE))
}

# The store file as a reader that lets a duplicate member's last value win
# (how the reference reports a file the store itself refuses to read).
.lenient_json <- function(text) {
  convert <- function(x) {
    if (is.list(x) && !is.null(names(x))) {
      keys <- unique(names(x))
      return(.json_object(setNames(lapply(keys, function(k) convert(x[[max(which(names(x) == k))]])), keys)))
    }
    if (is.list(x)) return(.json_array(lapply(x, convert)))
    if (is.numeric(x)) return(.json_number(format(x, scientific = FALSE, digits = 17)))
    x
  }
  convert(jsonlite::fromJSON(text, simplifyVector = FALSE))
}
