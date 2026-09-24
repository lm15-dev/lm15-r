.vet_lm <- function(msg) {
  credential <- if (.is_object(msg$credential)) from_dict(msg$credential, "credential") else api_key(msg$api_key %||% "vet-parse-only")
  fixed <- if (!is.null(msg$now)) .parse_rfc3339(msg$now) else NULL
  new_lm(msg$provider, api_key = credential, base_url = msg$base_url, settings = msg$settings %||% list(), env = character(),
    account_id = if (canonical_provider(msg$provider) == "openai-codex") "test-account" else NULL,
    clock = if (is.null(fixed)) Sys.time else function() fixed,
    transport = function(...) .abort("Vet shim cannot access the network.", "transport"))
}
.vet_adaptations <- function(records) lapply(records, function(a) {
  out <- json_object(field = a$field, action = a$action)
  if (!is.null(a$asked)) out["asked"] <- list(a$asked)
  if (!is.null(a$applied)) out["applied"] <- list(a$applied)
  out
})
.vet_wire <- function(wire) {
  url <- wire$url; params <- json_object()
  split <- regexpr("?", url, fixed = TRUE)[[1L]]
  if (split > 0L) {
    query <- substring(url, split + 1L); url <- substr(url, 1L, split - 1L)
    for (item in strsplit(query, "&", fixed = TRUE)[[1L]]) {
      at <- regexpr("=", item, fixed = TRUE)[[1L]]
      name <- if (at < 0L) item else substr(item, 1L, at - 1L)
      value <- if (at < 0L) "" else substring(item, at + 1L)
      decode <- function(x) utils::URLdecode(gsub("+", " ", x, fixed = TRUE))
      params[[decode(name)]] <- decode(value)
    }
  }
  out <- json_object(method = wire$method, url = url, params = params, headers = .json_object(wire$headers), body = NULL)
  if (length(wire$body)) {
    if (grepl("json", wire$headers[["content-type"]] %||% "", fixed = TRUE)) out$body <- .json_decode(rawToChar(wire$body))
    else out$body_b64 <- .base64_encode(wire$body)
  }
  if (length(wire$adaptations)) out$adaptations <- .vet_adaptations(wire$adaptations)
  out
}
.vet_response <- function(response) {
  out <- json_object(canonical_response = as_dict(response))
  if (!is.null(response$provider_data$`_lm15_unmapped`)) out$unmapped <- response$provider_data$`_lm15_unmapped`
  out
}
.vet_ops <- c("capabilities", "build_request", "parse_response", "replay_stream", "normalize_error", "serde_roundtrip", "validate", "surface_dump", "explain_auth", "resolve_model", "build_models_request", "parse_models_response", "file_op_build", "file_op_parse", "cache_op_build", "cache_op_parse", "batch_op_build", "batch_op_parse", "video_op_build", "video_op_parse", "generation_build", "generation_parse", "ingest_openai_chat", "sigv4_sign", "replay_live", "token_exchange_build", "token_exchange_parse")

vet_handle <- function(line, ...) {
  .check_dots(...); id <- NULL; trace <- list()
  reply <- tryCatch({
    msg <- .json_decode(line)
    if (!.is_object(msg)) stop("Vet input must be a JSON object.", call. = FALSE)
    id <- msg$id; op <- msg$op
    if (is.null(op) || !op %in% .vet_ops) stop("Unknown or unimplemented vet operation.", call. = FALSE)
    result <- switch(op,
      capabilities = json_object(language = "r", ops = as.list(.vet_ops), impl_version = "0.0.1.9000"),
      serde_roundtrip = json_object(value = as_dict(from_dict(msg$value, msg$kind))),
      validate = json_object(ok = TRUE, normalized = as_dict(from_dict(msg$value, msg$kind))),
      surface_dump = {
        dump <- surface_dump()
        json_object(types = .json_object(lapply(dump$types, function(t) json_object(fields = as.list(t$fields %||% character())))), enums = .json_object(lapply(dump$enums, as.list)))
      },
      resolve_model = {
        catalog <- lapply(msg$catalog %||% list(), from_dict, kind = "model_info")
        .json_object(resolve(new_router(catalog = catalog, env = character()), msg$model))
      },
      explain_auth = {
        env <- unlist(msg$env %||% list()); if (!length(env)) env <- character()
        keys <- setNames(lapply(msg$api_keys_providers %||% list(), function(p) api_key(msg$sentinel)), unlist(msg$api_keys_providers %||% list()))
        report <- explain_auth(msg$provider, api_keys = keys, env = env, path = msg$credentials_path, settings = msg$settings %||% list())
        json_object(configured = report$configured, steps = lapply(report$steps, function(s) json_object(kind = s$kind, state = s$state)), report_text = paste(utils::capture.output(print(report)), collapse = "\n"))
      },
      token_exchange_build = .json_object(unclass(token_exchange_build(msg$provider, msg$rung, msg$input, now = .parse_rfc3339(msg$now), settings = msg$settings %||% list()))),
      token_exchange_parse = tryCatch(
        json_object(ok = TRUE, credential = as_dict(token_exchange_parse(msg$provider, msg$rung, .wire_int(msg$status), msg$body, now = .parse_rfc3339(msg$now)))),
        LM15Error = function(e) json_object(ok = FALSE, error = json_object(class = class(e)[[1L]], code = e$code))),
      sigv4_sign = {
        r <- msg$request
        sign <- sigv4_sign(r$method, r$url, from_dict(msg$credential, "credential"), region = msg$region, service = msg$service, headers = r$headers, body = r$body %||% "", now = .parse_rfc3339(msg$now))
        json_object(canonical_request = sign$canonical_request, string_to_sign = sign$string_to_sign, authorization = sign$authorization, headers = .json_object(sign$headers))
      },
      {
        lm <- .vet_lm(msg)
        body <- if (!is.null(msg$body_b64)) jsonlite::base64_dec(msg$body_b64) else NULL
        status <- .wire_int(msg$status %||% 200L)
        req <- if (!is.null(msg$canonical_request)) from_dict(msg$canonical_request, "request") else NULL
        if (status >= 300L && op != "normalize_error" && !is.null(body)) stop(normalize_error(lm, status, rawToChar(body)))
        switch(op,
          replay_live = {
            cfg <- from_dict(msg$live_config, "live_config")
            json_object(setup_frames = live_setup_frames(lm, cfg),
              client_frames = lapply(msg$client_events, function(e) live_encode(lm, cfg, from_dict(e, "live_client_event"))),
              events = lapply(msg$server_frames_b64, function(frame) lapply(live_decode(lm, jsonlite::base64_dec(frame)), as_dict)))
          },
          build_request = .vet_wire(build_request(lm, req, stream = isTRUE(msg$stream))),
          parse_response = .vet_response(parse_response(lm, req, body, status = status)),
          replay_stream = {
            source <- .new_stream(lm, req, function(e) trace[[length(trace) + 1L]] <<- as_dict(e))
            source$feed(body); response <- source$finish()
            out <- .vet_response(response); out$events <- trace; out
          },
          normalize_error = {
            e <- normalize_error(lm, status, msg$body_text)
            json_object(class = class(e)[[1L]], code = e$code, provider_code = e$provider_code, message = e$message)
          },
          build_models_request = .vet_wire(build_models_request(lm)),
          parse_models_response = json_object(models = lapply(parse_models_response(lm, body), as_dict)),
          ingest_openai_chat = json_object(canonical_request = as_dict(request_from_openai_chat(msg$body, provider = lm$definition$id))),
          file_op_build = .vet_wire(file_op_build(lm, msg$file_op, upload_request = if (!is.null(msg$upload_request)) from_dict(msg$upload_request, "file_upload_request") else NULL, file_id = msg$file_id, limit = .wire_int(msg$limit %||% 20L), cursor = msg$cursor)),
          file_op_parse = {
            f <- file_op_parse(lm, body, page = msg$kind == "page")
            .json_object(setNames(list(as_dict(f)), if (msg$kind == "page") "page" else "file"))
          },
          cache_op_build = .vet_wire(cache_op_build(lm, msg$cache_op, prefix = if (!is.null(msg$prefix_request)) from_dict(msg$prefix_request, "request") else NULL, id = msg$cache_id, limit = .wire_int(msg$limit %||% 20L), cursor = msg$cursor, ttl_seconds = .wire_int(msg$ttl_seconds), label = msg$label)),
          cache_op_parse = {
            data <- .decode_body(body)
            if (msg$kind == "page") json_object(page = as_dict(cache_page(items = lapply(data$cachedContents %||% list(), .parse_cache), next_cursor = data$nextPageToken)))
            else json_object(cache = as_dict(.parse_cache(data)))
          },
          batch_op_build = json_object(requests = lapply(batch_op_build(lm, msg$action, request = if (!is.null(msg$batch_request)) from_dict(msg$batch_request, "batch_request") else NULL, id = msg$batch_id, limit = .wire_int(msg$limit %||% 20L), upload_body = msg$upload_body, status_body = msg$status_body), .vet_wire)),
          batch_op_parse = {
            out <- batch_op_parse(lm, msg$kind, body, status_body = msg$status_body, fetched = lapply(msg$fetched_b64 %||% list(), jsonlite::base64_dec))
            if (msg$kind == "job") json_object(job = as_dict(out)) else .json_object(setNames(list(lapply(out, as_dict)), if (msg$kind == "list") "jobs" else "entries"))
          },
          video_op_build = json_object(requests = lapply(video_op_build(lm, msg$action, request = if (!is.null(msg$video_request)) from_dict(msg$video_request, "video_generation_request") else NULL, id = msg$video_id, status_body = msg$status_body, limit = .wire_int(msg$limit %||% 20L), model = msg$model), .vet_wire)),
          video_op_parse = {
            fetched <- if (!is.null(msg$fetched_b64)) list(body = jsonlite::base64_dec(msg$fetched_b64), headers = msg$headers) else NULL
            out <- video_op_parse(lm, msg$kind, body, id = msg$video_id, status_body = msg$status_body, fetched = fetched)
            if (msg$kind == "list") json_object(jobs = lapply(out, as_dict)) else .json_object(setNames(list(as_dict(out)), if (msg$kind == "job") "job" else "part"))
          },
          generation_build = .vet_wire(generation_build(lm, from_dict(msg$generation_request, paste0(if (msg$kind == "image") "image" else "speech", "_generation_request")))),
          generation_parse = as_dict(generation_parse(lm, from_dict(msg$generation_request, paste0(if (msg$kind == "image") "image" else "speech", "_generation_request")), body, headers = msg$headers %||% list()))
        )
      }
    )
    json_object(id = id, ok = TRUE, result = result)
  }, error = function(e) {
    error <- json_object(type = class(e)[[1L]], message = conditionMessage(e))
    if (inherits(e, "LM15Error")) error$code <- e$code
    if (inherits(e, "LM15Error") && !is.null(e$feature)) error$feature <- e$feature
    if (inherits(e, "StreamAssemblyError")) {
      if (!is.null(e$partial)) error$partial_response <- as_dict(e$partial)
      error$events <- trace
    }
    if (inherits(e, "UnknownModelError") || inherits(e, "AmbiguousModelError")) error$model <- e$model
    if (inherits(e, "AmbiguousModelError")) error$providers <- as.list(e$providers)
    json_object(id = id, ok = FALSE, error = error)
  })
  .json_encode(reply)
}
