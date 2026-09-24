.batch_status <- function(data, dialect) {
  if (dialect == "anthropic") {
    status <- .wire_string(data$processing_status)
    if (status == "in_progress") return("running")
    if (status == "canceling") return("cancelling")
    if (status == "ended") {
      n <- function(key) .wire_int(data$request_counts[[key]] %||% 0L)
      if (n("canceled") > 0 && n("succeeded") + n("errored") + n("expired") == 0) return("cancelled")
      if (n("expired") > 0 && n("succeeded") + n("errored") + n("canceled") == 0) return("expired")
      return("completed")
    }
    return("queued")
  }
  if (dialect == "gemini") return(switch(.wire_string(data$metadata$state), BATCH_STATE_PENDING = "queued", BATCH_STATE_RUNNING = "running", BATCH_STATE_CANCELLING = "cancelling", BATCH_STATE_SUCCEEDED = "completed", BATCH_STATE_FAILED = "failed", BATCH_STATE_CANCELLED = "cancelled", BATCH_STATE_EXPIRED = "expired", if (isTRUE(data$done)) "completed" else "queued"))
  status <- .wire_string(data$status)
  if (status %in% c("completed", "failed", "cancelled", "expired")) return(status)
  if (status %in% c("cancelling", "canceling")) return("cancelling")
  if (status %in% c("in_progress", "finalizing")) "running" else "queued"
}
.batch_info <- function(lm, data) {
  d <- lm$definition$dialect
  batch_job_info(if (d == "gemini") data$name else data$id, .batch_status(data, d), label = if (d == "gemini") data$metadata$displayName else if (d != "anthropic") data$metadata$label else NULL, created_at = .iso_utc(if (d == "gemini") data$metadata$createTime else data$created_at), provider_data = data)
}
.batch_path <- function(lm, id = NULL) {
  d <- lm$definition$dialect
  if (is.null(id)) return(if (d == "anthropic") "/messages/batches" else "/batches")
  .string(id, "batch id")
  if (d == "gemini") paste0("/", .path_id(id, TRUE)) else paste0(if (d == "anthropic") "/messages/batches/" else "/batches/", .path_id(id))
}
.provider_fetch <- function(lm, url) {
  .string(url, "provider result URL")
  origin <- function(x) sub("^(https?://[^/]+).*", "\\1", x)
  # Result URLs are provider-controlled. A compromised response must not
  # send credentials to another origin, or to a URL containing userinfo.
  if (!grepl("^https?://", url) || grepl("@", origin(url), fixed = TRUE) || origin(url) != origin(lm$base_url))
    .abort("Result URL uses a different origin; credentials will not be forwarded.", "provider", lm$definition$id)
  fetch <- lm; fetch$base_url <- origin(url)
  .emit(fetch, "GET", substring(url, nchar(fetch$base_url) + 1L))
}

batch_op_build <- function(lm, action, ..., request = NULL, id = NULL, limit = 20L, upload_body = NULL, status_body = NULL) {
  .check_dots(...); .require_surface(lm, "batches")
  d <- lm$definition$dialect
  if (!action %in% c("upload", "submit", "status", "cancel", "list", "result_fetches")) stop("Unknown batch action.", call. = FALSE)
  if (action %in% c("upload", "submit")) {
    r <- validate(request)
    if (!inherits(r, "lm15_BatchRequest")) stop("Expected a BatchRequest.", call. = FALSE)
    # Build every member before uploading anything; an invalid later member
    # must not leave a billed upload behind.
    bodies <- lapply(r$requests, function(req) .build_payload(lm, req, FALSE))
    if (action == "upload") {
      if (d != "openai-responses") return(list())
      lines <- vapply(seq_along(bodies), function(i) .json_encode(json_object(custom_id = as.character(i - 1L), method = "POST", url = "/v1/responses", body = bodies[[i]])), "")
      form <- .multipart(list(purpose = "batch"), list(list(name = "file", filename = "lm15-batch.jsonl", media_type = "application/jsonl", data = charToRaw(paste0(paste(lines, collapse = "\n"), "\n")))))
      return(list(.emit(lm, "POST", "/files", body = form$body, headers = list("content-type" = form$content_type))))
    }
    ext <- r$extensions %||% json_object()
    if (d == "openai-responses") {
      if (is.null(upload_body$id)) .abort("Batch upload returned no file id.", provider = lm$definition$id)
      payload <- json_object(input_file_id = upload_body$id, endpoint = ext$endpoint %||% "/v1/responses", completion_window = ext$completion_window %||% "24h")
      if (!is.null(r$label)) payload$metadata <- json_object(label = r$label)
    } else if (d == "anthropic") {
      if (!is.null(r$label)) .unsupported(lm$definition$id, "batch label")
      payload <- json_object(requests = lapply(seq_along(bodies), function(i) json_object(custom_id = as.character(i - 1L), params = bodies[[i]])))
    } else {
      batch <- json_object(inputConfig = json_object(requests = json_object(requests = lapply(seq_along(bodies), function(i) json_object(request = bodies[[i]], metadata = json_object(key = as.character(i - 1L)))))))
      if (!is.null(r$label)) batch$displayName <- r$label
      payload <- json_object(batch = batch)
    }
    payload[names(ext)] <- ext
    path <- if (d == "gemini") paste0("/", .path_id(if (startsWith(r$model, "models/")) r$model else paste0("models/", r$model), TRUE), ":batchGenerateContent") else .batch_path(lm)
    return(list(.emit(lm, "POST", path, payload)))
  }
  if (action == "result_fetches") {
    if (d == "gemini") return(list())
    if (d == "anthropic") return(list(.provider_fetch(lm, status_body$results_url)))
    ids <- Filter(Negate(is.null), list(status_body$output_file_id, status_body$error_file_id))
    return(lapply(ids, function(id) .emit(lm, "GET", paste0("/files/", .path_id(id), "/content"))))
  }
  if (action == "list") return(list(.emit(lm, "GET", .batch_path(lm), params = setNames(list(.positive_limit(limit)), if (d == "gemini") "pageSize" else "limit"))))
  path <- .batch_path(lm, id)
  if (action == "cancel") path <- paste0(path, if (d == "gemini") ":cancel" else "/cancel")
  list(.emit(lm, if (action == "status") "GET" else "POST", path, if (action == "cancel" && d == "gemini") json_object() else NULL))
}
.positive_limit <- function(x) {
  x <- .number(x, "limit", TRUE)
  if (x <= 0) stop("limit must be positive.", call. = FALSE)
  x
}
.decode_body <- function(body) if (is.raw(body)) .json_decode(rawToChar(body)) else if (is.character(body)) .json_decode(body) else body

batch_op_parse <- function(lm, kind, body = NULL, ..., status_body = NULL, fetched = list()) {
  .check_dots(...); .require_surface(lm, "batches")
  d <- lm$definition$dialect
  if (kind == "job") return(.batch_info(lm, .decode_body(body)))
  if (kind == "list") {
    data <- .decode_body(body)
    return(lapply(.wire_array(data[[if (d == "gemini") "operations" else "data"]]), function(x) .batch_info(lm, x)))
  }
  if (kind != "entries") stop("Unknown batch parse kind.", call. = FALSE)
  entries <- list()
  add <- function(e) {
    key <- as.character(e$index)
    if (key %in% names(entries)) .abort("Batch results contain a duplicate submission index.", provider = lm$definition$id)
    entries[[key]] <<- e
  }
  parsed_response <- function(obj) {
    model <- obj$model %||% obj$modelVersion %||% "batch"
    parse_response(lm, request(model, list(message_user(""))), obj)
  }
  detail <- function(err) error_detail(err$code, message = err$message, provider_code = err$provider_code)
  index_value <- function(x) {
    if (is.character(x) && !inherits(x, "lm15_json_number")) x <- .json_number(x)
    out <- .number(x, "submission index", TRUE)
    if (out < 0) .abort("Batch index cannot be negative.", provider = lm$definition$id)
    out
  }
  if (d == "gemini") {
    inlined <- status_body$response$inlinedResponses
    if (.is_object(inlined)) inlined <- inlined$inlinedResponses
    for (position in seq_along(inlined %||% list())) {
      item <- inlined[[position]]; if (!.is_object(item)) next
      i <- if (!is.null(item$metadata$key)) index_value(item$metadata$key) else position - 1L
      if (.is_object(item$response)) add(batch_entry(i, "succeeded", response = parsed_response(item$response)))
      else add(batch_entry(i, "errored", error = error_detail("provider", message = .wire_string(item$error$message, "Batch entry errored."), provider_code = if (!is.null(item$error$status %||% item$error$code)) .wire_string(item$error$status %||% item$error$code) else NULL)))
    }
  } else {
    for (document in fetched) {
      lines <- strsplit(if (is.raw(document)) rawToChar(document) else document, "\n", fixed = TRUE)[[1L]]
      for (line in lines) {
        if (!nzchar(trimws(line))) next
        item <- .json_decode(line); i <- index_value(item$custom_id)
        if (d == "anthropic") {
          result <- .wire_object(item$result); outcome <- .wire_string(result$type)
          if (outcome == "succeeded") add(batch_entry(i, "succeeded", response = parsed_response(result$message)))
          else if (outcome %in% c("canceled", "expired")) add(batch_entry(i, if (outcome == "canceled") "cancelled" else "expired"))
          else {
            raw <- .wire_object(result$error); if (is.null(raw$error)) raw <- json_object(error = raw)
            add(batch_entry(i, "errored", error = detail(normalize_error(lm, 400L, raw))))
          }
        } else {
          response <- .wire_object(item$response); status <- .wire_int(response$status_code %||% 400L)
          if (status == 200L && .is_object(response$body)) add(batch_entry(i, "succeeded", response = parsed_response(response$body)))
          else add(batch_entry(i, "errored", error = detail(normalize_error(lm, status, response$body %||% item$error %||% json_object()))))
        }
      }
    }
    if (d == "openai-responses") {
      total <- .wire_int(status_body$request_counts$total %||% 0L)
      if (total == 0 && length(entries)) total <- max(as.numeric(names(entries))) + 1
      if (total > 1000000) .abort("Batch result count exceeds the in-memory materialization limit.", provider = lm$definition$id)
      status <- .batch_status(status_body, d)
      for (i in seq_len(total) - 1L) if (!as.character(i) %in% names(entries)) {
        if (status %in% c("cancelled", "expired")) add(batch_entry(i, status))
        else add(batch_entry(i, "errored", error = error_detail("provider", message = "Entry missing from batch output files.")))
      }
    }
  }
  unname(entries[order(as.numeric(names(entries)))])
}

batch_submit <- function(lm, request, ...) {
  .check_dots(...)
  # Anthropic labels are rejected before any network action.
  if (lm$definition$dialect == "anthropic" && !is.null(request$label)) .unsupported(lm$definition$id, "batch label")
  uploads <- batch_op_build(lm, "upload", request = request)
  upload_body <- NULL
  for (wire in uploads) upload_body <- .body_object(.send_wire(lm, wire))
  wire <- batch_op_build(lm, "submit", request = request, upload_body = upload_body)[[1L]]
  batch_op_parse(lm, "job", .send_wire(lm, wire)$body)
}
batch_status <- function(lm, id, ...) { .check_dots(...); batch_op_parse(lm, "job", .send_wire(lm, batch_op_build(lm, "status", id = id)[[1L]])$body) }
batch_cancel <- function(lm, id, ...) { .check_dots(...); batch_op_parse(lm, "job", .send_wire(lm, batch_op_build(lm, "cancel", id = id)[[1L]])$body) }
batch_list <- function(lm, ..., limit = 20L) { .check_dots(...); batch_op_parse(lm, "list", .send_wire(lm, batch_op_build(lm, "list", limit = limit)[[1L]])$body) }
batch_results <- function(lm, id, ...) {
  .check_dots(...); info <- batch_status(lm, id)
  if (!info$status %in% c("completed", "failed", "cancelled", "expired")) stop("Batch is not finished; poll status or wait explicitly.", call. = FALSE)
  fetched <- lapply(batch_op_build(lm, "result_fetches", status_body = info$provider_data), function(w) .send_wire(lm, w)$body)
  batch_op_parse(lm, "entries", status_body = info$provider_data, fetched = fetched)
}

video_op_build <- function(lm, action, ..., request = NULL, id = NULL, status_body = NULL, limit = 20L, model = NULL) {
  .check_dots(...); .require_surface(lm, "video")
  d <- lm$definition$dialect; xai <- lm$definition$id == "xai"
  if (!action %in% c("submit", "status", "result_fetch", "list")) stop("Unknown video action.", call. = FALSE)
  if (action == "submit") {
    r <- validate(request)
    if (!inherits(r, "lm15_VideoGenerationRequest")) stop("Expected a VideoGenerationRequest.", call. = FALSE)
    if (length(r$images)) .unsupported(lm$definition$id, "video input frames without a verified wire mapping")
    if (xai && !is.null(r$seconds)) .unsupported(lm$definition$id, "video duration")
    ext <- r$extensions %||% json_object()
    payload <- if (d == "gemini") json_object(instances = list(json_object(prompt = r$prompt))) else json_object(model = r$model, prompt = r$prompt)
    payload[names(ext)] <- ext
    if (!is.null(r$seconds)) {
      if (d == "gemini" && is.null(payload$parameters)) payload$parameters <- json_object(durationSeconds = r$seconds)
      else if (d != "gemini") payload$seconds <- as.character(r$seconds)
    }
    path <- if (d == "gemini") paste0("/", .path_id(if (startsWith(r$model, "models/")) r$model else paste0("models/", r$model), TRUE), ":predictLongRunning") else if (xai) "/videos/generations" else "/videos"
    return(list(.emit(lm, "POST", path, payload)))
  }
  if (action == "list") {
    if (xai) .unsupported(lm$definition$id, "video listing")
    if (d == "gemini") {
      if (is.null(model)) .unsupported(lm$definition$id, "video listing without a model")
      path <- paste0("/", .path_id(if (startsWith(model, "models/")) model else paste0("models/", model), TRUE), "/operations")
    } else path <- "/videos"
    return(list(.emit(lm, "GET", path, params = setNames(list(.positive_limit(limit)), if (d == "gemini") "pageSize" else "limit"))))
  }
  if (action == "result_fetch") {
    if (xai) return(list())
    if (d == "gemini") {
      samples <- .wire_array(status_body$response$generateVideoResponse$generatedSamples)
      if (!length(samples) || is.null(samples[[1L]]$video$uri)) .abort("Video result has no URI.", provider = lm$definition$id)
      return(list(.provider_fetch(lm, samples[[1L]]$video$uri)))
    }
    return(list(.emit(lm, "GET", paste0("/videos/", .path_id(status_body$id), "/content"))))
  }
  .string(id, "video id")
  list(.emit(lm, "GET", if (d == "gemini") paste0("/", .path_id(id, TRUE)) else paste0("/videos/", .path_id(id))))
}
.video_info <- function(lm, data, id = NULL) {
  d <- lm$definition$dialect; xai <- lm$definition$id == "xai"
  if (d == "gemini") return(video_job_info(data$name, if (isTRUE(data$done)) if (.is_object(data$error)) "failed" else "completed" else "running", provider_data = data))
  if (xai && !is.null(data$request_id)) return(video_job_info(data$request_id, "queued", provider_data = data))
  map <- if (xai) c(pending = "running", done = "completed", failed = "failed") else c(queued = "queued", in_progress = "running", completed = "completed", failed = "failed", cancelled = "cancelled")
  raw <- .wire_string(data$status)
  if (!raw %in% names(map)) .abort("Provider returned an unknown video status; polling cannot safely continue.", provider = lm$definition$id)
  video_job_info(if (xai) id else data$id, unname(map[[raw]]), progress = data$progress, created_at = .iso_utc(data$created_at), model = data$model, provider_data = data)
}
video_op_parse <- function(lm, kind, body = NULL, ..., id = NULL, status_body = NULL, fetched = NULL) {
  .check_dots(...); .require_surface(lm, "video")
  if (kind == "job") return(.video_info(lm, .decode_body(body), id))
  if (kind == "list") {
    data <- .decode_body(body)
    return(lapply(.wire_array(data[[if (lm$definition$dialect == "gemini") "operations" else "data"]]), function(x) .video_info(lm, x)))
  }
  if (kind != "part") stop("Unknown video parse kind.", call. = FALSE)
  if (lm$definition$id == "xai") {
    if (is.null(status_body$video$url)) .abort("Video result has no URL.", provider = lm$definition$id)
    return(video_part(url = status_body$video$url, media_type = "video/mp4"))
  }
  if (is.null(fetched) || is.null(fetched$headers[["content-type"]])) .abort("Video result needs fetched bytes and content-type.", provider = lm$definition$id)
  video_part(data = .base64_encode(fetched$body), media_type = sub(";.*$", "", fetched$headers[["content-type"]]))
}
video_submit <- function(lm, request, ...) { .check_dots(...); video_op_parse(lm, "job", .send_wire(lm, video_op_build(lm, "submit", request = request)[[1L]])$body) }
video_status <- function(lm, id, ...) { .check_dots(...); video_op_parse(lm, "job", .send_wire(lm, video_op_build(lm, "status", id = id)[[1L]])$body, id = id) }
video_list <- function(lm, ..., limit = 20L, model = NULL) { .check_dots(...); video_op_parse(lm, "list", .send_wire(lm, video_op_build(lm, "list", limit = limit, model = model)[[1L]])$body) }
video_result <- function(lm, id, ...) {
  .check_dots(...); info <- video_status(lm, id)
  if (info$status != "completed") stop(paste("Video is not completed; status is", info$status), call. = FALSE)
  fetches <- video_op_build(lm, "result_fetch", status_body = info$provider_data)
  fetched <- if (length(fetches)) .send_wire(lm, fetches[[1L]]) else NULL
  video_op_parse(lm, "part", status_body = info$provider_data, fetched = fetched)
}

# Handles are mutable connection-bound objects. Snapshots remain typed values;
# property access never contacts the provider. Only wait() polls.
.job_handle <- function(lm, info, type) {
  state <- new.env(parent = emptyenv()); state$info <- info
  structure(list(state = state, lm = lm, type = type), class = "lm15_job")
}
print.lm15_job <- function(x, ...) { cat("<lm15 ", x$type, " job: ", x$state$info$id, "; ", x$state$info$status, ">\n", sep = ""); invisible(x) }
batch <- function(lm, request, ...) { .check_dots(...); .job_handle(lm, batch_submit(lm, request), "batch") }
batch_job <- function(lm, id, ...) { .check_dots(...); .job_handle(lm, batch_status(lm, id), "batch") }
batches <- function(lm, ..., limit = 20L) { .check_dots(...); lapply(batch_list(lm, limit = limit), function(info) .job_handle(lm, info, "batch")) }
video_generate <- function(lm, request, ...) { .check_dots(...); .job_handle(lm, video_submit(lm, request), "video") }
video_job <- function(lm, id, ...) { .check_dots(...); .job_handle(lm, video_status(lm, id), "video") }
video_jobs <- function(lm, ..., limit = 20L, model = NULL) { .check_dots(...); lapply(video_list(lm, limit = limit, model = model), function(info) .job_handle(lm, info, "video")) }
job_info <- function(job) job$state$info
refresh <- function(job, ...) {
  .check_dots(...)
  job$state$info <- if (job$type == "batch") batch_status(job$lm, job$state$info$id) else video_status(job$lm, job$state$info$id)
  invisible(job)
}
wait <- function(job, ..., poll_every = 5, timeout = 300, cancelled = function() FALSE) {
  .check_dots(...)
  .number(poll_every, "poll_every"); if (poll_every <= 0) stop("poll_every must be positive.", call. = FALSE)
  if (!is.null(timeout)) { .number(timeout, "timeout"); if (timeout < 0) stop("timeout must be non-negative.", call. = FALSE) }
  deadline <- if (is.null(timeout)) Inf else proc.time()[["elapsed"]] + timeout
  repeat {
    if (job$state$info$status %in% c("completed", "failed", "cancelled", "expired")) return(invisible(job))
    if (isTRUE(cancelled())) stop(structure(list(message = "Job wait cancelled; provider job was not cancelled.", call = NULL, info = job$state$info), class = c("lm15_wait_cancelled", "error", "condition")))
    left <- deadline - proc.time()[["elapsed"]]
    if (left <= 0) stop(structure(list(message = "Job wait deadline expired; provider job may still be running.", call = NULL, info = job$state$info), class = c("lm15_wait_timeout", "error", "condition")))
    Sys.sleep(min(poll_every, left))
    if (proc.time()[["elapsed"]] >= deadline) next
    refresh(job)
  }
}
results <- function(job, ...) { .check_dots(...); if (job$type != "batch") stop("results() requires a batch job; use result() for video.", call. = FALSE); batch_results(job$lm, job$state$info$id) }
result <- function(job, ...) { .check_dots(...); if (inherits(job, "lm15_turn_view")) return(job$result()); if (job$type != "video") stop("result() requires a video job; use results() for a batch.", call. = FALSE); video_result(job$lm, job$state$info$id) }
cancel <- function(job, ...) { .check_dots(...); if (job$type != "batch") .unsupported(job$lm$definition$id, "video cancellation"); job$state$info <- batch_cancel(job$lm, job$state$info$id); invisible(job) }
