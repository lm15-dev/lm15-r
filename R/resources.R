.send_wire <- function(lm, wire) {
  result <- lm$transport(wire)
  if (result$status >= 300L) stop(.redact_wire_condition(normalize_error(lm, result$status, rawToChar(result$body), headers = result$headers), wire))
  result
}
.body_object <- function(response) {
  out <- .json_decode(rawToChar(response$body))
  if (!.is_object(out)) .abort("Provider returned a non-object response.")
  out
}
.iso_utc <- function(x) {
  if (is.null(x)) return(NULL)
  if (inherits(x, "lm15_json_number") || is.numeric(x)) {
    seconds <- suppressWarnings(as.numeric(x)); time <- as.POSIXct(seconds, origin = "1970-01-01", tz = "UTC")
  } else {
    value <- sub("Z$", "+0000", x)
    value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
    time <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OS%z", tz = "UTC"))
    if (is.na(time)) time <- suppressWarnings(as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  }
  if (length(time) != 1L || is.na(time)) NULL else format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC", usetz = FALSE)
}

build_models_request <- function(lm, ...) {
  .check_dots(...); .require_surface(lm, "models")
  params <- switch(lm$definition$dialect, anthropic = list(limit = 1000L), gemini = list(pageSize = 1000L), list())
  if (lm$definition$access$backend == "chatgpt-codex") params$client_version <- lm$definition$access$backend_options$client_version
  .emit(lm, "GET", "/models", params = params)
}
parse_models_response <- function(lm, body, ...) {
  .check_dots(...)
  data <- if (is.raw(body)) .json_decode(rawToChar(body)) else if (is.character(body)) .json_decode(body) else body
  dialect <- lm$definition$dialect; codex <- lm$definition$access$backend == "chatgpt-codex"
  family <- switch(dialect, "openai-chat" = "openai_chat", "openai-responses" = "openai_responses", anthropic = "anthropic_messages", gemini = "gemini_generate_content")
  out <- list()
  for (entry in .wire_array(data[[if (dialect == "gemini" || codex) "models" else "data"]])) {
    if (!.is_object(entry)) next
    id <- entry[[if (dialect == "gemini") "name" else if (codex) "slug" else "id"]]
    if (!is.character(id) || length(id) != 1L || !nzchar(id)) next
    if (dialect == "gemini") id <- sub("^models/", "", id)
    out[[length(out) + 1L]] <- model_info(id, lm$definition$id, family, origin = model_origin(provider_data = entry))
  }
  out
}
list_models <- function(lm, ...) { .check_dots(...); parse_models_response(lm, .send_wire(lm, build_models_request(lm))$body) }

.multipart <- function(fields, files) {
  # Boundary generation is independent of model/request content and carries
  # no secret. Verify absence from payload bytes before using it.
  repeat {
    boundary <- basename(tempfile("lm15-"))
    needle <- charToRaw(boundary)
    collision <- any(vapply(files, function(f) length(grepRaw(needle, f$data, fixed = TRUE)), integer(1)) > 0L)
    if (!collision) break
  }
  raw <- raw(); append <- function(x) raw <<- c(raw, if (is.raw(x)) x else charToRaw(enc2utf8(x)))
  quoted <- function(s) {
    if (grepl("[\r\n]", s)) stop("Multipart names cannot contain line breaks.", call. = FALSE)
    gsub('"', "%22", s, fixed = TRUE)
  }
  for (name in names(fields)) append(paste0("--", boundary, "\r\nContent-Disposition: form-data; name=\"", quoted(name), "\"\r\n\r\n", .wire_string(fields[[name]]), "\r\n"))
  for (f in files) {
    if (grepl("[\r\n]", f$media_type)) stop("Media type cannot contain line breaks.", call. = FALSE)
    append(paste0("--", boundary, "\r\nContent-Disposition: form-data; name=\"", quoted(f$name), "\"; filename=\"", quoted(f$filename), "\"\r\nContent-Type: ", f$media_type, "\r\n\r\n"))
    append(f$data); append("\r\n")
  }
  append(paste0("--", boundary, "--\r\n"))
  list(body = raw, content_type = paste0("multipart/form-data; boundary=", boundary))
}
.file_resource <- function(id) {
  if (grepl("://", id, fixed = TRUE)) {
    if (!grepl("/files/", id, fixed = TRUE)) stop("Gemini file URI has no files resource.", call. = FALSE)
    id <- paste0("files/", sub("^.*?/files/", "", id))
  }
  if (startsWith(id, "files/")) id else paste0("files/", id)
}
file_op_build <- function(lm, action, ..., upload_request = NULL, file_id = NULL, limit = 20L, cursor = NULL) {
  .check_dots(...); .require_surface(lm, "files")
  if (!action %in% c("upload", "get", "list", "delete", "download")) stop("Unknown file action.", call. = FALSE)
  dialect <- lm$definition$dialect
  if (action == "upload") {
    r <- validate(upload_request); bytes <- media_bytes(r)
    if (dialect == "gemini") {
      # Dedicated upload root belongs to the configured host, not a hardcoded
      # Google URL that would bypass a custom endpoint.
      boundary <- basename(tempfile("lm15-"))
      if (grepl("[\r\n]", r$media_type)) stop("Invalid upload media type.", call. = FALSE)
      raw <- c(charToRaw(paste0("--", boundary, "\r\nContent-Type: application/json; charset=UTF-8\r\n\r\n", .json_encode(json_object(file = json_object(display_name = r$filename))), "\r\n--", boundary, "\r\nContent-Type: ", r$media_type, "\r\n\r\n")), bytes, charToRaw(paste0("\r\n--", boundary, "--\r\n")))
      upload <- lm; upload$base_url <- sub("(/v1beta)$", "/upload\\1", lm$base_url)
      if (identical(upload$base_url, lm$base_url)) .unsupported(lm$definition$id, "upload root for this base URL")
      return(.emit(upload, "POST", "/files", body = raw, params = r$extensions %||% list(), headers = list("content-type" = paste0("multipart/related; boundary=", boundary), "x-goog-upload-protocol" = "multipart")))
    }
    fields <- r$extensions %||% json_object()
    if (dialect == "openai-responses" && is.null(fields$purpose)) fields$purpose <- "user_data"
    form <- .multipart(fields, list(list(name = "file", filename = r$filename, media_type = r$media_type, data = bytes)))
    return(.emit(lm, "POST", "/files", body = form$body, headers = list("content-type" = form$content_type)))
  }
  if (action == "list") {
    .number(limit, "limit", TRUE)
    if (limit <= 0) stop("limit must be positive.", call. = FALSE)
    params <- setNames(list(limit), if (dialect == "gemini") "pageSize" else "limit")
    if (!is.null(cursor)) params[[switch(dialect, gemini = "pageToken", anthropic = "page", "after")]] <- cursor
    return(.emit(lm, "GET", "/files", params = params))
  }
  .string(file_id, "file_id")
  path <- if (dialect == "gemini") paste0("/", .path_id(.file_resource(file_id), TRUE)) else paste0("/files/", .path_id(file_id))
  if (action == "download") path <- paste0(path, if (dialect == "gemini") ":download" else "/content")
  .emit(lm, if (action == "delete") "DELETE" else "GET", path, params = if (action == "download" && dialect == "gemini") list(alt = "media") else list())
}
.parse_file <- function(lm, data) {
  dialect <- lm$definition$dialect
  if (dialect == "gemini" && .is_object(data$file)) data <- data$file
  if (dialect == "gemini") return(file_info(data$uri %||% data$name, filename = data$displayName, media_type = data$mimeType, size_bytes = if (!is.null(data$sizeBytes)) .number(if (is.character(data$sizeBytes)) as.numeric(data$sizeBytes) else data$sizeBytes, "size_bytes", TRUE) else NULL, created_at = .iso_utc(data$createTime), expires_at = .iso_utc(data$expirationTime), readiness = if (endsWith(.wire_string(data$state), "PROCESSING")) "pending" else if (endsWith(.wire_string(data$state), "FAILED")) "failed" else "ready", downloadable = if (!is.null(data$downloadUri)) TRUE else if (identical(data$source, "UPLOADED")) FALSE else NULL, provider_data = data))
  if (dialect == "anthropic") return(file_info(data$id, filename = data$filename, media_type = data$mime_type, size_bytes = data$size_bytes, created_at = .iso_utc(data$created_at), expires_at = .iso_utc(data$expires_at), downloadable = data$downloadable, provider_data = data))
  readiness <- switch(.wire_string(data$status), uploaded = "pending", pending = "pending", error = "failed", failed = "failed", "ready")
  file_info(data$id, filename = data$filename, size_bytes = data$bytes, created_at = .iso_utc(data$created_at), expires_at = .iso_utc(data$expires_at), readiness = readiness, provider_data = data)
}
file_op_parse <- function(lm, body, ..., page = FALSE) {
  .check_dots(...)
  data <- if (is.raw(body)) .json_decode(rawToChar(body)) else if (is.character(body)) .json_decode(body) else body
  if (!page) return(.parse_file(lm, data))
  dialect <- lm$definition$dialect
  entries <- .wire_array(data[[if (dialect == "gemini") "files" else "data"]])
  cursor <- switch(dialect, gemini = data$nextPageToken, anthropic = data$next_page, if (isTRUE(data$has_more) && length(entries)) data$last_id else NULL)
  file_page(items = lapply(entries, function(e) .parse_file(lm, e)), next_cursor = cursor)
}
file_upload <- function(lm, request, ...) { .check_dots(...); file_op_parse(lm, .send_wire(lm, file_op_build(lm, "upload", upload_request = request))$body) }
file_get <- function(lm, id, ...) { .check_dots(...); file_op_parse(lm, .send_wire(lm, file_op_build(lm, "get", file_id = id))$body) }
file_list <- function(lm, ..., limit = 20L, cursor = NULL) { .check_dots(...); file_op_parse(lm, .send_wire(lm, file_op_build(lm, "list", limit = limit, cursor = cursor))$body, page = TRUE) }
file_delete <- function(lm, id, ...) { .check_dots(...); .send_wire(lm, file_op_build(lm, "delete", file_id = id)); invisible(NULL) }
file_download <- function(lm, id, ...) { .check_dots(...); .send_wire(lm, file_op_build(lm, "download", file_id = id))$body }

cache_op_build <- function(lm, action, ..., prefix = NULL, id = NULL, limit = 20L, cursor = NULL, ttl_seconds = NULL, label = NULL) {
  .check_dots(...); .require_surface(lm, "caches")
  if (!action %in% c("create", "get", "list", "delete", "update")) stop("Unknown cache action.", call. = FALSE)
  if (!is.null(ttl_seconds)) { ttl_seconds <- .number(ttl_seconds, "ttl_seconds", TRUE); if (ttl_seconds <= 0) stop("ttl_seconds must be positive.", call. = FALSE) }
  if (action == "create") {
    prefix <- validate(prefix)
    if (length(as_dict(prefix$config))) stop("Cache prefix must carry default config.", call. = FALSE)
    payload <- .build_payload(lm, prefix)
    payload$model <- if (startsWith(prefix$model, "models/")) prefix$model else paste0("models/", prefix$model)
    if (!is.null(ttl_seconds)) payload$ttl <- paste0(ttl_seconds, "s")
    if (!is.null(label)) payload$displayName <- label
    return(.emit(lm, "POST", "/cachedContents", payload))
  }
  if (action == "list") {
    params <- list(pageSize = .number(limit, "limit", TRUE)); if (!is.null(cursor)) params$pageToken <- cursor
    return(.emit(lm, "GET", "/cachedContents", params = params))
  }
  .string(id, "cache id")
  resource <- if (startsWith(id, "cachedContents/")) id else paste0("cachedContents/", id)
  if (action == "update" && is.null(ttl_seconds)) stop("Cache update requires ttl_seconds.", call. = FALSE)
  .emit(lm, switch(action, get = "GET", delete = "DELETE", update = "PATCH"), paste0("/", .path_id(resource, TRUE)), if (action == "update") json_object(ttl = paste0(ttl_seconds, "s")) else NULL)
}
.parse_cache <- function(data) cache_info(data$name, sub("^models/", "", data$model), tokens = data$usageMetadata$totalTokenCount, created_at = .iso_utc(data$createTime), expires_at = .iso_utc(data$expireTime), label = data$displayName, provider_data = data)
cache_create <- function(lm, prefix, ..., ttl_seconds = NULL, label = NULL) { .check_dots(...); .parse_cache(.body_object(.send_wire(lm, cache_op_build(lm, "create", prefix = prefix, ttl_seconds = ttl_seconds, label = label)))) }
cache_get <- function(lm, id, ...) { .check_dots(...); .parse_cache(.body_object(.send_wire(lm, cache_op_build(lm, "get", id = id)))) }
cache_list <- function(lm, ..., limit = 20L, cursor = NULL) {
  .check_dots(...); data <- .body_object(.send_wire(lm, cache_op_build(lm, "list", limit = limit, cursor = cursor)))
  cache_page(items = lapply(.wire_array(data$cachedContents), .parse_cache), next_cursor = data$nextPageToken)
}
cache_delete <- function(lm, id, ...) { .check_dots(...); .send_wire(lm, cache_op_build(lm, "delete", id = id)); invisible(NULL) }
cache_update <- function(lm, id, ..., ttl_seconds) { .check_dots(...); .parse_cache(.body_object(.send_wire(lm, cache_op_build(lm, "update", id = id, ttl_seconds = ttl_seconds)))) }
cache <- function(lm, prefix, ..., ttl_seconds = NULL, label = NULL) {
  .check_dots(...); pair <- .route(lm, prefix); lm <- pair$lm; prefix <- pair$request
  if (length(as_dict(prefix$config))) stop("Cache prefix must carry default config.", call. = FALSE)
  resource <- if (isTRUE(lm$definition$access$supports$caches)) cache_create(lm, prefix, ttl_seconds = ttl_seconds, label = label) else NULL
  cached_prefix(prefix, resource = resource)
}
cached_request <- function(cached, messages, ..., config = NULL) {
  .check_dots(...)
  if (is.character(messages)) messages <- list(message_user(messages))
  if (inherits(messages, "lm15_Message")) messages <- list(messages)
  config <- config %||% .new_value("Config", list())
  if (!is.null(config$cache)) stop("Cached prefix owns the cache setting.", call. = FALSE)
  config$cache <- cache_config(prefix_until_index = length(cached$prefix$messages) - 1L, resource = cached$resource$id)
  request(cached$prefix$model, c(cached$prefix$messages, messages), system = cached$prefix$system, tools = cached$prefix$tools, config = config)
}
