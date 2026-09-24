.field_error <- function(field, message) stop(paste0(field, ": ", message), call. = FALSE)
.string <- function(x, field, empty = FALSE) {
  if (!is.character(x) || is.object(x) || length(x) != 1L || is.na(x) || (!empty && !nzchar(x)))
    .field_error(field, if (empty) "must be one string" else "must be one non-empty string")
  x
}
.number <- function(x, field, integer = FALSE) {
  if (inherits(x, "lm15_integer")) {
    if (integer) return(x)
    x <- suppressWarnings(as.numeric(unclass(x)))
  }
  if (inherits(x, "lm15_json_number")) {
    token <- unclass(x)
    if (integer) {
      if (grepl("[.eE]", token) && !is.finite(suppressWarnings(as.numeric(token)))) .field_error(field, "must be finite")
      return(tryCatch(.integer_from_digits(.integer_normalize(token)), error = function(e) .field_error(field, "must be an integer, without rounding")))
    }
    x <- suppressWarnings(as.numeric(token))
  }
  if (!is.numeric(x) || is.object(x) || length(x) != 1L || !is.finite(x)) .field_error(field, "must be one finite number, not a boolean or NA")
  if (integer) {
    if (x != trunc(x)) .field_error(field, "must be an integer, without rounding")
    return(.integer_from_digits(sprintf("%.0f", x)))
  }
  as.double(x)
}
.base64 <- function(x, field) {
  if (is.raw(x)) x <- .base64_encode(x)
  .string(x, field)
  payload <- sub("^data:[^,]*;base64,", "", x)
  payload <- gsub("[[:space:]]", "", payload)
  if (!nzchar(payload) || nchar(payload) %% 4L || !grepl("^[A-Za-z0-9+/]*={0,2}$", payload)) .field_error(field, "must contain base64 data")
  # Validation examines the normalized payload; the original spelling is
  # retained, matching the reference's media serialization.
  x
}

.default_field <- function(type, name, desc) {
  if (name == "continuation" || endsWith(desc, "[]")) {
    if (type == "InferenceModelInfo" && name %in% c("input_modalities", "output_modalities")) return(list("text"))
    return(list())
  }
  if (name == "part_index" && type != "ContinuationDelta") return(0L)
  if (name == "is_error" || name == "supports_reasoning") return(FALSE)
  if (name == "turn_complete") return(TRUE)
  if (name == "channels") return(1L)
  if (name == "readiness") return("ready")
  if (name == "currency") return("USD")
  if (type == "ModelOrigin" && name == "type") return("provider")
  if (name == "mode") return("auto")
  if (name == "parameters") return(json_object(type = "object", properties = json_object()))
  if (name %in% c("data", "input") && desc == "object") return(json_object())
  if (name == "config" && desc == "Config") return(.new_value("Config", list()))
  if (name == "usage" && desc == "Usage") return(.new_value("Usage", list()))
  if (name == "origin") return(.new_value("ModelOrigin", list()))
  if (name %in% c("text", "input_delta") && desc == "text") return("")
  if (type == "ToolCallDelta" && name == "input") return("")
  if (type == "ErrorDetail" && name == "message") return("")
  if (name == "media_type") return(switch(type, ImagePart = "image/png", AudioPart = "audio/wav", VideoPart = "video/mp4", DocumentPart = "application/pdf", BinaryPart = "application/octet-stream", FileUploadRequest = "application/octet-stream", LiveClientAudioEvent = "audio/pcm;rate=16000", LiveClientImageEvent = "image/jpeg", NULL))
  NULL
}

.coerce_field <- function(x, desc, field, reading) {
  if (endsWith(desc, "?")) {
    if (is.null(x)) return(NULL)
    desc <- substr(desc, 1L, nchar(desc) - 1L)
  }
  if (endsWith(desc, "[]")) {
    inner <- substr(desc, 1L, nchar(desc) - 2L)
    if (is.null(x)) return(list())
    if (inherits(x, "lm15_value")) x <- list(x)
    if (is.atomic(x) && !inherits(x, "lm15_json_number")) x <- as.list(x)
    if (!.is_array(x)) .field_error(field, "must be an array")
    return(unname(lapply(seq_along(x), function(i) .coerce_field(x[[i]], inner, paste0(field, "[", i - 1L, "]"), reading))))
  }
  if (is.null(x)) .field_error(field, "is required")
  if (desc %in% names(.vocab)) {
    .string(x, field)
    if (!x %in% .vocab[[desc]]) .field_error(field, paste("must be one of", paste(.vocab[[desc]], collapse = ", ")))
    return(x)
  }
  if (desc %in% c("string", "text")) return(.string(x, field, empty = desc == "text"))
  if (desc == "bool") {
    if (!is.logical(x) || length(x) != 1L || is.na(x)) .field_error(field, "must be TRUE or FALSE")
    return(x)
  }
  if (desc %in% c("int", "positive", "nonnegative", "percentage", "float", "float_nonnegative", "probability", "penalty")) {
    x <- .number(x, field, integer = !desc %in% c("float", "float_nonnegative", "probability", "penalty"))
    if (desc == "penalty" && (x < -2 || x > 2)) .field_error(field, "must be between -2 and 2")
    if (desc == "positive" && x <= 0) .field_error(field, "must be positive")
    if (desc %in% c("nonnegative", "float_nonnegative", "percentage", "probability") && x < 0) .field_error(field, "must be non-negative")
    if (desc == "probability" && x > 1) .field_error(field, "must be at most 1")
    if (desc == "percentage" && x > 100) .field_error(field, "must be at most 100")
    return(if (desc %in% c("float", "float_nonnegative", "probability", "penalty")) as.double(x) else x)
  }
  if (desc == "json") {
    # Any JSON value (MAP-13 asked/applied); checked only for being encodable.
    tryCatch(.json_encode(x), error = function(e) .field_error(field, "must be a JSON value"))
    return(x)
  }
  if (desc == "base64") return(.base64(x, field))
  if (desc == "raw") {
    if (reading && is.character(x)) x <- tryCatch(jsonlite::base64_dec(x), error = function(e) .field_error(field, "invalid base64"))
    if (!is.raw(x) || !length(x)) .field_error(field, "must be non-empty raw bytes")
    return(x)
  }
  if (desc == "object") {
    if (!.is_object(x)) .field_error(field, "must be a JSON object; use json_object() for an empty object")
    .json_encode(x)
    return(x)
  }
  if (desc == "system") {
    if (is.character(x) && length(x) == 1L) return(.string(x, field))
    x <- .coerce_field(x, "Part[]", field, reading)
    .check_parts(x, "user", field)
    return(x)
  }
  if (desc %in% c(names(.schemas), names(.unions))) {
    if (inherits(x, "lm15_value")) {
      allowed <- if (desc %in% names(.unions)) unname(.unions[[desc]]) else desc
      if (!.value_type(x) %in% allowed) .field_error(field, paste("must be", desc))
      return(validate(x))
    }
    if (reading && .is_object(x)) return(.read_value(x, desc))
    .field_error(field, paste("must be a typed", desc, "object"))
  }
  .field_error(field, paste("unknown field type", desc))
}

.value_type <- function(x) attr(x, "lm15_type", exact = TRUE)
.new_value <- function(type, fields, reading = FALSE) {
  if (!type %in% names(.schemas)) stop("Unknown canonical type.", call. = FALSE)
  schema <- .schemas[[type]]
  if (length(fields) && (is.null(names(fields)) || anyDuplicated(names(fields)) || any(!names(fields) %in% names(schema))))
    stop(paste(type, "has unknown, unnamed, or duplicate fields."), call. = FALSE)
  out <- list()
  for (name in names(schema)) {
    x <- if (name %in% names(fields)) fields[[name]] else .default_field(type, name, schema[[name]])
    out[name] <- list(.coerce_field(x, schema[[name]], paste0(type, "$", name), reading))
  }
  if (!is.null(out$extensions) && !length(out$extensions)) out$extensions <- NULL
  if (type == "Usage" && is.null(out$total_tokens) && !is.null(out$input_tokens) && !is.null(out$output_tokens))
    out$total_tokens <- .integer_add(out$input_tokens, out$output_tokens)
  if (type == "BatchRequest" && is.null(out$model) && length(out$requests)) out$model <- out$requests[[1L]]$model
  .check_invariants(type, out)
  unions <- names(Filter(function(u) type %in% u, .unions))
  structure(out, lm15_type = type, class = c(paste0("lm15_", type), paste0("lm15_", unions), "lm15_value", "list"))
}

.check_parts <- function(parts, role, field) {
  if (!length(parts)) .field_error(field, "requires at least one part; use text('') for empty content")
  kinds <- vapply(parts, .value_type, "")
  forbidden <- switch(role, tool = setdiff(unname(.unions$Part), "ToolResultPart"), assistant = "ToolResultPart", result = c("ToolCallPart", "ToolResultPart", "ThinkingPart", "RefusalPart"), c("ToolCallPart", "ToolResultPart", "ThinkingPart", "RefusalPart", "CitationPart"))
  if (any(kinds %in% forbidden)) .field_error(field, paste("contains a part not allowed in", role, "content"))
}
.check_invariants <- function(type, x) {
  fail <- function(message) .field_error(type, message)
  if (type %in% c("ImagePart", "AudioPart", "VideoPart", "DocumentPart", "BinaryPart")) {
    if (sum(!vapply(x[c("data", "url", "file_id", "path")], is.null, logical(1))) != 1L) fail("requires exactly one of data, url, file_id, path")
  }
  if (type %in% c("ImageDelta", "AudioDelta") && sum(!vapply(x[c("data", "url", "file_id")], is.null, logical(1))) > 1L) fail("at most one media address is allowed")
  if (type %in% c("CitationPart", "CitationDelta") && all(vapply(x[c("url", "title", "text")], is.null, logical(1)))) fail("requires url, title, or text")
  if (type %in% c("ToolResultPart", "LiveClientToolResultEvent")) .check_parts(x$content, "result", type)
  if (type == "Message") .check_parts(x$parts, x$role, type)
  if (type == "LiveClientTurnEvent") .check_parts(x$parts, "user", type)
  if (type == "Reasoning" && x$effort == "off" && (!is.null(x$thinking_budget) || !is.null(x$summary))) fail("off cannot carry a budget or summary")
  if (type == "CacheConfig") {
    if (x$mode == "off" && any(!vapply(x[setdiff(names(x), "mode")], is.null, logical(1)))) fail("off cannot carry other cache settings")
    if (!is.null(x$prefix) && !is.null(x$prefix_until_index)) fail("prefix and prefix_until_index are mutually exclusive")
  }
  if (type == "ToolChoice" && x$mode == "none" && (length(x$allowed) || !is.null(x$parallel))) fail("none cannot carry allowed or parallel")
  if (type %in% c("Request", "LiveConfig")) {
    n <- vapply(x$tools, function(t) t$name, "")
    if (anyDuplicated(n)) fail("tool names must be unique")
    if (type == "Request") {
      if (!length(x$messages)) fail("requires messages")
      allowed <- unlist(x$config$tool_choice$allowed, use.names = FALSE)
      if (any(!allowed %in% n)) fail("tool_choice.allowed names an undeclared tool")
    }
  }
  if (type == "Response" && x$message$role != "assistant") fail("message must have the assistant role")
  if (type == "Config" && !is.null(x$response_format)) {
    f <- x$response_format
    if (!is.character(f$type) || length(f$type) != 1L || !f$type %in% c("json_object", "json_schema")) fail("response_format.type must be json_object or json_schema")
    allowed <- if (f$type == "json_object") "type" else c("type", "schema", "name", "strict")
    if (any(!names(f) %in% allowed)) fail("response_format has unexpected fields")
    if (f$type == "json_schema" && !.is_object(f$schema)) fail("json_schema requires a schema object")
    if ("name" %in% names(f)) .string(f$name, "response_format.name")
    if ("strict" %in% names(f)) .coerce_field(f$strict, "bool", "response_format.strict", FALSE)
  }
  if (type == "BatchRequest" && !length(x$requests)) fail("requires requests")
  if (type == "BatchEntry") {
    if (x$outcome == "succeeded" && (is.null(x$response) || !is.null(x$error))) fail("succeeded requires response and no error")
    if (x$outcome == "errored" && (is.null(x$error) || !is.null(x$response))) fail("errored requires error and no response")
    if (x$outcome %in% c("cancelled", "expired") && (!is.null(x$response) || !is.null(x$error))) fail("cancelled/expired cannot carry response or error")
  }
  if (type == "FileUploadRequest" && sum(!vapply(x[c("bytes_data", "path")], is.null, logical(1))) != 1L) fail("requires exactly one of bytes_data or path")
  if (type == "ImageGenerationResponse" && !length(x$images)) fail("requires images")
  if (type == "CachedPrefix") {
    if (length(as_dict(x$prefix$config))) fail("prefix must have default config")
    if (!is.null(x$resource) && x$resource$model != x$prefix$model) fail("resource model must equal prefix model")
  }
  if (type %in% c("LiveClientAudioEvent", "LiveServerAudioEvent") && !is.null(x$media_type) && !startsWith(x$media_type, "audio/")) fail("media_type must start with audio/")
  if (type == "LiveClientImageEvent" && !startsWith(x$media_type, "image/")) fail("media_type must start with image/")
  invisible(NULL)
}

validate <- function(x, ...) {
  .check_dots(...)
  if (!inherits(x, "lm15_value")) stop("Expected a canonical lm15 object.", call. = FALSE)
  .new_value(.value_type(x), unclass(x))
}

# Canonical objects use R's copy-on-modify values. Replacements go through
# validation, so a typo or incompatible content cannot corrupt an object.
`$<-.lm15_value` <- function(x, name, value) {
  fields <- unclass(x); fields[name] <- list(value)
  .new_value(.value_type(x), fields)
}
`[[<-.lm15_value` <- function(x, i, value) {
  fields <- unclass(x); fields[i] <- list(value)
  .new_value(.value_type(x), fields)
}
`[<-.lm15_value` <- function(x, i, value) {
  fields <- unclass(x); fields[i] <- value
  .new_value(.value_type(x), fields)
}
`$.lm15_value` <- function(x, name) {
  if (name == "kind" && inherits(x, "lm15_Credential")) return(names(.unions$Credential)[match(.value_type(x), .unions$Credential)])
  if (name == "type" && .value_type(x) != "ModelOrigin" && !inherits(x, "lm15_Credential")) {
    for (u in .unions) if (.value_type(x) %in% u) return(names(u)[match(.value_type(x), u)])
  }
  unclass(x)[[name, exact = TRUE]]
}
print.lm15_value <- function(x, ...) {
  .check_dots(...)
  cat("<lm15 ", .value_type(x), ">\n", sep = "")
  cat("Fields: ", paste(names(x), collapse = ", "), "\n", sep = "")
  invisible(x)
}

str.lm15_value <- function(object, ...) { print.lm15_value(object); invisible(NULL) }

.normalize_parts <- function(content) {
  if (is.character(content)) return(lapply(as.list(content), text))
  if (inherits(content, "lm15_Part")) return(list(content))
  if (!is.list(content)) stop("Content must be text, a part, or a list of parts.", call. = FALSE)
  lapply(content, function(x) if (is.character(x) && length(x) == 1L) text(x) else x)
}
text <- function(content, ..., continuation = list()) { .check_dots(...); .new_value("TextPart", list(text = content, continuation = continuation)) }
thinking <- function(content, ..., continuation = list()) { .check_dots(...); .new_value("ThinkingPart", list(text = content, continuation = continuation)) }
refusal <- function(content, ..., continuation = list()) { .check_dots(...); .new_value("RefusalPart", list(text = content, continuation = continuation)) }
message_user <- function(content, ...) { .check_dots(...); .new_value("Message", list(role = "user", parts = .normalize_parts(content))) }
message_developer <- function(content, ...) { .check_dots(...); .new_value("Message", list(role = "developer", parts = .normalize_parts(content))) }
message_assistant <- function(content, ...) { .check_dots(...); .new_value("Message", list(role = "assistant", parts = .normalize_parts(content))) }
message_tool <- function(id, content, ..., is_error = FALSE, name = NULL) {
  .check_dots(...)
  part <- .new_value("ToolResultPart", list(id = id, content = .normalize_parts(content), is_error = is_error, name = name))
  .new_value("Message", list(role = "tool", parts = list(part)))
}

response_text <- function(response, ...) {
  .check_dots(...)
  parts <- response$message$parts
  if (any(!vapply(parts, function(p) p$type %in% c("text", "thinking", "citation"), logical(1)))) return(NULL)
  texts <- Filter(function(p) p$type == "text", parts)
  if (!length(texts)) return(NULL)
  paste(vapply(texts, function(p) p$text, ""), collapse = "\n")
}
tool_calls <- function(response, ...) { .check_dots(...); Filter(function(p) p$type == "tool_call", response$message$parts) }
citations <- function(response, ...) { .check_dots(...); Filter(function(p) p$type == "citation", response$message$parts) }
parse_json <- function(response, ...) {
  .check_dots(...); value <- response_text(response)
  if (is.null(value)) stop("Response has non-text content.", call. = FALSE)
  .json_decode(value)
}
continuation_data <- function(x, provider, kind) {
  states <- if (inherits(x, "lm15_value")) x$continuation else x
  for (s in states) if (s$provider == provider && s$kind == kind) return(s$data)
  NULL
}
media_bytes <- function(part, ...) {
  .check_dots(...)
  if (!is.null(part$data)) return(jsonlite::base64_dec(gsub("[[:space:]]", "", sub("^data:[^,]*;base64,", "", part$data))))
  if (!is.null(part$bytes_data)) return(part$bytes_data)
  if (!is.null(part$path)) {
    con <- file(part$path, "rb"); on.exit(close(con), add = TRUE)
    return(readBin(con, "raw", n = file.info(part$path)$size))
  }
  stop("URL/file-id media has no local bytes; fetch it explicitly first.", call. = FALSE)
}
