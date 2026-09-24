# Deserialization tolerances are deliberately confined to this boundary.
.read_value <- function(fields, type) {
  if (!.is_object(fields)) stop("Canonical JSON values must be objects.", call. = FALSE)
  if (type %in% names(.unions)) {
    discriminator <- fields[[if (type == "Credential") "kind" else "type"]]
    if (type == "Tool") discriminator <- if (identical(discriminator, "builtin")) "builtin" else "function"
    if (!is.character(discriminator) || length(discriminator) != 1L || !discriminator %in% names(.unions[[type]])) stop(paste("Unknown", type, "discriminator."), call. = FALSE)
    type <- unname(.unions[[type]][[discriminator]])
  }
  if (!type %in% names(.schemas)) stop("Unknown canonical kind.", call. = FALSE)
  if (type == "Reasoning") {
    effort <- fields$effort %||% if (identical(fields$enabled, FALSE)) "off" else "medium"
    if (identical(effort, "adaptive")) effort <- "medium"
    fields["effort"] <- list(effort)
    if (identical(effort, "off")) { fields$thinking_budget <- NULL; fields$summary <- NULL }
    else if (!"thinking_budget" %in% names(fields)) fields["thinking_budget"] <- list(fields$budget)
  }
  if (type %in% c("Message", "ToolResultPart")) {
    key <- if (type == "Message") "parts" else "content"
    raw <- fields[[key]] %||% list()
    if (type == "ToolResultPart" && is.character(raw)) raw <- if (nzchar(raw)) list(raw) else list()
    if (!.is_array(raw)) raw <- list()
    fields[key] <- list(lapply(raw, function(p) if (.is_object(p)) .read_value(p, "Part") else text(.scalar_text(p))))
  }
  # On media parts, canonical JSON requires media_type even though the R
  # constructor supplies the media factory's default.
  if (type %in% c("ImagePart", "AudioPart", "VideoPart", "DocumentPart", "BinaryPart") && is.null(fields$media_type))
    stop("Media JSON requires media_type.", call. = FALSE)
  telemetry <- switch(type, Response = "usage", StreamEndEvent = "usage", ModelInfo = c("origin", "inference"), LiveConfig = c("input_format", "output_format"), character())
  for (name in telemetry) if (!.is_object(fields[[name]])) fields[[name]] <- NULL
  schema <- .schemas[[type]]
  # Unknown ordinary fields are ignored, but unknown discriminators never
  # are. Opaque objects are preserved without cleaning their children.
  fields <- fields[intersect(names(fields), names(schema))]
  .new_value(type, fields, reading = TRUE)
}

from_dict <- function(value, kind, ...) {
  .check_dots(...)
  .string(kind, "kind")
  type <- if (kind %in% names(.kind_names)) unname(.kind_names[[kind]]) else kind
  .read_value(value, type)
}

.required_shape <- list(
  ContinuationState = c("provider", "kind", "data"),
  TextPart = "text", ThinkingPart = "text", RefusalPart = "text",
  ToolCallPart = c("id", "name", "input"), ToolResultPart = c("id", "content"),
  Message = c("role", "parts"), FunctionTool = c("name", "parameters"),
  Request = c("model", "messages"), Response = c("model", "message", "finish_reason"),
  TextDelta = c("text", "part_index"), ThinkingDelta = c("text", "part_index"),
  ToolCallDelta = c("input", "part_index"), ImageDelta = "part_index", AudioDelta = "part_index", CitationDelta = "part_index",
  ContinuationDelta = c("provider", "kind", "data"),
  TokenLogprob = c("token", "logprob"), TopLogprob = c("token", "logprob"),
  LiveClientTurnEvent = c("parts", "turn_complete"), LiveClientTextEvent = "text", LiveServerTextEvent = "text",
  LiveServerToolCallEvent = c("id", "name", "input"), LiveClientToolResultEvent = c("id", "content")
)

as_dict <- function(x, ..., include_provider_data = FALSE) {
  .check_dots(...)
  if (!inherits(x, "lm15_value")) stop("Expected a canonical lm15 object.", call. = FALSE)
  type <- .value_type(x); schema <- .schemas[[type]]
  out <- json_object()
  for (name in names(.unions)) {
    u <- .unions[[name]]
    if (type %in% u) { out[[if (name == "Credential") "kind" else "type"]] <- names(u)[match(type, u)]; break }
  }
  required <- .required_shape[[type]] %||% character()
  for (name in names(schema)) {
    if (type == "Response" && name == "provider_data" && !include_provider_data) next
    v <- x[[name, exact = TRUE]]; desc <- sub("\\?$", "", schema[[name]])
    if (name == "is_error" && identical(v, FALSE)) next
    if (name == "supports_reasoning" && identical(v, FALSE)) next
    if (is.null(v)) next
    if (inherits(v, "lm15_value")) {
      include <- type == "BatchEntry" && name == "response"
      v <- as_dict(v, include_provider_data = include)
    } else if (endsWith(desc, "[]") || (desc == "system" && is.list(v))) {
      inner <- sub("\\[\\]$", "", desc)
      v <- .json_array(lapply(v, function(item) {
        if (inherits(item, "lm15_value")) return(as_dict(item))
        if (inner %in% c("positive", "nonnegative", "int")) return(.json_number(.integer_digits(item)))
        item
      }))
    } else if (desc %in% c("positive", "nonnegative", "int", "percentage")) {
      v <- .json_number(.integer_digits(v))
    } else if (desc == "raw") v <- .base64_encode(v)
    if (type == "ModelInfo" && name == "origin" && identical(names(v), "type") && identical(v$type, "provider")) next
    delta_field <- type %in% unname(.unions$Delta) && name != "logprobs"
    if (!name %in% required && !delta_field && .empty(v)) next
    out[name] <- list(v)
  }
  out
}

# Introspection reads the same schemas validation uses, not a second list.
surface_dump <- function() {
  list(types = lapply(.schemas, function(s) list(fields = names(s))), enums = .vocab)
}
