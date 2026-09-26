# Judgments: declared keys in, a distribution out (MAP-14,
# lm15-contract changes/2026-09-17-judgments.md).
#
# A judgment is a top-level property of a json_schema response_format that
# declares its answer set: a boolean; a string enum or anyOf of consts; or an
# ordered integer enum / anyOf of consts 0..n-1. This file reads that
# convention off a schema (section 1), rewrites judgment properties for the two
# wires that need it (section 2), folds a model's JSON text into a data part
# (section 3), and offers the helpers that emit the convention. Nothing here
# touches the network.

.max_ordered_levels <- 10L  # Jev's Score ceiling
.max_choice_keys <- 255L    # Jev's Choice ceiling

# A JSON string (not a number token), or NULL.
.json_str <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && !inherits(x, "lm15_json_number")) x else NULL
# A JSON integer (not a boolean), as an R integer, or NULL.
.json_int <- function(x) {
  if (inherits(x, "lm15_json_number")) {
    token <- unclass(x)
    return(if (grepl("^-?[0-9]+$", token)) suppressWarnings(as.integer(token)) else NULL)
  }
  if (inherits(x, "lm15_integer")) return(suppressWarnings(as.integer(.integer_digits(x))))
  if (is.numeric(x) && !is.object(x) && length(x) == 1L && is.finite(x) && x == trunc(x)) return(as.integer(x))
  NULL
}

.judgment_of <- function(name, prop) {
  if (!.is_object(prop)) return(NULL)
  instruction <- .json_str(prop$description)
  if (!is.null(instruction) && !nzchar(instruction)) instruction <- NULL
  new <- function(kind, keys, descriptions = list(), titles = list())
    structure(list(name = name, kind = kind, keys = keys, instruction = instruction,
      descriptions = setNames(lapply(keys, function(k) descriptions[[k]]), keys),
      titles = setNames(lapply(keys, function(k) titles[[k]]), keys)), class = "lm15_judgment")
  if (identical(.json_str(prop$type), "boolean")) return(new("boolean", c("true", "false")))
  enum <- prop$enum; branches <- prop$anyOf
  const_branches <- .is_array(branches) && length(branches) && all(vapply(branches, function(b) .is_object(b) && "const" %in% names(b), logical(1)))
  if (.is_array(enum) && length(enum) && !const_branches) {
    values <- enum; descriptions <- list(); titles <- list()
  } else if (const_branches && is.null(enum)) {
    values <- lapply(branches, function(b) b$const)
    keys <- vapply(branches, function(b) { v <- b$const; .json_str(v) %||% as.character(.json_int(v) %||% NA) }, "")
    descriptions <- setNames(lapply(branches, function(b) .json_str(b$description)), keys)
    titles <- setNames(lapply(branches, function(b) .json_str(b$title)), keys)
  } else return(NULL)
  type <- prop$type
  strings <- vapply(values, function(v) { s <- .json_str(v); !is.null(s) && nzchar(s) }, logical(1))
  if (all(strings)) {
    if (!is.null(type) && !identical(.json_str(type), "string")) return(NULL)
    keys <- vapply(values, function(v) v, "")
    if (anyDuplicated(keys)) return(NULL)
    return(new("choice", keys, descriptions, titles))
  }
  ints <- lapply(values, .json_int)
  if (all(!vapply(ints, is.null, logical(1))) && !any(vapply(values, is.logical, logical(1)))) {
    if (!is.null(type) && !identical(.json_str(type), "integer")) return(NULL)
    levels <- unlist(ints)
    if (length(levels) < 2L || !identical(levels, seq.int(0L, length(levels) - 1L))) return(NULL)
    return(new("ordered", as.character(levels), descriptions, titles))
  }
  NULL
}
print.lm15_judgment <- function(x, ...) {
  cat("<lm15 judgment ", x$name, ": ", x$kind, " over ", paste(x$keys, collapse = ", "), ">\n", sep = "")
  invisible(x)
}

judgments_in_schema <- function(schema, ...) {
  .check_dots(...)
  if (!.is_object(schema) || !(is.null(schema$type) || identical(.json_str(schema$type), "object"))) return(list())
  props <- schema$properties
  if (!.is_object(props)) return(list())
  out <- list()
  for (name in names(props)) {
    j <- .judgment_of(name, props[[name]])
    if (!is.null(j)) out[[name]] <- j
  }
  out
}
request_judgments <- function(request, ...) {
  .check_dots(...)
  f <- request$config$response_format
  if (!.is_object(f) || !identical(f$type, "json_schema")) return(list())
  judgments_in_schema(f$schema)
}
.non_judgment_properties <- function(schema, found) {
  props <- if (.is_object(schema)) schema$properties else NULL
  if (!.is_object(props)) return(character())
  setdiff(names(props), names(found))
}

# Section 3 on a wire that measures no distribution: "if_available" records
# "dropped"; "required" refuses before the wire (MAP-13 condition b).
.note_unmeasurable <- function(req, provider) {
  policy <- req$config$probabilities
  if (is.null(policy) || policy == "off" || !length(request_judgments(req))) return(invisible(NULL))
  if (policy == "required")
    stop(lm15_error(paste0(provider, ": config.probabilities='required' but this wire cannot measure a distribution over the declared keys (it returns a pick only); use 'if_available' or a provider that can (typesafe, or a vLLM server that honours logprob_token_ids)"), code = "unsupported_feature", provider = provider, feature = "config.probabilities"))
  .adapt("config.probabilities", "dropped", "this wire cannot measure a distribution over the declared keys; the answer carries the pick only", asked = policy)
}

# Anthropic: a judgment property with both `type` and `anyOf` has its type
# moved into every branch (the Messages wire answers 400 otherwise).
.anthropic_judgment_schema <- function(schema, found) {
  if (!length(found)) return(schema)
  for (name in names(found)) {
    prop <- schema$properties[[name]]
    if (!is.null(prop$type) && .is_array(prop$anyOf)) {
      kind <- prop$type
      branches <- lapply(prop$anyOf, function(b) { if (is.null(b$type)) b <- .json_object(c(unclass(b), list(type = kind))); b })
      prop <- .json_object(unclass(prop)[names(prop) != "type"])
      prop$anyOf <- .json_array(branches)
      schema$properties[[name]] <- prop
    }
  }
  schema
}

# Gemini: judgment properties go as `enum` with the per-key descriptions
# folded into the property description (the wire ignores anyOf/const).
.gemini_judgment_schema <- function(schema, found) {
  if (!length(found)) return(schema)
  for (name in names(found)) {
    j <- found[[name]]; prop <- schema$properties[[name]]
    if (j$kind == "boolean" || is.null(prop$anyOf)) next
    prop <- .json_object(unclass(prop)[names(prop) != "anyOf"])
    prop$type <- if (j$kind == "ordered") "integer" else "string"
    prop$enum <- .json_array(if (j$kind == "ordered") lapply(j$keys, as.integer) else as.list(j$keys))
    lines <- vapply(j$keys, function(k) {
      label <- j$titles[[k]]; desc <- j$descriptions[[k]]
      if (is.null(label) && is.null(desc)) return(k)
      paste0(k, " = ", if (!is.null(label) && nzchar(label) && !is.null(desc) && nzchar(desc)) paste0(label, ": ", desc) else (label %||% desc %||% ""))
    }, "")
    if (any(vapply(j$keys, function(k) nzchar(j$titles[[k]] %||% "") || nzchar(j$descriptions[[k]] %||% ""), logical(1)))) {
      head <- .json_str(prop$description) %||% ""
      prop$description <- trimws(paste0(head, if (nzchar(head)) " " else "", if (j$kind == "ordered") "Levels: " else "Options: ", paste(lines, collapse = "; ")))
    }
    schema$properties[[name]] <- prop
  }
  schema
}

# The single text part of a judgment answer becomes a data part holding the
# model's JSON object; a text that is not a JSON object stays text.
.replace_text_with_data <- function(parts, found) {
  if (!length(found)) return(parts)
  texts <- which(vapply(parts, function(p) identical(p$type, "text"), logical(1)))
  if (length(texts) != 1L) return(parts)
  value <- tryCatch(.json_decode(trimws(parts[[texts]]$text)), error = function(e) NULL)
  if (!.is_object(value)) return(parts)
  parts[[texts]] <- data_part(value, continuation = parts[[texts]]$continuation)
  parts
}

# Softmax over log-scores: one normalisation over the key set.
.normalize_logprobs <- function(scores) {
  top <- max(unlist(scores))
  weights <- lapply(scores, function(v) exp(v - top))
  total <- sum(unlist(weights))
  lapply(weights, function(w) w / total)
}

expected_level <- function(distribution, ...) {
  .check_dots(...)
  if (!length(distribution) || is.null(names(distribution))) stop("distribution must be a named list of level -> probability.", call. = FALSE)
  levels <- suppressWarnings(as.integer(names(distribution)))
  if (anyNA(levels)) stop("An expected level needs integer level keys.", call. = FALSE)
  sum(vapply(distribution, function(p) as.double(unclass(p)), double(1)) * levels)
}

# Helpers that emit the convention, so it need not be written by hand.
choice <- function(instruction, options, ...) {
  .check_dots(...); .string(instruction, "instruction")
  if (is.null(names(options))) {
    keys <- as.character(unlist(options)); descriptions <- rep(list(NULL), length(keys))
  } else {
    keys <- names(options); descriptions <- lapply(options, function(d) if (is.null(d) || (length(d) == 1L && is.na(d))) NULL else .string(d, "option description"))
  }
  if (!length(keys)) stop("choice needs at least one option.", call. = FALSE)
  if (anyNA(keys) || any(!nzchar(keys))) stop("choice option keys must be non-empty strings.", call. = FALSE)
  if (anyDuplicated(keys)) stop("choice option keys must be unique.", call. = FALSE)
  prop <- json_object(type = "string", description = instruction)
  if (all(vapply(descriptions, is.null, logical(1)))) prop$enum <- .json_array(as.list(keys))
  else prop$anyOf <- .json_array(lapply(seq_along(keys), function(i) if (is.null(descriptions[[i]])) json_object(const = keys[[i]]) else json_object(const = keys[[i]], description = descriptions[[i]])))
  prop
}
yes_no <- function(instruction, ...) {
  .check_dots(...); .string(instruction, "instruction")
  json_object(type = "boolean", description = instruction)
}
score <- function(instruction, levels, ...) {
  .check_dots(...); .string(instruction, "instruction")
  descriptions <- as.list(unname(unlist(levels)))
  titles <- names(levels)
  if (length(descriptions) < 2L) stop("score needs at least two levels.", call. = FALSE)
  if (length(descriptions) > .max_ordered_levels) stop(paste("score takes at most", .max_ordered_levels, "levels."), call. = FALSE)
  branches <- lapply(seq_along(descriptions), function(i) {
    b <- json_object(const = i - 1L)
    if (!is.null(titles) && !is.na(titles[[i]]) && nzchar(titles[[i]])) b$title <- titles[[i]]
    d <- descriptions[[i]]
    if (!is.null(d) && !is.na(d) && nzchar(d)) b$description <- .string(d, "level description")
    b
  })
  json_object(type = "integer", description = instruction, anyOf = .json_array(branches))
}
judgments <- function(..., name = "judgments", strict = TRUE) {
  properties <- list(...)
  if (!length(properties)) stop("judgments needs at least one property.", call. = FALSE)
  if (is.null(names(properties)) || any(!nzchar(names(properties))) || anyDuplicated(names(properties))) stop("judgments properties must be uniquely named.", call. = FALSE)
  .string(name, "name"); .coerce_field(strict, "bool", "strict", FALSE)
  schema <- json_object(type = "object", properties = .json_object(properties), required = .json_array(as.list(names(properties))), additionalProperties = FALSE)
  json_object(type = "json_schema", name = name, strict = strict, schema = schema)
}

# Reading an answer.
.first_data_part <- function(response) {
  for (p in response$message$parts) if (identical(p$type, "data")) return(p)
  NULL
}
response_data <- function(response, ...) {
  .check_dots(...)
  part <- .first_data_part(response)
  if (!is.null(part)) return(part$value)
  words <- response_text(response)
  if (is.null(words)) return(NULL)
  tryCatch(.json_decode(trimws(words)), error = function(e) NULL)
}
response_probabilities <- function(response, ...) { .check_dots(...); .first_data_part(response)$probabilities }
response_method <- function(response, ...) { .check_dots(...); .first_data_part(response)$method }
response_expected <- function(response, field, ...) {
  .check_dots(...); .string(field, "field")
  dist <- response_probabilities(response)[[field]]
  if (is.null(dist)) return(NULL)
  expected_level(dist)
}

# Chat Completions: on a server that scores named tokens (compat
# token_scoring = "logprob_token_ids") complete() measures a distribution by
# candidate-sequence likelihood; stream() and every other server answer with
# generated JSON and say that no distribution was measured.
.scores_named_tokens <- function(compat) identical(compat$token_scoring, "logprob_token_ids")
.judgments_via_token_scoring <- function(compat, req) .scores_named_tokens(compat) && (req$config$probabilities %||% "off") %in% c("if_available", "required") && length(request_judgments(req)) > 0L
.chat_judgment_policy <- function(lm, req, streaming, compat) {
  scoring <- .scores_named_tokens(compat)
  if (streaming || !scoring) {
    if (streaming && scoring && (req$config$probabilities %||% "off") %in% c("if_available", "required") && length(request_judgments(req))) {
      if (req$config$probabilities == "required")
        stop(lm15_error(paste0(lm$definition$id, ": candidate scoring produces a non-streamable DataPart; use complete() for required probabilities or a separate scoring request"), code = "unsupported_feature", provider = lm$definition$id, feature = "config.probabilities"))
      .adapt("config.probabilities", "dropped", "stream() uses generated JSON, not candidate scoring; the answer carries an unmeasured pick only; use complete() for scoring", asked = req$config$probabilities)
    } else .note_unmeasurable(req, lm$definition$id)
  }
}
