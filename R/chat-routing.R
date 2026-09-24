openai_chat_model_string <- function(model, ...) {
  .check_dots(...); .string(model, "model")
  if (grepl(":", model, fixed = TRUE)) return(model)
  slash <- regexpr("/", model, fixed = TRUE)[[1L]]
  if (slash > 0L && slash < nchar(model)) {
    head <- substr(model, 1L, slash - 1L)
    provider <- .provider_tables()$chat_model_prefixes[[head]]
    if (is.null(provider)) .abort("Unknown or ambiguous foreign provider prefix; use an explicit provider:model name.", "unknown_model", model = model)
    return(paste0(provider, ":", substring(model, slash + 1L)))
  }
  model
}
resolve_openai_chat <- function(router, model, ...) {
  .check_dots(...)
  resolution <- resolve(router, openai_chat_model_string(model))
  if (resolution$source == "rule" && resolution$provider == "openai") resolution <- resolve(router, paste0("openai-chat:", resolution$model))
  resolution
}
route_openai_chat <- function(router, body, ...) {
  .check_dots(...)
  if (!inherits(router, "lm15_router") || !.is_object(body)) stop("Expected a router and a Chat Completions request object.", call. = FALSE)
  client_keys <- intersect(names(body), names(.provider_tables()$chat_client_keywords))
  if (length(client_keys)) .unsupported("openai-chat", paste0("client options inside a request (", paste(client_keys, collapse = ", "), "); configure new_router() or its transport instead"))
  resolution <- resolve_openai_chat(router, body$model)
  lm <- .router_lm(router, resolution)
  body$model <- resolution$model
  provider <- if (lm$definition$dialect == "openai-chat") lm$definition$id else "openai-chat"
  list(request = request_from_openai_chat(body, provider = provider), lm = lm, resolution = resolution)
}
stream_from_openai_chat <- function(lm, body, on_event, ...) {
  .check_dots(...)
  complete_from_openai_chat(lm, body, streaming = TRUE, on_event = on_event)
}
