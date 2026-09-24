.gemini_inband_error <- function(data, provider) {
  reason <- data$promptFeedback$blockReason
  if (!is.null(reason) && reason != "BLOCK_REASON_UNSPECIFIED") return(lm15_error("Provider blocked the prompt.", code = "invalid_request", provider = provider, provider_code = "promptFeedback"))
  candidates <- .wire_array(data$candidates)
  if (!length(candidates)) return(NULL)
  candidate <- .wire_object(candidates[[1L]])
  blocked <- c("SAFETY", "RECITATION", "LANGUAGE", "BLOCKLIST", "PROHIBITED_CONTENT", "SPII", "MALFORMED_FUNCTION_CALL", "IMAGE_SAFETY", "IMAGE_PROHIBITED_CONTENT", "IMAGE_OTHER", "NO_IMAGE", "IMAGE_RECITATION", "UNEXPECTED_TOOL_CALL", "TOO_MANY_TOOL_CALLS", "MISSING_THOUGHT_SIGNATURE", "MALFORMED_RESPONSE")
  if (!is.null(candidate$finishReason) && candidate$finishReason %in% blocked) return(lm15_error(.wire_string(candidate$finishMessage, "Provider blocked the candidate."), code = "invalid_request", provider = provider, provider_code = candidate$finishReason))
  NULL
}
