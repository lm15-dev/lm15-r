# Generated from schema.R by tools/generate-constructors.py.

api_key <- function(value, ...) {
  .check_dots(...)
  .new_value("ApiKey", list(value = value))
}

bearer_token <- function(value, ..., expires_at = NULL) {
  .check_dots(...)
  .new_value("BearerToken", list(value = value, expires_at = expires_at))
}

aws_credentials <- function(access_key_id, secret_access_key, ..., session_token = NULL, expires_at = NULL) {
  .check_dots(...)
  .new_value("AwsCredentials", list(access_key_id = access_key_id, secret_access_key = secret_access_key, session_token = session_token, expires_at = expires_at))
}

continuation_state <- function(provider, kind, ..., data = json_object()) {
  .check_dots(...)
  .new_value("ContinuationState", list(provider = provider, kind = kind, data = data))
}

text_part <- function(text, ..., continuation = list()) {
  .check_dots(...)
  .new_value("TextPart", list(text = text, continuation = continuation))
}

thinking_part <- function(text, ..., continuation = list()) {
  .check_dots(...)
  .new_value("ThinkingPart", list(text = text, continuation = continuation))
}

refusal_part <- function(text, ..., continuation = list()) {
  .check_dots(...)
  .new_value("RefusalPart", list(text = text, continuation = continuation))
}

citation_part <- function(..., url = NULL, title = NULL, text = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("CitationPart", list(url = url, title = title, text = text, continuation = continuation))
}

image_part <- function(..., media_type = "image/png", data = NULL, url = NULL, file_id = NULL, path = NULL, detail = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("ImagePart", list(media_type = media_type, data = data, url = url, file_id = file_id, path = path, detail = detail, continuation = continuation))
}

audio_part <- function(..., media_type = "audio/wav", data = NULL, url = NULL, file_id = NULL, path = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("AudioPart", list(media_type = media_type, data = data, url = url, file_id = file_id, path = path, continuation = continuation))
}

video_part <- function(..., media_type = "video/mp4", data = NULL, url = NULL, file_id = NULL, path = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("VideoPart", list(media_type = media_type, data = data, url = url, file_id = file_id, path = path, continuation = continuation))
}

document_part <- function(..., media_type = "application/pdf", data = NULL, url = NULL, file_id = NULL, path = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("DocumentPart", list(media_type = media_type, data = data, url = url, file_id = file_id, path = path, continuation = continuation))
}

binary_part <- function(..., media_type = "application/octet-stream", data = NULL, url = NULL, file_id = NULL, path = NULL, continuation = list()) {
  .check_dots(...)
  .new_value("BinaryPart", list(media_type = media_type, data = data, url = url, file_id = file_id, path = path, continuation = continuation))
}

tool_call_part <- function(id, name, ..., input = json_object(), continuation = list()) {
  .check_dots(...)
  .new_value("ToolCallPart", list(id = id, name = name, input = input, continuation = continuation))
}

tool_result_part <- function(id, content, ..., name = NULL, is_error = FALSE, continuation = list()) {
  .check_dots(...)
  .new_value("ToolResultPart", list(id = id, content = content, name = name, is_error = is_error, continuation = continuation))
}

message <- function(role, parts, ..., continuation = list()) {
  .check_dots(...)
  .new_value("Message", list(role = role, parts = parts, continuation = continuation))
}

text_delta <- function(text, ..., part_index = 0L, logprobs = list()) {
  .check_dots(...)
  .new_value("TextDelta", list(text = text, part_index = part_index, logprobs = logprobs))
}

thinking_delta <- function(text, ..., part_index = 0L) {
  .check_dots(...)
  .new_value("ThinkingDelta", list(text = text, part_index = part_index))
}

audio_delta <- function(..., data = NULL, url = NULL, file_id = NULL, part_index = 0L, media_type = NULL) {
  .check_dots(...)
  .new_value("AudioDelta", list(data = data, url = url, file_id = file_id, part_index = part_index, media_type = media_type))
}

image_delta <- function(..., data = NULL, url = NULL, file_id = NULL, part_index = 0L, media_type = NULL) {
  .check_dots(...)
  .new_value("ImageDelta", list(data = data, url = url, file_id = file_id, part_index = part_index, media_type = media_type))
}

tool_call_delta <- function(input, ..., part_index = 0L, id = NULL, name = NULL) {
  .check_dots(...)
  .new_value("ToolCallDelta", list(input = input, part_index = part_index, id = id, name = name))
}

citation_delta <- function(..., text = NULL, url = NULL, title = NULL, part_index = 0L) {
  .check_dots(...)
  .new_value("CitationDelta", list(text = text, url = url, title = title, part_index = part_index))
}

continuation_delta <- function(provider, kind, ..., data = json_object(), part_index = NULL) {
  .check_dots(...)
  .new_value("ContinuationDelta", list(provider = provider, kind = kind, data = data, part_index = part_index))
}

stream_start_event <- function(..., id = NULL, model = NULL, adaptations = list()) {
  .check_dots(...)
  .new_value("StreamStartEvent", list(id = id, model = model, adaptations = adaptations))
}

stream_delta_event <- function(delta, ...) {
  .check_dots(...)
  .new_value("StreamDeltaEvent", list(delta = delta))
}

stream_end_event <- function(..., finish_reason = NULL, usage = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("StreamEndEvent", list(finish_reason = finish_reason, usage = usage, provider_data = provider_data))
}

stream_error_event <- function(error, ...) {
  .check_dots(...)
  .new_value("StreamErrorEvent", list(error = error))
}

error_detail <- function(code, ..., message = "", provider_code = NULL) {
  .check_dots(...)
  .new_value("ErrorDetail", list(code = code, message = message, provider_code = provider_code))
}

function_tool <- function(name, ..., description = NULL, parameters = json_object(type = "object", properties = json_object())) {
  .check_dots(...)
  .new_value("FunctionTool", list(name = name, description = description, parameters = parameters))
}

builtin_tool <- function(name, ..., config = NULL) {
  .check_dots(...)
  .new_value("BuiltinTool", list(name = name, config = config))
}

tool_choice <- function(..., mode = "auto", allowed = list(), parallel = NULL) {
  .check_dots(...)
  .new_value("ToolChoice", list(mode = mode, allowed = allowed, parallel = parallel))
}

reasoning <- function(effort, ..., thinking_budget = NULL, summary = NULL) {
  .check_dots(...)
  .new_value("Reasoning", list(effort = effort, thinking_budget = thinking_budget, summary = summary))
}

cache_config <- function(..., mode = "auto", retention = NULL, key = NULL, prefix_until_index = NULL, prefix = NULL, resource = NULL) {
  .check_dots(...)
  .new_value("CacheConfig", list(mode = mode, retention = retention, key = key, prefix_until_index = prefix_until_index, prefix = prefix, resource = resource))
}

config <- function(..., max_tokens = NULL, temperature = NULL, top_p = NULL, top_k = NULL, seed = NULL, frequency_penalty = NULL, presence_penalty = NULL, stop = list(), response_format = NULL, tool_choice = NULL, reasoning = NULL, cache = NULL, service_tier = NULL, user_id = NULL, store = NULL, logprobs = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("Config", list(max_tokens = max_tokens, temperature = temperature, top_p = top_p, top_k = top_k, seed = seed, frequency_penalty = frequency_penalty, presence_penalty = presence_penalty, stop = stop, response_format = response_format, tool_choice = tool_choice, reasoning = reasoning, cache = cache, service_tier = service_tier, user_id = user_id, store = store, logprobs = logprobs, extensions = extensions))
}

request <- function(model, messages, ..., system = NULL, tools = list(), config = .new_value("Config", list())) {
  .check_dots(...)
  .new_value("Request", list(model = model, messages = messages, system = system, tools = tools, config = config))
}

usage <- function(..., input_tokens = NULL, output_tokens = NULL, total_tokens = NULL, cache_read_tokens = NULL, cache_write_tokens = NULL, reasoning_tokens = NULL, input_audio_tokens = NULL, output_audio_tokens = NULL) {
  .check_dots(...)
  .new_value("Usage", list(input_tokens = input_tokens, output_tokens = output_tokens, total_tokens = total_tokens, cache_read_tokens = cache_read_tokens, cache_write_tokens = cache_write_tokens, reasoning_tokens = reasoning_tokens, input_audio_tokens = input_audio_tokens, output_audio_tokens = output_audio_tokens))
}

response <- function(model, message, finish_reason, ..., id = NULL, usage = .new_value("Usage", list()), logprobs = list(), provider_data = NULL, adaptations = list()) {
  .check_dots(...)
  .new_value("Response", list(id = id, model = model, message = message, finish_reason = finish_reason, usage = usage, logprobs = logprobs, provider_data = provider_data, adaptations = adaptations))
}

top_logprob <- function(token, logprob, ..., bytes = list(), token_id = NULL) {
  .check_dots(...)
  .new_value("TopLogprob", list(token = token, logprob = logprob, bytes = bytes, token_id = token_id))
}

token_logprob <- function(token, logprob, ..., bytes = list(), token_id = NULL, top = list()) {
  .check_dots(...)
  .new_value("TokenLogprob", list(token = token, logprob = logprob, bytes = bytes, token_id = token_id, top = top))
}

file_upload_request <- function(filename, ..., bytes_data = NULL, media_type = "application/octet-stream", extensions = NULL, path = NULL) {
  .check_dots(...)
  .new_value("FileUploadRequest", list(filename = filename, bytes_data = bytes_data, media_type = media_type, extensions = extensions, path = path))
}

file_info <- function(id, ..., filename = NULL, media_type = NULL, size_bytes = NULL, created_at = NULL, expires_at = NULL, readiness = "ready", downloadable = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("FileInfo", list(id = id, filename = filename, media_type = media_type, size_bytes = size_bytes, created_at = created_at, expires_at = expires_at, readiness = readiness, downloadable = downloadable, provider_data = provider_data))
}

file_page <- function(..., items = list(), next_cursor = NULL) {
  .check_dots(...)
  .new_value("FilePage", list(items = items, next_cursor = next_cursor))
}

cache_info <- function(id, model, ..., tokens = NULL, created_at = NULL, expires_at = NULL, label = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("CacheInfo", list(id = id, model = model, tokens = tokens, created_at = created_at, expires_at = expires_at, label = label, provider_data = provider_data))
}

cache_page <- function(..., items = list(), next_cursor = NULL) {
  .check_dots(...)
  .new_value("CachePage", list(items = items, next_cursor = next_cursor))
}

cached_prefix <- function(prefix, ..., resource = NULL) {
  .check_dots(...)
  .new_value("CachedPrefix", list(prefix = prefix, resource = resource))
}

batch_request <- function(requests, ..., model = NULL, label = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("BatchRequest", list(model = model, requests = requests, label = label, extensions = extensions))
}

batch_job_info <- function(id, status, ..., label = NULL, created_at = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("BatchJobInfo", list(id = id, status = status, label = label, created_at = created_at, provider_data = provider_data))
}

batch_entry <- function(index, outcome, ..., response = NULL, error = NULL) {
  .check_dots(...)
  .new_value("BatchEntry", list(index = index, outcome = outcome, response = response, error = error))
}

image_generation_request <- function(model, prompt, ..., size = NULL, images = list(), extensions = NULL) {
  .check_dots(...)
  .new_value("ImageGenerationRequest", list(model = model, prompt = prompt, size = size, images = images, extensions = extensions))
}

image_generation_response <- function(images, ..., text = NULL, id = NULL, model = NULL, usage = .new_value("Usage", list()), provider_data = NULL) {
  .check_dots(...)
  .new_value("ImageGenerationResponse", list(images = images, text = text, id = id, model = model, usage = usage, provider_data = provider_data))
}

speech_generation_request <- function(model, prompt, ..., voice = NULL, format = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("SpeechGenerationRequest", list(model = model, prompt = prompt, voice = voice, format = format, extensions = extensions))
}

speech_generation_response <- function(audio, ..., id = NULL, model = NULL, usage = .new_value("Usage", list()), provider_data = NULL) {
  .check_dots(...)
  .new_value("SpeechGenerationResponse", list(audio = audio, id = id, model = model, usage = usage, provider_data = provider_data))
}

video_generation_request <- function(model, prompt, ..., seconds = NULL, images = list(), extensions = NULL) {
  .check_dots(...)
  .new_value("VideoGenerationRequest", list(model = model, prompt = prompt, seconds = seconds, images = images, extensions = extensions))
}

video_job_info <- function(id, status, ..., progress = NULL, created_at = NULL, model = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("VideoJobInfo", list(id = id, status = status, progress = progress, created_at = created_at, model = model, provider_data = provider_data))
}

audio_format <- function(encoding, sample_rate, ..., channels = 1L) {
  .check_dots(...)
  .new_value("AudioFormat", list(encoding = encoding, sample_rate = sample_rate, channels = channels))
}

live_config <- function(model, ..., system = NULL, tools = list(), voice = NULL, input_format = NULL, output_format = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("LiveConfig", list(model = model, system = system, tools = tools, voice = voice, input_format = input_format, output_format = output_format, extensions = extensions))
}

live_client_turn_event <- function(parts, ..., turn_complete = TRUE) {
  .check_dots(...)
  .new_value("LiveClientTurnEvent", list(parts = parts, turn_complete = turn_complete))
}

live_client_audio_event <- function(data, ..., media_type = "audio/pcm;rate=16000") {
  .check_dots(...)
  .new_value("LiveClientAudioEvent", list(data = data, media_type = media_type))
}

live_client_image_event <- function(data, ..., media_type = "image/jpeg") {
  .check_dots(...)
  .new_value("LiveClientImageEvent", list(data = data, media_type = media_type))
}

live_client_text_event <- function(text, ...) {
  .check_dots(...)
  .new_value("LiveClientTextEvent", list(text = text))
}

live_client_tool_result_event <- function(id, content, ...) {
  .check_dots(...)
  .new_value("LiveClientToolResultEvent", list(id = id, content = content))
}

live_client_interrupt_event <- function(...) {
  .check_dots(...)
  .new_value("LiveClientInterruptEvent", list())
}

live_client_end_audio_event <- function(...) {
  .check_dots(...)
  .new_value("LiveClientEndAudioEvent", list())
}

live_server_audio_event <- function(data, ..., media_type = NULL) {
  .check_dots(...)
  .new_value("LiveServerAudioEvent", list(data = data, media_type = media_type))
}

live_server_text_event <- function(text, ...) {
  .check_dots(...)
  .new_value("LiveServerTextEvent", list(text = text))
}

live_server_tool_call_event <- function(id, name, ..., input = json_object()) {
  .check_dots(...)
  .new_value("LiveServerToolCallEvent", list(id = id, name = name, input = input))
}

live_server_tool_call_delta_event <- function(input_delta, ..., id = NULL, name = NULL) {
  .check_dots(...)
  .new_value("LiveServerToolCallDeltaEvent", list(input_delta = input_delta, id = id, name = name))
}

live_server_interrupted_event <- function(...) {
  .check_dots(...)
  .new_value("LiveServerInterruptedEvent", list())
}

live_server_turn_end_event <- function(..., usage = .new_value("Usage", list())) {
  .check_dots(...)
  .new_value("LiveServerTurnEndEvent", list(usage = usage))
}

live_server_usage_event <- function(..., usage = .new_value("Usage", list())) {
  .check_dots(...)
  .new_value("LiveServerUsageEvent", list(usage = usage))
}

live_server_error_event <- function(error, ...) {
  .check_dots(...)
  .new_value("LiveServerErrorEvent", list(error = error))
}

tool_call_info <- function(id, name, ..., input = json_object()) {
  .check_dots(...)
  .new_value("ToolCallInfo", list(id = id, name = name, input = input))
}

inference_pricing <- function(..., input_per_million = NULL, output_per_million = NULL, cache_read_per_million = NULL, cache_write_per_million = NULL, currency = "USD", dimensions = NULL) {
  .check_dots(...)
  .new_value("InferencePricing", list(input_per_million = input_per_million, output_per_million = output_per_million, cache_read_per_million = cache_read_per_million, cache_write_per_million = cache_write_per_million, currency = currency, dimensions = dimensions))
}

inference_model_info <- function(..., input_modalities = list("text"), output_modalities = list("text"), context_window = NULL, max_output_tokens = NULL, supports_reasoning = FALSE, reasoning_efforts = list(), pricing = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("InferenceModelInfo", list(input_modalities = input_modalities, output_modalities = output_modalities, context_window = context_window, max_output_tokens = max_output_tokens, supports_reasoning = supports_reasoning, reasoning_efforts = reasoning_efforts, pricing = pricing, extensions = extensions))
}

model_origin <- function(..., type = "provider", id = NULL, base_model = NULL, provider_data = NULL) {
  .check_dots(...)
  .new_value("ModelOrigin", list(type = type, id = id, base_model = base_model, provider_data = provider_data))
}

model_info <- function(id, provider, api_family, ..., aliases = list(), origin = .new_value("ModelOrigin", list()), inference = NULL, extensions = NULL) {
  .check_dots(...)
  .new_value("ModelInfo", list(id = id, provider = provider, api_family = api_family, aliases = aliases, origin = origin, inference = inference, extensions = extensions))
}
