\name{lm15}
\alias{lm15}
\alias{api_key}
\alias{as_dict}
\alias{as_json}
\alias{audio_delta}
\alias{audio_format}
\alias{audio_part}
\alias{aws_credentials}
\alias{batch}
\alias{batch_cancel}
\alias{batch_entry}
\alias{batch_job}
\alias{batch_job_info}
\alias{batch_list}
\alias{batch_op_build}
\alias{batch_op_parse}
\alias{batch_request}
\alias{batch_results}
\alias{batch_status}
\alias{batch_submit}
\alias{batches}
\alias{bearer_token}
\alias{binary_part}
\alias{browser_dispatch}
\alias{build_live_completion}
\alias{build_models_request}
\alias{build_request}
\alias{builtin_tool}
\alias{cache}
\alias{cache_config}
\alias{cache_create}
\alias{cache_delete}
\alias{cache_get}
\alias{cache_info}
\alias{cache_list}
\alias{cache_op_build}
\alias{cache_page}
\alias{cache_update}
\alias{cached_prefix}
\alias{cached_request}
\alias{cancel}
\alias{canonical_provider}
\alias{citation_delta}
\alias{citation_part}
\alias{citations}
\alias{cloud_credential_provider}
\alias{complete}
\alias{complete_from_openai_chat}
\alias{config}
\alias{continuation_data}
\alias{continuation_delta}
\alias{continuation_state}
\alias{credential_expired}
\alias{credentials_path}
\alias{document_part}
\alias{error_detail}
\alias{explain_auth}
\alias{file_delete}
\alias{file_download}
\alias{file_get}
\alias{file_info}
\alias{file_list}
\alias{file_op_build}
\alias{file_op_parse}
\alias{file_page}
\alias{file_upload}
\alias{file_upload_request}
\alias{from_dict}
\alias{from_json}
\alias{function_tool}
\alias{generate_pkce}
\alias{generation_build}
\alias{generation_parse}
\alias{image_delta}
\alias{image_generate}
\alias{image_generation_request}
\alias{image_generation_response}
\alias{image_part}
\alias{inference_model_info}
\alias{inference_pricing}
\alias{integer_value}
\alias{job_info}
\alias{json_array}
\alias{json_object}
\alias{jwt_rs256}
\alias{list_models}
\alias{live}
\alias{live_client_audio_event}
\alias{live_client_end_audio_event}
\alias{live_client_image_event}
\alias{live_client_interrupt_event}
\alias{live_client_text_event}
\alias{live_client_tool_result_event}
\alias{live_client_turn_event}
\alias{live_config}
\alias{live_connection_request}
\alias{live_decode}
\alias{live_encode}
\alias{live_server_audio_event}
\alias{live_server_error_event}
\alias{live_server_interrupted_event}
\alias{live_server_text_event}
\alias{live_server_tool_call_delta_event}
\alias{live_server_tool_call_event}
\alias{live_server_turn_end_event}
\alias{live_server_usage_event}
\alias{live_setup_frames}
\alias{lm15_error}
\alias{load_local_credential}
\alias{login}
\alias{materialize_response}
\alias{media_bytes}
\alias{message}
\alias{message_assistant}
\alias{message_developer}
\alias{message_tool}
\alias{message_user}
\alias{model_info}
\alias{model_origin}
\alias{new_lm}
\alias{new_router}
\alias{normalize_error}
\alias{openai_chat_model_string}
\alias{parse_json}
\alias{parse_models_response}
\alias{parse_response}
\alias{pkce_challenge}
\alias{poll_device_login}
\alias{providers}
\alias{reasoning}
\alias{refresh}
\alias{refusal}
\alias{refusal_part}
\alias{replay_stream}
\alias{request}
\alias{request_from_openai_chat}
\alias{resolve}
\alias{resolve_openai_chat}
\alias{response}
\alias{response_from_openai_chat}
\alias{response_text}
\alias{result}
\alias{results}
\alias{retryable}
\alias{route_openai_chat}
\alias{router_lm}
\alias{select_auth_scheme}
\alias{sigv4_sign}
\alias{speech_generate}
\alias{speech_generation_request}
\alias{speech_generation_response}
\alias{start_device_login}
\alias{stream}
\alias{stream_delta_event}
\alias{stream_end_event}
\alias{stream_error_event}
\alias{stream_from_openai_chat}
\alias{stream_start_event}
\alias{surface_dump}
\alias{text}
\alias{text_delta}
\alias{text_part}
\alias{thinking}
\alias{thinking_delta}
\alias{thinking_part}
\alias{token_exchange_build}
\alias{token_exchange_parse}
\alias{token_logprob}
\alias{tool_call_delta}
\alias{tool_call_info}
\alias{tool_call_part}
\alias{tool_calls}
\alias{tool_choice}
\alias{tool_result_part}
\alias{top_logprob}
\alias{transport_curl}
\alias{usage}
\alias{validate}
\alias{vet_handle}
\alias{video_generate}
\alias{video_generation_request}
\alias{video_job}
\alias{video_job_info}
\alias{video_jobs}
\alias{video_list}
\alias{video_op_build}
\alias{video_op_parse}
\alias{video_part}
\alias{video_result}
\alias{video_status}
\alias{video_submit}
\alias{wait}
\alias{write_credentials}
\title{Provider-neutral foundation model conversations}
\description{
Construct typed requests and content parts, translate provider wire formats,
and obtain typed responses and incremental stream events. This development
version is implementation work in progress, not a claim of conformance.
}
\details{
Use \code{new_router()} for model-name routing or \code{new_lm("openai")}
for a provider directly. Construct a \code{request()} with a list of
\code{message_user()}, \code{message_assistant()}, and \code{message_tool()}
values. Settings are constructed with \code{config()}; optional arguments
following dots must be named, and unused arguments are errors.

\code{complete(lm, request)} returns a Response. The R streaming idiom is
\code{stream(lm, request, on_event)}: the callback receives typed events as
they arrive, and the function returns the assembled Response after completion.
Callbacks execute on the R thread. An interruption releases the HTTP transfer;
it does not guarantee that provider generation or billing has stopped.
\code{replay_stream()} decodes a recorded stream without networking.

\code{as_json()} and \code{from_json(text, kind)} use canonical JSON.
\code{json_object()} and \code{json_array()} distinguish empty objects from
empty arrays. Named lists are objects, unnamed lists are arrays, and atomic
values must be length-one scalars. Use lists for arrays, not vector guessing.
\code{NULL} denotes absent optional fields; zero and FALSE are actual data.
NA, non-finite numbers, factors and arbitrary R objects are not JSON values.
Typed integer fields preserve large integers as decimal values. Use
\code{integer_value("9007199254740993")} to construct one without rounding.
Large values support exact addition, subtraction and comparisons; coercion to
an R double refuses when it would lose precision. Opaque numbers read from
JSON retain their original decimal tokens.

Canonical types have snake-case constructors matching the contract's type
names: \code{tool_call_part()}, \code{cache_config()},
\code{image_generation_request()}, etc. Content helpers \code{text()},
\code{thinking()} and role-specific message constructors cover common calls.
\code{validate()} revalidates an object. Ordinary dollar and bracket
replacement of canonical objects also validates before returning a new value.

\code{build_request()} and resource \code{*_op_build()} functions construct
wire requests without sending them. Credential providers are invoked at
request-build time. Explicit raw wire values contain credentials: do not log
or save them. Printing and structure displays hide them.
\code{parse_response()} and \code{*_op_parse()} functions transform recorded
bodies. Model listing is advisory and does not alter request-building rules.

File, cache, batch, image, speech and video operations are supported where the
provider access policy allows them. Job handles returned by \code{batch()}
and \code{video_generate()} retain a snapshot. \code{job_info()} reads it;
\code{refresh()} fetches a new snapshot; only \code{wait()} polls. Local wait
timeouts have class \code{lm15_wait_timeout}, distinct from provider timeouts.
A wait's deadline bounds polling, not an already-running HTTP request.

\code{api_key()}, \code{bearer_token()} and \code{aws_credentials()} create
credential values. A credential callback may return a value or a key string.
\code{sigv4_sign()}, \code{jwt_rs256()} and PKCE helpers use the optional
openssl package. Credential serialization is deliberately explicit and is
not redacted. Stored logins renew under a cross-process lock and are written
privately and atomically. Expired subscriptions never fall back to paid keys.
Cloud credential providers cache tokens per client identity. The offline
\code{explain_auth()} report never contacts cloud endpoints. \code{login("xai")}
runs the owned device-code flow; other providers give their CLI or console hint.
\code{live()} returns a session with send, next_event, turn, interrupt, and close
functions. Its codec is shared with recorded live-conversation replay.

The browser module at \code{system.file("browser/lm15.mjs", package="lm15")}
uses browser fetch and the R codecs through webR. It requires an initialized
webR worker with this package and jsonlite already installed. It does not use
curl, local CLI credentials, or persistent browser credential storage.
Provider CORS rules and forbidden browser headers still apply.

See the package README and IMPLEMENTATION.md for current limitations and
verification status. Tests cover the installed package, offline contract
transformations, credential lifecycle, live sessions and the browser bridge.
The WebAssembly build has been exercised in real Chromium with local HTTP
and WebSocket test servers. Desktop live TLS is separately tested for untrusted
certificates, hostname mismatches, valid trust, fragmentation and frame limits.
Windows and macOS release verification remain separate from local Linux results.
}
\value{
Constructors return typed S3 list values. Client and router constructors return
configuration objects. Network calls return the canonical response type for
the endpoint. Deletion returns invisible NULL. JSON serialization returns one
string. Pure parsers return canonical values or lists of values. Conditions
inherit from LM15Error and the corresponding contract error classes.
}
\examples{
r <- request("openai:gpt-4.1-mini", list(message_user("Hello")),
             config = config(max_tokens = 100L))
as_json(r)
# Networking is explicit, not part of these examples:
# answer <- complete(new_router(), r)
# response_text(answer)
}
