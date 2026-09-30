# The reference manual's hand-written content; tools/document.R turns it
# into man/*.Rd. Rd markup; `%` must be written as `\%`.

page <- function(name, title, functions, description, value, args = character(), details = NULL, examples = NULL, seealso = NULL, aliases = NULL, internal = FALSE, docType = NULL)
  list(name = name, title = title, functions = functions, description = description, value = value, args = args, details = details, examples = examples, seealso = seealso, aliases = aliases, internal = internal, docType = docType)

common_args <- c(
  "..." = "Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.",
  lm = "A provider client from \\code{new_lm()}. Operations that route a model string also accept a router from \\code{new_router()}.",
  auth = "The sign-in scope: \\code{local_auth()} (the default: the private credentials file), \\code{memory_auth()}, or \\code{new_auth()}.",
  provider = "A provider id, such as \\code{\"openai\"}, \\code{\"anthropic\"} or \\code{\"xai\"}. \\code{providers()} lists them; an underscore is read as a hyphen.",
  request = "A canonical request from \\code{request()}.",
  id = "The provider's identifier for the object.",
  env = "A named character vector read instead of the process environment. \\code{NULL} reads the environment; \\code{character()} reads nothing.",
  transport = "A function that sends one wire request and returns \\code{list(status, headers, body)}; see \\code{transport_curl()} and \\code{fake_transport()}. \\code{NULL} uses curl.",
  response = "A canonical response, as returned by \\code{complete()}.",
  body = "A reply body: a JSON string, raw bytes, or an already parsed JSON object.",
  continuation = "Provider continuation state that must travel with this value when it is sent back (a list of \\code{continuation_state()} values). Keep what a response gave you; do not build it by hand.",
  model = "A model identifier.",
  limit = "The largest number of entries to ask for.",
  cursor = "A paging cursor returned by a previous page, or \\code{NULL} for the first page.",
  job = "A job handle from \\code{batch()}, \\code{batch_job()}, \\code{video_generate()} or \\code{video_job()}.",
  now = "The current time, as a \\code{POSIXct}. A fixed value makes the result reproducible.",
  x = "A canonical lm15 value.",
  extensions = "A JSON object of provider-specific fields passed through unchanged (\\code{json_object()}), or \\code{NULL}.",
  provider_data = "The provider's own reply fields, kept verbatim for inspection, or \\code{NULL}.",
  label = "A label to show, or \\code{NULL}.",
  created_at = "An RFC 3339 timestamp, or \\code{NULL}.",
  expires_at = "An RFC 3339 timestamp with a time zone, or \\code{NULL} when unknown.",
  status = "An HTTP status code.",
  headers = "HTTP headers as a named list.",
  kind = "What the body describes; see Details.",
  action = "The operation to build; see Details.",
  status_body = "The JSON body of a status reply the operation depends on, or \\code{NULL}.",
  fetched = "Result bodies already fetched, keyed as the operation's builder asked for them.",
  lock_timeout = "Seconds to wait for another process holding the credentials file's lock.",
  path = "A file path.",
  timeout = "Seconds to wait.",
  settings = "Host settings for a cloud provider, such as \\code{list(region = \"us-east-1\")} or \\code{list(project = \"my-project\")}. A setting given here wins over the environment.",
  usage = "Token counts, from \\code{usage()}.",
  data = "Base64 data (a string, optionally a data URI) or raw bytes.",
  url = "A URL the provider fetches itself.",
  file_id = "The id of a file already uploaded to the provider.",
  media_type = "The media type, such as \\code{\"image/png\"}.",
  part_index = "The index of the part this fragment belongs to (0-based, as on the wire).",
  text = "Text.",
  name = "A name.",
  input = "Tool-call arguments: a JSON object (\\code{json_object()}); a delta carries a text fragment of the JSON instead.",
  content = "Content: a string, a content part, or a list of strings and parts.",
  error = "An \\code{error_detail()} value.",
  events = "A list of stream events, in order.",
  on_event = "A function called with each canonical event as it arrives, on the R thread.",
  model_id = "A model identifier.",
  value = "The value.",
  schema = "A JSON Schema as a JSON object.",
  instruction = "The question the model answers, in words."
)

pages <- list(

page("lm15-package", "lm15: provider-neutral foundation model conversations", character(),
  docType = "package", aliases = c("lm15"),
  description = r"-(
lm15 turns one request into the right call for each provider (OpenAI, Anthropic,
Google, xAI, Azure, AWS, Google Cloud, open-model hosts and local servers) and
turns every reply into the same response. You build a request, ask a client to
send it, and keep the whole response. Nothing is sent, retried, truncated or
remembered behind your back.

This R package implements the shared lm15 contract, the same one the Python,
TypeScript, Rust and Go packages implement, and passes the same conformance
suite. Names follow R conventions; the words are the family's.
)-",
  details = r"-(
Start here:
\itemize{
\item \code{\link{request}()} and \code{\link{message_user}()} build a request.
\item \code{\link{new_router}()} picks a provider from the model name;
\code{\link{new_lm}()} talks to one provider directly.
\item \code{\link{complete}()} returns a response; \code{\link{stream}()}
calls your function with each event as it arrives.
\item \code{\link{response_text}()}, \code{\link{tool_calls}()} and
\code{\link{response_data}()} read a response.
\item \code{\link{login}()} and \code{\link{connect}()} sign in to a
subscription or save a key; \code{\link{explain_auth}()} says which
credential a request would use and why.
}
When a provider's wire cannot take something you asked for, lm15 adapts it
and records what it did on the response (\code{response$adaptations}), or
refuses before sending; see \code{\link{plan}()}. Errors are conditions with
contract classes (\code{RateLimitError}, \code{AuthError}, ...); see
\code{\link{lm15_error}()}.

Networking uses the optional \pkg{curl} package. Live sessions need a libcurl
with WebSocket support at install time. Sign-in, PKCE and request signing use
\pkg{openssl}; the loopback sign-in listener uses \pkg{httpuv} and \pkg{later}.
Missing optional packages fail with a message naming them.
)-",
  value = "Not applicable.",
  examples = r"-(
req <- request("openai:gpt-5-mini", list(message_user("Say hello.")),
               config = config(max_tokens = 50L))
client <- fake_lm(list("Hello."))       # a scripted stand-in; no network
response_text(complete(client, req))
)-"),

page("request", "Build a request and its messages",
  c("request", "message_user", "message_assistant", "message_developer", "message_tool", "new_message"),
  description = r"-(
A request is a model, a list of messages, and optionally a system prompt, tools
and settings. Messages hold parts (text, images, tool calls, data, ...).
\code{message_user()} and friends take a string, a part, or a list of strings
and parts; a character vector is several text parts in one message.
)-",
  args = c(messages = "A message, or a list of messages from \\code{message_user()}, \\code{message_assistant()}, \\code{message_tool()} or \\code{new_message()}.",
    model = "The model: \\code{\"provider:model\"} (for example \\code{\"anthropic:claude-haiku-4-5\"}) or a bare model name a router can route.",
    system = "A system prompt: a non-empty string or a list of prompt parts, or \\code{NULL}.",
    tools = "A list of tools from \\code{function_tool()} or \\code{builtin_tool()}.",
    config = "Settings from \\code{config()}.",
    is_error = "Whether the tool failed; the provider is told so.",
    name = "The tool's name, needed by some providers when the result is sent back; \\code{NULL} lets lm15 find it from the matching call.",
    role = "One of \\code{\"user\"}, \\code{\"assistant\"}, \\code{\"tool\"}, \\code{\"developer\"}.",
    parts = "A list of content parts.",
    id = "The id of the tool call this result answers (\\code{tool_calls(response)[[i]]$id})."),
  details = r"-(
Requests are values: changing a field returns a new, validated request. A
request that breaks a contract rule (a tool result in a user message, an empty
message list, duplicate tool names, ...) is refused when it is built.
\code{new_message()} is the general constructor; it is named with the
package's \code{new_} prefix because \code{message()} would mask
\code{base::message()}.
)-",
  value = "\\code{request()} returns a Request; the others return a Message.",
  seealso = "\\code{\\link{content-parts}}, \\code{\\link{config}}, \\code{\\link{complete}}",
  examples = r"-(
req <- request("openai:gpt-5-mini",
               list(message_user("What is 2 + 2?")),
               system = "Answer with a number.")
# Continue a conversation: append the assistant's message, then yours.
answer <- complete(fake_lm(list("4")), req)
req$messages <- c(req$messages, list(answer$message, message_user("And 3 + 3?")))
length(req$messages)
)-"),

page("content-parts", "Content parts",
  c("text_part", "thinking_part", "thinking", "refusal_part", "refusal", "citation_part", "data_part", "image_part", "audio_part", "video_part",
    "document_part", "binary_part", "tool_call_part", "tool_result_part", "continuation_state", "continuation_data", "media_bytes"),
  description = r"-(
The pieces a message is made of. Media parts take exactly one of \code{data},
\code{url}, \code{file_id} or \code{path} (a local file read when the request is
sent). A data part carries any JSON value: as input it is structured data, and
in an assistant message it is the answer to a judgment request
(\code{\link{judgments}()}).
)-",
  args = c(title = "A title.",
    detail = "Image detail for providers that take it: \\code{\"low\"}, \\code{\"high\"} or \\code{\"auto\"}.",
    path = "A local file, read when the request is sent.",
    value = "Any JSON value; \\code{NULL} is JSON null.",
    probabilities = "For an answer only: a JSON object mapping each judgment to a distribution over its keys, or \\code{NULL}.",
    method = "How \\code{probabilities} were measured: \\code{\"provider_classification\"} or \\code{\"candidate_sequence_likelihood\"}; present exactly when \\code{probabilities} is.",
    id = "The tool call's id (for a result: the id of the call it answers).",
    name = "The tool's name.",
    is_error = "Whether the tool failed.",
    content = "The result: a string, a part, or a list of parts (text, media, data).",
    kind = "The continuation kind, such as \\code{\"thought_signature\"}.",
    data = "Base64 data or raw bytes; for \\code{continuation_state()}, a JSON object.",
    part = "A media part.",
    provider = "The dialect the state belongs to, such as \\code{\"anthropic\"}.",
    x = "A message, a part, or a list of continuation states."),
  details = "\\code{thinking()} and \\code{refusal()} are shorter spellings of \\code{thinking_part()} and \\code{refusal_part()}. \\code{continuation_data()} returns the \\code{data} of the first matching state. \\code{media_bytes()} returns a part's bytes (decoded data or the file's contents).",
  value = "A part; \\code{continuation_data()} returns a JSON object or \\code{NULL}; \\code{media_bytes()} returns a raw vector.",
  examples = r"-(
photo <- image_part(data = as.raw(c(0x89, 0x50, 0x4e, 0x47)), media_type = "image/png")
msg <- message_user(list("What is in this picture?", photo))
as_json(data_part(json_object(city = "Oslo", temperature = 12L)))
)-"),

page("config", "Settings and tools",
  c("config", "reasoning", "tool_choice", "cache_config", "function_tool", "builtin_tool"),
  description = r"-(
\code{config()} holds generation settings. Leave a setting \code{NULL} to use the
provider's default. A setting the provider's wire cannot take is adapted and
recorded, or refused, never silently dropped (see \code{\link{plan}()}).
)-",
  args = c(max_tokens = "The most tokens to generate.", temperature = "Sampling temperature, 0 or more.",
    top_p = "Nucleus sampling, between 0 and 1.", top_k = "Top-k sampling.", seed = "A sampling seed.",
    frequency_penalty = "Between -2 and 2.", presence_penalty = "Between -2 and 2.",
    stop = "A list of stop sequences.",
    response_format = "\\code{json_object(type = \"json_object\")} for any JSON, or a JSON Schema format such as \\code{judgments()} returns.",
    tool_choice = "A \\code{tool_choice()} value.", reasoning = "A \\code{reasoning()} value.",
    cache = "A \\code{cache_config()} value.",
    service_tier = "The provider's service tier word, such as \\code{\"flex\"}.",
    user_id = "An opaque end-user identifier for the provider's abuse attribution.",
    store = "Whether the provider may store the response (\\code{FALSE} opts out).",
    logprobs = "Ask for token log-probabilities: 0 for the chosen tokens, n for n alternatives too.",
    probabilities = "For judgment requests: \\code{\"off\"}, \\code{\"if_available\"} or \\code{\"required\"}; see \\code{\\link{judgments}}.",
    effort = "One of \\code{\"off\"}, \\code{\"minimal\"}, \\code{\"low\"}, \\code{\"medium\"}, \\code{\"high\"}, \\code{\"xhigh\"}, \\code{\"max\"}.",
    thinking_budget = "A reasoning token budget for providers that take one.",
    summary = "Show reasoning: \\code{\"auto\"}, \\code{\"concise\"} or \\code{\"detailed\"}.",
    mode = "For \\code{tool_choice()}: \\code{\"auto\"}, \\code{\"required\"} or \\code{\"none\"}; for \\code{cache_config()}: \\code{\"auto\"} or \\code{\"off\"}.",
    allowed = "A list of tool names the model may choose from.",
    parallel = "Whether several tool calls may come at once, or \\code{NULL} for the default.",
    retention = "Cache lifetime: \\code{\"short\"} or \\code{\"long\"}.",
    key = "A cache key.", prefix_until_index = "Cache the messages up to this index (0-based).",
    prefix = "\\code{\"stable\"} or \\code{\"history\"}: which prefix to cache.",
    resource = "The id of a stored cache to read (see \\code{\\link{cache}}).",
    name = "The tool's name.", description = "What the tool does, for the model.",
    parameters = "The tool's arguments as a JSON Schema object; the default takes none.",
    config = "For \\code{builtin_tool()}: the provider's own settings as a JSON object."),
  details = "Nothing derives a tool from an R function: write the schema. \\code{builtin_tool()} names a provider-hosted tool such as \\code{\"web_search\"}.",
  value = "A Config, Reasoning, ToolChoice, CacheConfig, FunctionTool or BuiltinTool value.",
  examples = r"-(
weather <- function_tool("weather", description = "Current weather in a city.",
  parameters = json_object(type = "object",
    properties = json_object(city = json_object(type = "string")),
    required = json_array("city")))
cfg <- config(max_tokens = 200L, temperature = 0.2, reasoning = reasoning("low"),
              tool_choice = tool_choice(mode = "auto"))
req <- request("anthropic:claude-haiku-4-5", list(message_user("Weather in Oslo?")),
               tools = list(weather), config = cfg)
)-"),

page("new_router", "Clients, routers and transports",
  c("new_router", "new_lm", "providers", "canonical_provider", "resolve", "router_lm", "transport_curl"),
  description = r"-(
A router picks the provider from the model string and builds a client for it
on first use. A client talks to one provider. Constructing either reads no file
and contacts no server; credentials are resolved when a request is sent.
)-",
  args = c(api_keys = "Explicit credentials by provider: \\code{list(openai = \"sk-...\")}. A string is an API key; \\code{bearer_token()} and credential functions are also accepted. An explicit entry wins over everything else.",
    base_urls = "Address overrides by provider.",
    settings = "Settings by provider (for new_router), or for this client (for new_lm): a cloud provider's host settings, such as \\code{list(region = \"us-east-1\")}, or a subscription provider's \\code{client_version} (the Claude Code or Codex release it names; \\code{LM15_CLAUDE_CODE_VERSION} and \\code{LM15_CODEX_CLIENT_VERSION} when a router reads the environment). A setting the provider does not read is an error.",
    catalog = "Model metadata used to route bare model names: a list of \\code{model_info()} values or a registry from \\code{new_model_registry()}.",
    rules = "Routing rules as a list of \\code{c(prefix, provider)}; \\code{NULL} uses the built-in ones (\\code{claude-} to Anthropic, \\code{gpt-} to OpenAI, ...).",
    live_connect = "A function that opens live sessions, replacing the native WebSocket connector.",
    adaptations = "What to do when the wire cannot take a setting: \\code{\"note\"} (adapt and record it on the response), \\code{\"silent\"} (adapt, record nothing) or \\code{\"refuse\"} (raise instead).",
    auth = "A sign-in scope (\\code{local_auth()}, ...) whose saved connections this router uses, or \\code{NULL}. With a scope, environment keys are never used.",
    credentials = "Named cloud identities by provider: \\code{list(bedrock_chat = \"cli\")}. One of \\code{\"platform\"}, \\code{\"workload\"}, \\code{\"environment\"}, \\code{\"cli\"}.",
    api_key = "The credential: a key string, \\code{api_key()}, \\code{bearer_token()}, \\code{aws_credentials()}, or a function returning one (called for every request). \\code{NULL} uses the provider's declared chain.",
    credential = "A named cloud identity (\\code{\"platform\"}, \\code{\"workload\"}, \\code{\"environment\"} or \\code{\"cli\"}) instead of walking the cloud chain.",
    base_url = "The server's address, overriding the provider's.",
    compat = "For servers speaking another provider's wire (Chat Completions, Responses, Messages): a preset name such as \\code{\"ollama\"} or \\code{\"groq\"}, or a policy list.",
    account_id = "The ChatGPT account id for \\code{openai-codex}, when it cannot be read from the token.",
    clock = "A function returning the current time; for tests.",
    credentials_path = "Where the subscription login is stored, for providers that use one.",
    router = "A router from \\code{new_router()}.",
    name = "A provider name.",
    model = "A model string.",
    timeout = "Seconds for the whole transfer.", connect_timeout = "Seconds to connect.",
    max_response_bytes = "The largest reply accepted."),
  details = r"-(
Credentials, in order: an explicit \code{api_keys} entry or \code{api_key};
then, by provider, a subscription login (xAI, Claude Code, Codex), the
provider's environment variables (\code{OPENAI_API_KEY}, ...), or the cloud's
own chain (Azure, AWS, Google Cloud). A failed or signed-out subscription is
never replaced by a paid key on its own. \code{\link{explain_auth}()} shows
the walk.

\code{resolve()} says which provider and model a string routes to, without
sending anything. \code{router_lm()} returns the client a router would use.
The curl transport never follows redirects and never retries.
)-",
  value = "A router, a client, a transport function, a character vector of provider ids, or for \\code{resolve()} a list with \\code{provider}, \\code{model} and \\code{source}.",
  examples = r"-(
router <- new_router(api_keys = list(openai = "sk-example"))
resolve(router, "gpt-5-mini")
resolve(router, "groq:llama-3.3-70b-versatile")
head(providers())
local <- new_lm("ollama")  # a keyless local server at its default address
)-"),

page("complete", "Send a request",
  c("complete", "stream", "plan", "adaptation", "build_request", "parse_response", "replay_stream", "materialize_response", "response_to_events"),
  description = r"-(
\code{complete()} sends a request and returns the response. \code{stream()}
calls \code{on_event} with each event as it arrives and returns the assembled
response at the end. \code{plan()} says, without sending anything, what the
provider's wire would adapt or refuse.
)-",
  args = c(lm = "A client from \\code{new_lm()}, a router from \\code{new_router()}, a bound client from \\code{connect()} or \\code{bind_model()}, or any object with a method.",
    request = "A request from \\code{request()}. A bound client also takes messages or a string.",
    "..." = "Arguments for methods. The package's own clients and routers take none; a bound client takes \\code{system}, \\code{tools} and \\code{config} when \\code{request} is messages.",
    stream = "Whether to build or plan the streaming form.",
    field = "The request path that was adapted, such as \\code{\"config.top_k\"}.",
    action = "What was done: \\code{\"dropped\"}, \\code{\"clamped\"}, \\code{\"substituted\"}, \\code{\"client_side\"}, \\code{\"satisfied\"} or \\code{\"defaulted\"}.",
    reason = "Why, in words.", asked = "What was asked for, or \\code{NULL}.", applied = "What was applied instead, or \\code{NULL}.",
    body = "A recorded reply body (JSON text, raw bytes, or for streams the SSE text).",
    status = "The reply's HTTP status.", on_event = "For \\code{stream()}: a function called with each event. For \\code{response_to_events()}: an optional function called with each event."),
  details = r"-(
Nothing is retried: a request that fails raises its error once. Interrupting a
stream releases the connection; it cannot promise that the provider stopped
generating or billing. \code{build_request()} returns the exact HTTP request
(credentials included: do not log it); \code{parse_response()} and
\code{replay_stream()} turn recorded replies into responses without a network.
\code{response_to_events()} turns a response into the events a stream would
have produced.
)-",
  value = "\\code{complete()}, \\code{stream()}, \\code{parse_response()} and \\code{materialize_response()} return a Response; \\code{plan()} a list of Adaptation values; \\code{build_request()} a wire request; \\code{replay_stream()} a list of \\code{events} and \\code{response}; \\code{response_to_events()} a list of events.",
  examples = r"-(
req <- request("openai:gpt-5-mini", list(message_user("Hi")),
               config = config(top_k = 40L, max_tokens = 20L))
plan(new_router(), req)      # OpenAI's wire has no top_k: dropped, and said so
client <- fake_lm(list("Hello!", "Hello again!"))
response_text(complete(client, req))
invisible(stream(client, req, function(event) cat(event$type, "")))
)-"),

page("response", "Read a response",
  c("response", "usage", "response_text", "tool_calls", "citations", "parse_json", "token_logprob", "top_logprob", "tool_call_info"),
  description = r"-(
A response holds the assistant's message, the finish reason, token usage, and
any adaptations made to the request. Usage counts the provider did not report
are \code{NULL}, never zero.
)-",
  args = c(message = "The assistant's message.",
    finish_reason = "Why generation stopped: \\code{\"stop\"}, \\code{\"length\"}, \\code{\"tool_call\"}, \\code{\"content_filter\"} or \\code{\"error\"}.",
    logprobs = "Token log-probabilities, a list of \\code{token_logprob()} values.",
    logprobs_complete = "\\code{FALSE} when local editing left text without its original scores.",
    adaptations = "A list of \\code{adaptation()} values.",
    input_tokens = "Tokens read.", output_tokens = "Tokens written.", total_tokens = "The provider's total, or input plus output.",
    cache_read_tokens = "Tokens read from cache.", cache_write_tokens = "Tokens written to cache.",
    reasoning_tokens = "Reasoning tokens, when counted separately.", input_audio_tokens = "Audio tokens read.", output_audio_tokens = "Audio tokens written.",
    token = "The token's text.", logprob = "Its log-probability.", bytes = "Its UTF-8 bytes, as a list of integers.",
    token_id = "The token's id, or \\code{NULL}.", top = "Alternatives, a list of \\code{top_logprob()} values.",
    id = "An id.", name = "The tool's name."),
  details = "\\code{response_text()} joins the text parts, or returns \\code{NULL} when the answer is not plain text. \\code{parse_json()} parses it as JSON (numbers keep their spelling).",
  value = "A Response or Usage value; \\code{response_text()} a string or \\code{NULL}; \\code{tool_calls()} and \\code{citations()} lists of parts; \\code{parse_json()} a JSON value.",
  examples = r"-(
answer <- complete(fake_lm(list("A list column holds a list in each row.")),
                   request("m", list(message_user("What is a list column?"))))
response_text(answer)
answer$finish_reason
answer$usage$input_tokens   # NULL: the stand-in reported no usage
)-"),

page("stream-events", "Stream events",
  c("stream_start_event", "stream_delta_event", "stream_end_event", "stream_error_event", "text_delta", "thinking_delta",
    "tool_call_delta", "image_delta", "audio_delta", "citation_delta", "continuation_delta", "error_detail"),
  description = "The events \\code{stream()} delivers: one start, deltas, and one end (or an error). Deltas carry a fragment and the index of the part it belongs to.",
  args = c(delta = "A delta.", finish_reason = "Why generation stopped, or \\code{NULL}.",
    usage = "Token counts, or \\code{NULL}.", adaptations = "Adaptations made to the request, sent with the start event.",
    logprobs = "Token log-probabilities for this fragment.", logprobs_complete = "\\code{FALSE} when local editing left text without scores.",
    id = "An id, or \\code{NULL}.", name = "A tool name, or \\code{NULL}.",
    title = "A citation title.", provider = "The dialect the state belongs to.", kind = "The continuation kind.",
    data = "For media deltas: base64 data; for continuation deltas: a JSON object.",
    code = "The error code, such as \\code{\"rate_limit\"}.",
    message = "The error message.", provider_code = "The provider's own error code, or \\code{NULL}.",
    http_response = "Diagnostics from the HTTP handshake (\\code{request_id}, \\code{retry_after}, \\code{rate_limit_headers}), or \\code{NULL}.",
    part_index = "The index of the part this fragment belongs to; for continuation deltas \\code{NULL} means the whole message."),
  value = "A stream event, delta or ErrorDetail value.",
  examples = r"-(
events <- list(stream_start_event(model = "m"),
               stream_delta_event(text_delta("Hel")),
               stream_delta_event(text_delta("lo")),
               stream_end_event(finish_reason = "stop"))
response_text(materialize_response(events, request("m", list(message_user("hi")))))
)-"),

page("judgments", "Judgments: declared answers with probabilities",
  c("judgments", "choice", "yes_no", "score", "judgments_in_schema", "request_judgments", "response_data", "response_probabilities",
    "response_method", "response_expected", "expected_level"),
  description = r"-(
A judgment is a question whose answer is one of the keys you declare: yes or
no, a choice among labels, or an ordered score. \code{judgments()} writes the
JSON Schema response format; the answer comes back as a data part holding the
model's JSON, and, where the provider can measure it, a probability for every
key.
)-",
  args = c("..." = "For \\code{judgments()}: the judgment properties, named (from \\code{yes_no()}, \\code{choice()}, \\code{score()} or written by hand). Elsewhere unused.",
    name = "The response format's name.", strict = "Whether the provider must follow the schema strictly.",
    instruction = "The question, in words.",
    options = "The choices: a character vector of keys, or a named vector of descriptions (\\code{NA} for none).",
    levels = "The ordered levels, lowest first: a character vector of descriptions, optionally named with a short title. At least two, at most ten.",
    schema = "A JSON Schema object.", field = "The name of an ordered judgment.",
    distribution = "A named list of level to probability."),
  details = r"-(
\code{config(probabilities = )} says what to do when the provider cannot
measure a distribution: \code{"off"} asks for nothing; \code{"if_available"}
answers with the pick only and records that probabilities were dropped;
\code{"required"} refuses before sending. TypeSafe's \code{jev-} models return
their own classification; a vLLM server scores every key from token
log-probabilities (one batched call); other providers return the pick only.
\code{response_data()} reads the answer (or the parsed JSON text of a plain
structured-output reply); \code{response_expected()} is the probability-weighted
level of an ordered judgment. No number the model wrote in prose is ever read
as a probability.
)-",
  value = "\\code{judgments()} returns a response format; \\code{choice()}, \\code{yes_no()}, \\code{score()} a schema property; the readers return JSON values, a distribution, a method string or a number, or \\code{NULL}.",
  examples = r"-(
wine <- judgments(
  quality = score("How good is this wine?", c(poor = "Faulty", ok = "Sound", great = "Superb")),
  style = choice("Which style?", c("fruit", "oak", "mineral")),
  ageing = yes_no("Will it improve with age?"))
req <- request("jev-latest", list(message_user("Ripe cassis, firm tannins, long finish.")),
               config = config(response_format = wine, probabilities = "if_available"))
names(request_judgments(req))
)-"),

page("errors", "Errors", c("lm15_error", "normalize_error", "retryable"),
  description = r"-(
lm15 signals conditions with the contract's classes, all inheriting from
\code{LM15Error}: \code{AuthError}, \code{BillingError}, \code{RateLimitError},
\code{InvalidRequestError}, \code{ContextLengthError}, \code{TimeoutError},
\code{ServerError}, \code{UnsupportedModelError}, \code{UnsupportedFeatureError},
\code{NotConfiguredError}, \code{UnknownModelError}, \code{AmbiguousModelError},
\code{TransportError}, \code{LockTimeoutError}, \code{StreamAssemblyError},
\code{CollectionLimitError}, \code{AuthOperationError} and \code{ProviderError}.
Catch them with \code{tryCatch()} by class.
)-",
  args = c(message = "The message.", code = "The contract error code, such as \\code{\"rate_limit\"}.",
    provider_code = "The provider's own code.", status = "The HTTP status.", request_id = "The provider's request id.",
    retry_after = "The provider's wait advice in seconds.", partial = "A partial response, when one exists.",
    part_index = "The part a stream error concerns.", model = "The model string that failed to route.",
    providers = "The providers a model string matched.", credential_hint = "How to fix the credential, shown under the message.",
    path = "The credentials file.", lock_path = "The lock file.", feature = "The request path of a refused feature.",
    rate_limit_headers = "Rate-limit headers the provider sent, kept verbatim.",
    body = "The reply body.", error = "A condition."),
  details = r"-(
\code{retryable()} is \code{TRUE} for rate limits, timeouts, server errors,
transport failures and lock contention. lm15 never retries by itself; a retry
loop is yours to write, with \code{error$retry_after} as advice.
\code{AuthOperationError} (sign-in lifecycle) carries \code{reason},
\code{stage}, \code{commit_state} and \code{recovery}.
)-",
  value = "A condition object; \\code{retryable()} returns \\code{TRUE} or \\code{FALSE}.",
  examples = r"-(
client <- fake_lm(list(lm15_error("Slow down.", code = "rate_limit", retry_after = 2)))
err <- tryCatch(complete(client, request("m", list(message_user("hi")))),
                RateLimitError = identity)
retryable(err)
err$retry_after
)-"),

page("json", "Canonical JSON", c("as_json", "from_json", "as_dict", "from_dict", "json_object", "json_array", "validate", "integer_value"),
  description = r"-(
Every canonical value converts to and from the contract's JSON exactly, in
every lm15 language. Empty objects and empty arrays stay distinct
(\code{json_object()}, \code{json_array()}), numbers keep their spelling, and
large integers are exact.
)-",
  args = c(include_provider_data = "Whether to include the provider's raw reply fields.",
    text = "A JSON string.", value = "A parsed JSON object.",
    kind = "The canonical kind, such as \\code{\"request\"}, \\code{\"response\"}, \\code{\"message\"}, \\code{\"part\"} or \\code{\"config\"}.",
    "..." = "For \\code{json_object()} and \\code{json_array()}: the members. Elsewhere unused.",
    decimal = "An integer written in decimal, such as \\code{\"9007199254740993\"}."),
  details = "Named lists are objects, unnamed lists are arrays; \\code{NULL} means absent, and zero and \\code{FALSE} are data. \\code{validate()} re-checks a value; assigning a field with \\code{$<-} also validates.",
  value = "\\code{as_json()} returns a string; \\code{as_dict()} a JSON object; \\code{from_json()} and \\code{from_dict()} a canonical value; \\code{integer_value()} an exact integer.",
  examples = r"-(
req <- request("openai:gpt-5-mini", list(message_user("hi")))
json <- as_json(req)
json
identical(as_json(from_json(json, "request")), json)
as_json(json_object(empty_object = json_object(), empty_array = json_array()))
)-"),

page("openai-chat", "Code written for the OpenAI Chat Completions format",
  c("request_from_openai_chat", "response_from_openai_chat", "complete_from_openai_chat", "stream_from_openai_chat", "route_openai_chat",
    "resolve_openai_chat", "openai_chat_model_string"),
  description = r"-(
For code that already builds OpenAI Chat Completions bodies (or litellm
calls): read such a body into a canonical request, or send it through a router
to any provider. Every field is mapped, passed through as an extension, or
refused by name; nothing is dropped silently.
)-",
  args = c(body = "A Chat Completions request body as a JSON object (or JSON text); for \\code{response_from_openai_chat()}, a response body.",
    compat = "The server dialect the body was written for, such as \\code{\"groq\"}.",
    provider = "The provider the body was written for.",
    model = "The model, when the body carries none; for model-string functions, the model string (\\code{provider:model} or litellm's \\code{provider/model}).",
    choice = "Which choice to read when the body has several.",
    streaming = "Whether to stream; by default the body's \\code{stream} field.",
    on_event = "A function called with each event while streaming.",
    router = "A router from \\code{new_router()}.", lm = "A router from \\code{new_router()} or a client from \\code{new_lm()}."),
  value = "A Request, a Response, a routing list, or a model string.",
  examples = r"-(
body <- json_object(model = "gpt-5-mini",
  messages = json_array(json_object(role = "user", content = "Hello")),
  max_tokens = 50L)
req <- request_from_openai_chat(body)
as_json(req)
openai_chat_model_string("groq/llama-3.3-70b-versatile")
)-"),

page("models", "Model listings and metadata",
  c("list_models", "build_models_request", "parse_models_response", "model_info", "model_origin", "inference_model_info", "inference_pricing",
    "new_model_registry", "discover_model_registry", "estimate_cost"),
  description = "List a provider's models, describe models with metadata, and collect metadata in a registry a router can use to route bare model names. Metadata is advisory: it never changes how a request is built.",
  args = c(provider = "The provider id.", api_family = "The wire family, such as \\code{\"openai_chat\"}.",
    aliases = "Other names for the model.", origin = "Where the model comes from, a \\code{model_origin()} value.",
    inference = "An \\code{inference_model_info()} value, or \\code{NULL}.",
    type = "\\code{\"provider\"} or another origin type.", base_model = "The model this one derives from.",
    input_modalities = "What the model reads, such as \\code{list(\"text\", \"image\")}.",
    output_modalities = "What it writes.", context_window = "Its context window in tokens.",
    max_output_tokens = "Its largest output.", supports_reasoning = "Whether it reasons.",
    reasoning_efforts = "The effort words it accepts.", pricing = "An \\code{inference_pricing()} value.",
    input_per_million = "Price per million input tokens.", output_per_million = "Price per million output tokens.",
    cache_read_per_million = "Price per million cached tokens read.", cache_write_per_million = "Price per million tokens written to cache.",
    currency = "The currency code.", dimensions = "Other price dimensions, as a JSON object.",
    models = "A list of \\code{model_info()} values.",
    catalogs = "Files to read (JSON catalogs), or \\code{NULL} to discover them in installed packages.",
    libraries = "Library paths to search for packages that ship \\code{lm15/model-catalog.json}.",
    usage = "Token counts, from a response."),
  value = "A list of ModelInfo values, a wire request, a ModelInfo/ModelOrigin/pricing value, a registry, or an estimated cost (a number, or \\code{NULL} when unknown).",
  examples = r"-(
gpt <- model_info("gpt-5-mini", "openai", "openai_responses",
  inference = inference_model_info(pricing = inference_pricing(input_per_million = 0.25,
                                                              output_per_million = 2)))
registry <- new_model_registry(list(gpt))
estimate_cost(gpt$inference$pricing, usage(input_tokens = 1000L, output_tokens = 500L))
resolve(new_router(catalog = registry), "gpt-5-mini")
)-"),

page("files", "Provider files",
  c("file_upload", "file_get", "file_list", "file_delete", "file_download", "file_op_build", "file_op_parse", "file_upload_request", "file_info", "file_page"),
  description = "Upload, list, fetch and delete files stored by a provider, for use in requests by \\code{file_id}.",
  args = c(request = "A \\code{file_upload_request()} value.", filename = "The file's name.",
    bytes_data = "The file's bytes, or \\code{NULL} when \\code{path} is given.", path = "A local file to upload.",
    action = "\\code{\"upload\"}, \\code{\"get\"}, \\code{\"list\"}, \\code{\"delete\"} or \\code{\"download\"}.",
    upload_request = "A \\code{file_upload_request()} for uploads.",
    page = "Whether the body is a listing page.", size_bytes = "The file's size.",
    readiness = "\\code{\"pending\"}, \\code{\"ready\"} or \\code{\"failed\"}.", downloadable = "Whether its bytes can be downloaded.",
    items = "A list of values on this page.", next_cursor = "The cursor of the next page, or \\code{NULL}.",
    file_id = "A file id."),
  details = "\\code{file_op_build()} and \\code{file_op_parse()} are the pure halves: they build the HTTP request and read the reply, without networking.",
  value = "A FileInfo, a FilePage, raw bytes for a download, a wire request, or invisible \\code{NULL} for a deletion.",
  examples = r"-(
up <- file_upload_request("notes.txt", bytes_data = charToRaw("hello"), media_type = "text/plain")
wire <- file_op_build(new_lm("openai", api_key = "sk-example"), "upload", upload_request = up)
wire$method
)-"),

page("cache", "Stored prompt caches",
  c("cache", "cache_create", "cache_get", "cache_list", "cache_update", "cache_delete", "cache_op_build", "cached_request", "cached_prefix", "cache_info", "cache_page"),
  description = "Create a provider-side cache of a request prefix (Gemini's cached contents; automatic or marker caching elsewhere) and send later requests that reuse it.",
  args = c(prefix = "A request whose messages are the prefix to cache (its settings must be the defaults).",
    ttl_seconds = "The cache's lifetime in seconds.", cached = "A \\code{cached_prefix()} from \\code{cache()}.",
    messages = "The messages that follow the cached prefix.", config = "Settings for the new request.",
    resource = "The stored cache, a \\code{cache_info()} value.", provider = "The provider the cache belongs to, when a router created it.",
    tokens = "Tokens in the cache.", items = "A list of values on this page.", next_cursor = "The cursor of the next page, or \\code{NULL}.",
    action = "\\code{\"create\"}, \\code{\"get\"}, \\code{\"list\"}, \\code{\"update\"} or \\code{\"delete\"}."),
  value = "A CachedPrefix, CacheInfo, CachePage, Request, wire request, or invisible \\code{NULL}.",
  examples = r"-(
prefix <- request("gemini-2.5-flash", list(message_user("A long document ...")))
cached <- cached_prefix(prefix)   # no stored resource: the request is sent whole
req <- cached_request(cached, list(message_user("Summarise it.")))
length(req$messages)
)-"),

page("batch", "Batches",
  c("batch", "batch_job", "batches", "batch_submit", "batch_status", "batch_results", "batch_cancel", "batch_list", "batch_op_build", "batch_op_parse",
    "batch_request", "batch_entry", "batch_job_info"),
  description = "Submit many requests as one provider batch (cheaper, slower) and fetch the results later. \\code{batch()} returns a job handle; see \\code{\\link{wait}}.",
  args = c(request = "A \\code{batch_request()} value.", requests = "A list of requests.",
    model = "The model all requests use; defaults to the first request's.",
    action = "\\code{\"submit\"}, \\code{\"status\"}, \\code{\"results\"}, \\code{\"cancel\"} or \\code{\"list\"}.",
    upload_body = "The reply of the upload step, for providers that upload first.",
    kind = "\\code{\"job\"}, \\code{\"list\"} or \\code{\"results\"}.",
    index = "The request's position (0-based).", outcome = "\\code{\"succeeded\"}, \\code{\"errored\"}, \\code{\"cancelled\"} or \\code{\"expired\"}.",
    response = "The response of a succeeded entry.", error = "The error of an errored entry.",
    status = "\\code{\"queued\"}, \\code{\"running\"}, \\code{\"cancelling\"}, \\code{\"completed\"}, \\code{\"failed\"}, \\code{\"cancelled\"} or \\code{\"expired\"}."),
  value = "A job handle, a BatchJobInfo, a list of BatchEntry values or jobs, or a wire request.",
  examples = r"-(
reqs <- lapply(c("red", "green"), function(colour)
  request("openai:gpt-5-mini", list(message_user(paste("A fruit that is", colour)))))
b <- batch_request(reqs, label = "fruit")
b$model
)-"),

page("video", "Video generation", c("video_generate", "video_job", "video_jobs", "video_submit", "video_status", "video_result", "video_list", "video_op_build", "video_op_parse", "video_generation_request", "video_job_info"),
  description = "Video is a job: submission returns a ticket. \\code{video_generate()} returns a handle; \\code{wait()} polls it and \\code{result()} fetches the video.",
  args = c(request = "A \\code{video_generation_request()} value.", prompt = "What to generate.",
    seconds = "The length in seconds.", images = "Input frames, a list of image parts.",
    action = "\\code{\"submit\"}, \\code{\"status\"}, \\code{\"result\"} or \\code{\"list\"}.",
    kind = "\\code{\"job\"}, \\code{\"list\"} or \\code{\"result\"}.", fetched = "The downloaded video, when already fetched.",
    status = "\\code{\"queued\"}, \\code{\"running\"}, \\code{\"completed\"}, \\code{\"failed\"} or \\code{\"cancelled\"}.",
    progress = "Percent done.", model = "A model, or \\code{NULL} for all."),
  value = "A job handle, a VideoJobInfo, a video part, a list, or a wire request.",
  examples = r"-(
v <- video_generation_request("sora-2", "A paper boat on a rainy street", seconds = 4L)
wire <- video_op_build(new_lm("openai", api_key = "sk-example"), "submit", request = v)
wire[[1]]$method
)-"),

page("wait", "Job handles", c("job_info", "refresh", "wait", "result", "results", "cancel"),
  description = "A handle holds a snapshot of a batch or video job. Reading it never contacts the provider; \\code{refresh()} fetches a new snapshot; only \\code{wait()} polls.",
  args = c(poll_every = "Seconds between polls.", timeout = "Seconds to wait before giving up (an error of class \\code{lm15_wait_timeout}).",
    cancelled = "A function returning \\code{TRUE} to stop waiting."),
  details = "A wait's deadline bounds polling, not an HTTP request already running. \\code{cancel()} asks the provider to cancel a batch.",
  value = "\\code{job_info()} returns the snapshot; \\code{refresh()}, \\code{wait()} and \\code{cancel()} the updated handle; \\code{result()} and \\code{results()} the output.",
  examples = r"-(
\dontrun{
job <- batch(new_lm("openai"), batch_request(list(request("gpt-5-mini", list(message_user("hi"))))))
job <- wait(job, poll_every = 30, timeout = 3600)
results(job)
}
)-"),

page("generation", "Image and speech generation",
  c("image_generate", "speech_generate", "generation_build", "generation_parse", "image_generation_request", "image_generation_response",
    "speech_generation_request", "speech_generation_response"),
  description = "Generate images and speech on providers that sell them. The build and parse halves work without a network.",
  args = c(request = "An \\code{image_generation_request()} or \\code{speech_generation_request()} value.",
    prompt = "What to generate or say.", size = "The image size, such as \\code{\"1024x1024\"}.",
    images = "For requests: images to edit; for responses: the generated images.", voice = "The voice.",
    format = "The audio format, such as \\code{\"mp3\"}.", audio = "The generated audio part.",
    text = "Text the provider returned with the images."),
  value = "An ImageGenerationResponse or SpeechGenerationResponse, a request value, or a wire request.",
  examples = r"-(
img <- image_generation_request("gpt-image-1", "A lighthouse at dusk", size = "1024x1024")
wire <- generation_build(new_lm("openai", api_key = "sk-example"), img)
wire$method
)-"),

page("live", "Live sessions",
  c("live", "live_config", "audio_format", "turn", "materialize_turn", "build_live_completion", "live_connection_request", "live_setup_frames",
    "live_encode", "live_decode", "websocket_connect"),
  description = r"-(
A live session is a two-way WebSocket conversation (OpenAI Realtime, Gemini
Live): send text, audio or images, and read events as they arrive.
\code{turn()} opens a view that collects one turn's events up to its boundary.
)-",
  args = c(config = "A \\code{live_config()} value.", model = "The model.", system = "A system prompt.",
    tools = "A list of tools.", voice = "The voice.", input_format = "An \\code{audio_format()} for audio you send.",
    output_format = "An \\code{audio_format()} for audio you receive.",
    encoding = "\\code{\"pcm16\"}, \\code{\"opus\"}, \\code{\"mp3\"} or \\code{\"aac\"}.", sample_rate = "Samples per second.",
    channels = "The number of channels.", connect = "A function that opens the connection (the native WebSocket by default).",
    max_queue = "The most unread events kept.", max_frame_bytes = "The largest frame accepted.",
    max_turn_bytes = "The most bytes one turn may collect.", session = "A session from \\code{live()}.",
    max_events = "The most events a turn may collect.", max_bytes = "The most bytes a turn may collect.",
    frame = "A received frame (JSON text).", event = "A client event.",
    url = "A \\code{ws://} or \\code{wss://} URL.", ca_bundle = "A file of trusted certificates for a private server, or \\code{NULL}.",
    headers = "Handshake headers as a named list."),
  details = "A turn over its budget raises \\code{CollectionLimitError}, keeping the events it accepted. TLS certificates and host names are always verified.",
  value = "A session, a LiveConfig/AudioFormat value, a turn view, a Turn, frames, events, or a connection.",
  examples = r"-(
cfg <- live_config("gpt-realtime", system = "Be brief.")
frames <- live_setup_frames(new_lm("openai", api_key = "sk-example"), cfg)
length(frames)
)-"),

page("live-events", "Live session events",
  c("live_client_turn_event", "live_client_audio_event", "live_client_image_event", "live_client_text_event", "live_client_tool_result_event",
    "live_client_interrupt_event", "live_client_end_audio_event", "live_server_audio_event", "live_server_text_event", "live_server_tool_call_event",
    "live_server_tool_call_delta_event", "live_server_interrupted_event", "live_server_turn_end_event", "live_server_usage_event", "live_server_error_event"),
  description = "Events you send to a live session (\\code{live_client_*}) and events it sends back (\\code{live_server_*}).",
  args = c(parts = "The turn's parts.", turn_complete = "Whether the turn is finished.",
    id = "The tool call's id.", content = "The tool's result parts.", input_delta = "A fragment of the call's JSON arguments.",
    data = "Base64 media."),
  value = "A live event value.",
  examples = r"-(
as_json(live_client_text_event("Hello there"))
)-"),

page("login", "Sign in and manage saved connections",
  c("login", "logout", "status", "connections", "configure", "set_api_key", "cancel_login", "verify", "request_auth", "login_providers", "login_methods"),
  description = r"-(
Sign in to a subscription (xAI, Claude, ChatGPT, GitHub Copilot, Kimi Code,
Meta, OpenRouter) or save a key, a key's environment variable name, a cloud
identity or a local server, once, in a private file every lm15 language
reads. A router given \code{auth = } then uses the saved connection, renewing
it when due.
)-",
  args = c(method = "The login method id (see \\code{login_methods()}); \\code{NULL} asks when several are available.",
    ui = "How to talk to the person: \\code{terminal_ui()} (the default in an interactive session) or your own list of \\code{prompt} and \\code{notify} functions.",
    answers = "Answers to the method's fields, such as \\code{list(name = \"GEMINI_API_KEY\")}.",
    replace = "The id of the saved connection this one replaces. Without it, a provider that already has a connection is refused.",
    lifetime = "Seconds the sign-in may take.", allow_unverified = "Allow methods that exist but have no live receipt yet.",
    target = "A provider id or a connection id.", key = "The API key, saved as typed.",
    pinned = "\\code{c(connection_id, identity_generation)} a request must still match, or \\code{NULL}.",
    method = "The login method id."),
  details = r"-(
A saved connection is never overwritten without \code{replace}. \code{logout()}
forgets it locally (it contacts no provider) and keeps a marker, so a signed-out
subscription is not replaced by an environment key after a restart.
\code{status()} and \code{connections()} read the store only; \code{verify()}
lists models as a check (possibly metered). A renewal that may have spent a
one-use token is reported \code{"indeterminate"} and never retried. Methods
marked unverified need \code{allow_unverified = TRUE}.

The file is \code{LM15_CREDENTIALS_PATH}, else
\code{$XDG_CONFIG_HOME/lm15/credentials.json}, else
\code{~/.config/lm15/credentials.json}. It is written with mode 0600 and
atomically, under a lock shared with the other lm15 languages. It is not
encrypted.
)-",
  value = "A Connection (\\code{login()}, \\code{configure()}, \\code{set_api_key()}), a status, a list of connections, a logout result, \\code{\"cancelled\"}/\\code{\"complete\"}/\\code{\"none\"} for \\code{cancel_login()}, a verification, request authentication, or descriptors.",
  seealso = "\\code{\\link{connect}}, \\code{\\link{local_auth}}, \\code{\\link{explain_auth}}",
  examples = r"-(
auth <- memory_auth()                      # a scope that lives in this R session
key <- set_api_key("openai", "sk-example", auth = auth)
status("openai", auth = auth)$usability
configure("gemini", "env", answers = list(name = "GEMINI_API_KEY"), auth = auth)
length(connections(auth = auth))
logout("openai", auth = auth)
\dontrun{
login("xai")          # a device code to approve in your browser
router <- new_router(auth = local_auth())
}
)-"),

page("local_auth", "Sign-in scopes and stores", c("local_auth", "memory_auth", "new_auth", "file_store", "memory_store"),
  description = "A scope is where saved connections live: the private local file (\\code{local_auth()}), this R session only (\\code{memory_auth()}), or a store you choose (\\code{new_auth()}). Constructing one reads and writes nothing.",
  args = c(path = "The credentials file, or \\code{NULL} for the default.",
    "..." = "For \\code{local_auth()} and \\code{memory_auth()}: arguments passed to \\code{new_auth()}. Elsewhere unused.",
    store = "A store from \\code{file_store()} or \\code{memory_store()}.",
    clock = "A function returning the current time in seconds since the epoch.",
    monotonic = "A function returning a monotonic time in seconds.",
    http = "A function that sends one sign-in HTTP request; for tests. \\code{NULL} uses curl.",
    sleep = "A function that waits a number of seconds; for tests."),
  value = "An Auth scope, or a store.",
  examples = r"-(
auth <- new_auth(file_store(file.path(tempdir(), "credentials.json")))
auth
)-"),

page("connect", "Connect interactively and bind a model",
  c("connect", "bind_model", "model_choices", "terminal_ui"),
  description = r"-(
\code{connect()} asks which saved connection or provider to use, signs in or
saves a key if needed, lists the account's models, and returns a client bound
to that connection and model. It never sends a prompt. Without an interactive
session and without \code{ui}, it stops before reading anything.
)-",
  args = c(provider = "A provider id to skip the provider question, or \\code{NULL}.",
    model = "A model id to skip the model question, or \\code{NULL}.",
    ui = "A UI; \\code{NULL} uses \\code{terminal_ui()} in an interactive session.",
    capability = "Offer only models known to support \\code{\"reasoning\"}, \\code{\"vision\"} or \\code{\"structured-output\"}.",
    open_browser = "Open sign-in pages in the browser (https only).",
    allow_unverified = "Offer methods that have no live receipt yet, labelled as such.",
    adaptations = "As for \\code{new_router()}.",
    refresh = "Fetch the account's own model list now (\\code{TRUE}) or read \\code{registry}.",
    include_unknown = "Also offer models whose support for \\code{capability} is unknown.",
    registry = "A registry from \\code{new_model_registry()}."),
  details = r"-(
A bound client takes \code{complete(client, "text")}, a message, a list of
messages, or a request for its model. It follows its connection's renewals
and nothing else: after a replacement or a logout it stops with
\code{connection_changed} or \code{login_required} rather than switch who pays.
A sign-in completed by \code{connect()} stays saved even if you then cancel the
model question.
)-",
  value = "A bound client, a list of model choices, or a UI.",
  examples = r"-(
auth <- memory_auth()
set_api_key("openai", "sk-example", auth = auth)
client <- bind_model("openai", "gpt-5-mini", auth = auth)
client
client$request("Hello")$model
\dontrun{
client <- connect()           # at a console
response_text(complete(client, "Hello"))
}
)-"),

page("credentials", "Credentials and the authentication doctor",
  c("api_key", "bearer_token", "aws_credentials", "credential_expired", "select_auth_scheme", "explain_auth", "credentials_path", "load_local_credential",
    "credential_store", "write_credentials", "cloud_credential_provider"),
  description = r"-(
Credential values, and \code{explain_auth()}: which credential a request
would use, rung by rung, and where each host setting comes from, without
reading a secret aloud or contacting anything.
)-",
  args = c(value = "The secret, or for \\code{write_credentials()} the whole JSON document.", access_key_id = "The AWS access key id.",
    secret_access_key = "The AWS secret key.", session_token = "The AWS session token, or \\code{NULL}.",
    credential = "A credential value; for \\code{explain_auth()}, a named cloud identity.",
    skew_seconds = "Treat a credential as expired this many seconds early.",
    schemes = "The schemes the provider accepts, such as \\code{c(\"bearer\", \"x-api-key\")}.",
    api_keys = "Explicit credentials by provider, as for \\code{new_router()}.",
    path = "The credentials file.", auth = "A sign-in scope to explain instead of the unmanaged chain, or \\code{NULL}.",
    run = "A function that runs a command, for tests.", clock = "A function returning the current time.",
    named = "A named cloud identity, or \\code{NULL} for the whole chain."),
  details = "\\code{explain_auth()} walks the same chain a request uses and never calls a credential function, runs a command or contacts a server. A cloud chain's metadata-server rung is shown as unprobed offline.",
  value = "A credential value, \\code{TRUE}/\\code{FALSE}, a scheme name, an explanation report (print it), a path, a stored credential, a store, or a credential function.",
  examples = r"-(
explain_auth("openai", env = c(OPENAI_API_KEY = "sk-example"))
explain_auth("vertex", env = c(GOOGLE_CLOUD_PROJECT = "my-project", NO_GCE_CHECK = "1",
                               HOME = tempdir()))
)-"),

page("oauth", "Protocol building blocks",
  c("generate_pkce", "pkce_challenge", "jwt_rs256", "sigv4_sign", "token_exchange_build", "token_exchange_parse", "oauth_callback_listener"),
  description = "The protocol pieces lm15's sign-in and cloud credentials are made of, for applications that need them directly: PKCE, RS256 JWTs, AWS Signature Version 4, token exchanges and a loopback callback listener.",
  args = c(verifier = "A PKCE verifier.", header = "The JWT header, a JSON object.", claims = "The JWT claims, a JSON object.",
    private_key_pem = "An RSA private key in PEM.", method = "The HTTP method.", url = "The URL.",
    credential = "An \\code{aws_credentials()} value.", region = "The AWS region.", service = "The AWS service name.",
    body = "The request body (raw), or for \\code{token_exchange_parse()} the reply.", rung = "The chain rung, such as \\code{\"web-identity\"}.",
    input = "The rung's input (a credential file's contents, ...).",
    expected_state = "The OAuth state the return must carry.", host = "The address to listen on: only 127.0.0.1.",
    port = "The port, or 0 for any free one.", path = "The callback path."),
  value = "A PKCE pair or challenge, a JWT, signed headers, a token request, a credential, or a listener with \\code{redirect_uri}, \\code{wait()} and \\code{close()}.",
  examples = r"-(
pkce_challenge("dBjftJeZ4CVP-mJ92K9pVlxdFKrJK1GZkDy0BNIAVQM") # RFC 7636 Appendix B
)-"),

page("testing", "Test without a network", c("fake_lm", "fake_transport", "recorded_requests"),
  description = "\\code{fake_lm()} answers requests from a script; \\code{fake_transport()} answers HTTP requests from a script, so the real request building and reply parsing run. Both record what they were sent.",
  args = c(responses = "For \\code{fake_lm()}: a list of strings, responses or conditions, used in order. For \\code{fake_transport()}: a list of \\code{list(status, body, headers, chunks)} replies or conditions.",
    x = "A fake from \\code{fake_lm()} or \\code{fake_transport()}."),
  value = "A fake client, a transport function, or the list of recorded requests.",
  examples = r"-(
transport <- fake_transport(list(list(status = 200L, body =
  '{"id":"r","model":"gpt-5-mini","output":[{"type":"message","role":"assistant",
    "content":[{"type":"output_text","text":"hi"}]}]}')))
client <- new_lm("openai", api_key = "sk-example", transport = transport)
response_text(complete(client, request("gpt-5-mini", list(message_user("hello")))))
recorded_requests(transport)[[1]]$url
)-"),

page("internal", "Internal entry points", c("vet_handle", "browser_dispatch", "surface_dump", "live_setup_frames_internal_placeholder"), internal = TRUE,
  description = "Entry points for the lm15 conformance harness (\\code{vet_handle()}), the webR browser bridge (\\code{browser_dispatch()}) and introspection (\\code{surface_dump()}). Not for direct use.",
  args = c(line = "One JSON line of the harness protocol.", action = "The bridge action.", id = "The bridge call id.", payload = "The call's JSON payload."),
  value = "A JSON string, or a list describing the canonical types.")
)
pages[[length(pages)]]$functions <- c("vet_handle", "browser_dispatch", "surface_dump")
