# lm15 1.1.0

Implements the lm15 contract at commit `0f3ea82`, the same commit as lm15 for
Python 1.2.1, TypeScript and Rust 1.0.0-rc.5 and Go v1.1.0-rc.4, and passes
all 1,901 of its checks.

* New: `provider_definition()` declares a provider lm15 does not list (a
  gateway, an untested host, a second OpenAI-compatible vendor). Pass it to
  `new_router(providers = list(...))` and the router treats it like a built-in
  provider, by name and alias, with `api_keys`, `base_urls` and the declared
  environment variables, in that router only; `new_lm()` also accepts one. A
  name another provider, a litellm prefix or a managed-login route already
  uses is refused, and so is an unknown compat knob. The built-in routing
  rules are now copied from the reference.

* Long streamed replies no longer fail at the last moment. The stream reader
  refused a line over 1 MiB and an event over 8 MiB, and real streams are
  larger: Gemini sends a 4K image as one 29.7 MB line, and OpenAI Responses
  repeats the whole response, system prompt included, in its first and last
  events. Streams now take an event of any size, as a non-streamed reply
  does; both stay bounded by the transport's whole-reply limit
  (`transport_curl(max_response_bytes = 128 * 1024^2)`). Splitting a long
  line out of many network reads is now linear (a 30 MB line: 1.3 s; the old
  splitter took 2.7 s for 4 MB, growing with the square of the length).
  Reading a 30 MB generated image end to end over HTTP now takes about 4 s
  (it took 45 s): base64 is checked with a PCRE pattern, and fields that
  only need to be valid JSON are walked rather than encoded and discarded.
  lm15-contract INV-056 (`changes/2026-10-06-sse-event-bound.md`); contract
  `0f3ea82`.

* A tool with no description works on every provider. A `function_tool()`
  with only a name and parameters was sent with `"description": null`, which
  Anthropic and Groq refuse with a 400. The description key is now left out
  when the tool has none (`""` counts as none, as it already does in lm15's
  own JSON), on every wire, including Gemini cached prefixes, Gemini Live and
  the OpenAI Realtime session. A tool with a description is sent exactly as
  before. lm15-contract MAP-17
  (`changes/2026-10-02-tool-description-absent.md`); contract `f6465c8`.

# lm15 1.0.1

Implements the lm15 contract at commit `57e33d1`, the same commit as lm15
for Python 1.2.0, TypeScript and Rust 1.0.0-rc.4 and Go v1.1.0-rc.3, and
passes all 1,838 of its checks. Also since 1.0.0: `input_audio` in Chat
Completions ingest reads ogg, opus, flac, aac, aiff, webm and mpeg as their
true media types.

* The `claude-code` provider claims Claude Code 2.1.285 (it claimed 2.1.170,
  which `claude-opus-5-5` refuses). When a model needs a newer release, pass
  `settings = list(client_version = "...")` (to `new_lm()`, or per provider
  to `new_router()`) or set `LM15_CLAUDE_CODE_VERSION`, which a router reads.
  The Codex provider's `client_version` is the same setting
  (`LM15_CODEX_CLIENT_VERSION`); `explain_auth()` prints the release and
  where it came from.
* The minimum-version refusal says which setting to change (updating the
  `claude` program does not move what lm15 sends).
* A settings entry a provider does not read is an error (it was ignored).
* An unset `max_tokens` on a Claude model is the model's own output ceiling:
  128000 for the 4.6 generation and later, 64000 for 4.5 (it was 16384); on
  the manual class it covers the thinking budget. Other models on
  Anthropic-dialect servers keep 16384.
* lm15-contract `changes/2026-09-30-claude-code-client-version.md`.

# lm15 1.0.0

First CRAN release. Implements the lm15 contract at commit `fe5cdf9`, the same
commit as lm15 for Python 1.1.0, TypeScript and Rust 1.0.0-rc.2 and Go
v1.1.0-rc.2, and passes all 1,788 of its checks.

* Requests, responses, stream events and errors shared with the other lm15
  languages; OpenAI (Responses and Chat Completions), Anthropic, Gemini, xAI,
  DeepSeek, Groq, OpenRouter, Z.AI, Moonshot, Meta, DeepInfra, Together AI,
  Fireworks AI, Parasail, TypeSafe, Azure, Amazon Bedrock, Google Vertex AI and
  local servers.
* Judgments: `judgments()`, `choice()`, `yes_no()`, `score()`; answers as data
  parts with per-key probabilities where the provider measures them
  (TypeSafe; vLLM by candidate-sequence likelihood).
* Sign-in: `login()`, `connect()`, `status()`, `logout()` and saved keys, env
  names, cloud identities and local servers, in a store shared with the other
  languages under one lock.
* Named cloud identities (`credentials = list(azure = "cli")`); host settings
  report where they came from (`explain_auth()`).
* Rate-limit diagnostics on errors (`rate_limit_headers`, `retry_after`).
* Files, batches, caches, image, speech and video generation, live sessions.
