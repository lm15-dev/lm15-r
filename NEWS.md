# lm15 (development version)

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
