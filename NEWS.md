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
