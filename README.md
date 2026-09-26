# lm15 for R

One way to talk to many language model providers. You build a request, ask a
client to send it, and keep the whole response. Nothing is sent, retried,
shortened or remembered behind your back.

lm15 also exists for [Python](https://github.com/lm15-dev/lm15-python),
[TypeScript](https://github.com/lm15-dev/lm15-ts),
[Rust](https://github.com/lm15-dev/lm15-rs) and
[Go](https://github.com/lm15-dev/lm15-go). All five implement the same
[contract](https://github.com/lm15-dev/lm15-contract) and pass the same
conformance suite. Guides for every language are at [lm15.dev](https://lm15.dev/docs/).

## Install

```r
# install.packages("remotes")
remotes::install_github("lm15-dev/lm15-r")
install.packages(c("curl", "openssl"))  # networking; sign-in and request signing
```

Live (WebSocket) sessions also need libcurl 7.86 or newer with WebSocket
support when the package is installed. Everything else works without it.

## Start

```r
library(lm15)

router <- new_router()   # reads OPENAI_API_KEY, ANTHROPIC_API_KEY, ... when a request is sent
req <- request(
  "claude-haiku-4-5",
  list(message_user("Explain a list column in one sentence.")),
  config = config(max_tokens = 100L)
)
answer <- complete(router, req)
response_text(answer)
answer$usage$input_tokens
```

Continue a conversation by appending messages; lm15 keeps no hidden history:

```r
req$messages <- c(req$messages, list(answer$message, message_user("Show an example.")))
answer <- complete(router, req)
```

Stream, with your function called for each event:

```r
final <- stream(router, req, function(event) {
  if (event$type == "delta" && event$delta$type == "text") cat(event$delta$text)
})
```

## What it covers

* **Providers:** OpenAI (Responses and Chat Completions), Anthropic, Google
  Gemini, xAI, DeepSeek, Groq, OpenRouter, Z.AI, Moonshot, Meta, DeepInfra,
  Together AI, Fireworks AI, Parasail, TypeSafe, Azure OpenAI and Foundry,
  Amazon Bedrock, Google Vertex AI, and local Ollama, vLLM, SGLang and LM Studio.
  `providers()` lists them.
* **Requests:** text, images, audio, video, documents, tool calls and results,
  structured output, reasoning, prompt caching, token log-probabilities.
* **Judgments:** declared answers with a probability for each key where the
  provider can measure it (`judgments()`, `response_probabilities()`).
* **Beyond chat:** files, batches, stored caches, image, speech and video
  generation, live WebSocket sessions (OpenAI Realtime, Gemini Live).
* **Sign-in:** subscriptions (xAI, Claude, ChatGPT, GitHub Copilot, Kimi Code,
  Meta, OpenRouter), saved keys and cloud identities, in a private file shared
  with the other lm15 languages (`login()`, `connect()`).
* **Honesty rules:** a setting a provider cannot take is adapted and recorded
  on the response, or refused before sending, never dropped silently
  (`plan()` shows it in advance). A failed or signed-out subscription is never
  replaced by a paid key on its own. `explain_auth()` says which credential a
  request uses and why.

Start with `vignette("lm15")`; then `vignette("conversations-and-tools")`,
`vignette("signing-in")` and `vignette("beyond-chat")`.

## Stated deviations from the family's API

The words are the family's ([API family](https://github.com/lm15-dev/lm15-contract/blob/main/playbooks/api-family.md));
the mechanics are R's. Where R differs:

| Family | R | Why |
|---|---|---|
| `router.complete(req)`, `router.stream(req)` | `complete(router, req)`, `stream(router, req, on_event)` | S3 generics, client first. R has no iterator protocol: a stream calls your function with each event and returns the assembled response. |
| `ResponseStream` | `stream()`'s return value, `materialize_response()`, `replay_stream()` | Same reason. |
| Async variants | none | R code runs on one thread; interrupting a call releases the connection. |
| `Message(...)` constructor | `new_message()` | `message()` would mask `base::message()` for every user. |
| `text(...)` part factory | `text_part()` | `text()` would mask `graphics::text()`. `message_user("hi")` takes strings directly. |
| `response.data`, `.probabilities`, `.method` | `response_data()`, `response_probabilities()`, `response_method()`, `response_expected()` | R values have no computed properties. |
| `Auth.local().login(...)` | `login(provider, auth = local_auth())`, `status()`, `logout()`, ... | Functions over a scope; the local file is the default scope. |
| Error classes by `except` | `tryCatch(..., RateLimitError = ...)` | Conditions carry the contract's class names and `code`. |
| Loopback sign-in raced against a pasted return | the listener waits; interrupt (Esc or Ctrl-C) to paste the return instead | R runs one thread, so the two cannot be raced. |

## Status

* **Contract:** pinned at [`fe5cdf9`](https://github.com/lm15-dev/lm15-contract/commit/fe5cdf94b0494ebd9f968498e717dfeba8fb4df1), the same commit as Python
  1.1.0, TypeScript and Rust 1.0.0-rc.2 and Go v1.1.0-rc.2: **1,788 of 1,788**
  checks pass, including all 43 sign-in lifecycle runs. The credentials file has
  been shared with Python, TypeScript, Rust and Go processes in every order,
  and concurrent renewal spends a refresh token once in every language pair.
* **Live:** checked against a real vLLM server (chat, streaming, model
  listing, errors, the judgment fallback). No paid provider has been called
  from R yet.
* **Platforms:** `R CMD check` passes on Linux, Windows and macOS (CI), and
  the package runs in the browser through webR (tested in Chromium).

Details and the verification record are in [IMPLEMENTATION.md](https://github.com/lm15-dev/lm15-r/blob/main/IMPLEMENTATION.md).

## License

MIT
