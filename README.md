# lm15 for R: the API we want to build

**Design preview—not instructions for the current implementation.**
This README and the seven vignettes propose a coherent R interface for review.
Some names and behavior are new. All examples are static, non-executable text;
no code, tests, validation, rendering or builds were run for this rewrite.
The R implementation, exports and installed reference manuals are unchanged.

Start with the [API decision ledger](docs/api-vision.md): it defines names,
signatures, return values, side effects and compatibility decisions before the
guides use them. The [earlier API review](docs/vignette-api-review.md) explains
what motivated the proposal. Neither document ratifies a shared-contract change.

## The idea

lm15 is a small foundation for conversations with different model providers.
You build a request, explicitly ask a client to perform it, and keep the
complete response. A friendlier R API should make those steps easier to inspect
and combine—not hide them behind an agent, a prompt mini-language or retries.

```r
library(lm15)

req <- request(
  "demo-model",
  message_user("Explain a list column in one sentence."),
  config = config(max_tokens = 100L)
)

client <- fake_lm("A list column holds a list element in each row.")
answer <- complete(client, req)
response_text(answer)
```

The reply is scripted teaching data, not a real model result. The same
completion name and response shape remain when you choose a provider.

## Less structural editing, no hidden conversation

```r
next_req <- req |>
  append_messages(answer$message) |>
  append_messages(message_user("Show a small R example."))
```

Each operation returns a new request. It does not modify `req`, send anything,
truncate history, or reconstruct the assistant's message from displayed text.
Continuation data stays attached to the actual message and its parts.

`complete(client, request)` stays client-first. Use pipes where values naturally
compose; do not add another completion name solely to rearrange the arguments.

## A client is a recipe, not an implicit login

```r
client <- new_lm(
  "openai",
  credential = credential_env("OPENAI_API_KEY")
)
report <- explain_auth(client)
```

Construction checks supplied arguments without reading sources or opening a
connection. Credential acquisition happens during an explicit operation.
Inspection may read local source metadata but never invokes a credential
function, refreshes, runs a CLI or contacts a provider.

The preferred arguments are `credential` and router `credentials`, with
exclusive compatibility aliases for `api_key` and `api_keys`. A source recipe,
tagged credential or callback still uses the one declared authentication policy.
A failed selected subscription never quietly becomes a paid-key request.

For actual model work, configuration and local execution controls are separate:

```r
req <- request("gpt-4.1-mini", message_user("Explain the idea briefly."))
answer <- complete(client, req, control = request_control(timeout = 30))
```

This proposed example would require credentials and may incur charges if run
after implementation. Time and memory controls are not model parameters and
are not written into canonical request JSON.

## Useful summaries without leaking contents

A proposed display—not captured package output:

```text
<lm15 request [value]>
  messages: 1 user; parts: 1 text
  config: max_tokens
  content: hidden
```

Responses should identify their ending, part kinds and reported usage.
Resource summaries should identify shared state and whether reading a snapshot
is local. Automatic display must not dump prompts, credentials, tool input,
authorization codes, provider data or error messages from user code.

Input errors should name the operation, safe argument/field paths and the
required shape. Unused arguments must be rejected without evaluating them.
Canonical provider error classes and codes stay unchanged.

## Analysis views keep the evidence

```r
responses <- list(answer)
view <- response_table(responses)
texts <- response_texts(responses)
counts <- usage_dbl(responses, "input_tokens")
exact_counts <- usage_chr(responses, "input_tokens")
```

The optional table adapter retains whole responses, usage and conditions in
list columns. It never runs a request or deletes a failed row. Refusals, tool
calls and failed attempts remain different outcomes.

`usage_dbl()` provides checked ordinary numeric values; it refuses unsafe
integer conversion. `usage_chr()` preserves exact decimal digits. Unknown
counts remain missing, not zero. The core still returns typed responses, not
auto-flattened data frames, and does not depend on the whole tidyverse.

## Resource verbs tell you what happens

| Operation | Proposed meaning |
|---|---|
| `snapshot(job)` | Read local cached state; no provider request |
| `refresh(job)` | Fetch status once and update the shared handle |
| `wait(job)` | Poll with an explicit overall budget |
| `results(job)` | Refresh status once, then fetch batch output; never poll |
| `results(job, status = "snapshot")` | Use an explicitly trusted terminal snapshot; no status-fetch fallback |
| `cancel(job)` | Request remote batch cancellation |
| `close(session)` | Attempt local connection cleanup; not a promise about billing |

`turn(session)` has one meaning: a collecting view which sends nothing.
Its snapshot can be incomplete; its result stops at a tool call when the
application owes an answer. Closing the view does not close the session.

```r
answer <- with_live(
  client,
  live_config("gpt-realtime"),
  function(session) {
    session$send(live_client_text_event("Hello"))
    result(turn(session, control = turn_control(timeout = 60)))
  }
)
```

The helper takes an ordinary function and always attempts cleanup. It preserves
a primary failure rather than replacing it with a cleanup error or retrying.
The existing send-and-collect `session$turn(content)` ambiguity is removed from
the recommended surface.

## JSON remains a precise boundary

```r
object <- as_json_object(list(label = "example", note = NULL))
array <- as_json_array(c("a", "b"))
archive <- responses_json(responses, include_provider_data = FALSE)
```

Objects and arrays remain distinct when empty. Keys are not repaired into R
names. Opaque payloads keep their nulls and empty values. Exact large integers
remain exact; explicit conversion chooses an analysis representation.

The canonical reader's forward compatibility is unchanged. An optional
`check_canonical_fields()` authoring check does not silently redefine that
reader. Foreign-format import keeps its own map/pass/refuse boundary.

## Read the proposed guides

1. [Start with a request](vignettes/lm15.Rmd)
2. [Keep control of tools](vignettes/conversations-and-tools.Rmd)
3. [Keep whole responses in your table](vignettes/data-workflows.Rmd)
4. [Make data boundaries explicit](vignettes/json-and-media.Rmd)
5. [Separate identity, preparation and execution](vignettes/clients-and-routing.Rmd)
6. [One stream contract, one turn meaning](vignettes/streams-and-live.Rmd)
7. [Read snapshots, request work](vignettes/jobs-and-resources.Rmd)

The guides explain expected behavior; they are not executable demonstrations
of it. The current `man/` reference pages and `R/` sources describe the existing
implementation. Earlier implementation results remain recorded in
[IMPLEMENTATION.md](IMPLEMENTATION.md); they do not verify this proposed API.

## What is deliberately not added

No automatic tool loop, retry engine, model substitution, history summary,
response cache, global inference defaults, or schema guessed from R arguments.
No flattening that loses content or turns missing usage into zero. No promise
that browser APIs can read desktop credentials or set forbidden headers.

The proposal preserves lm15's shared words, canonical values and provider
truth. Argument aliases, registry value semantics, prepared-request methods,
resource methods and timeout changes need explicit approval and migration
before implementation. The aim is not more magic: it is less guessing.
