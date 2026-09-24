# Proposed R API: decisions before implementation

**Design preview for review. Not implemented or approved as a new contract.**
The README and seven vignettes are rewritten against this plan. Their code
blocks are deliberately non-executable. The current R code and reference
manuals have not been changed. No examples, tests, validation or builds were run.

This is the naming and behavior ledger for the proposal. A vignette must not
invent an additional convenience function or change an option's meaning.
The earlier `vignette-api-review.md` remains a record of the implementation
that motivated this design, not a description of the proposed API.

## 1. Dense change list

| Decision | Proposed rule | Review findings |
|---|---|---|
| D01 | Keep `complete(lm, request)` and `stream(lm, request, on_event)`; retain canonical inputs and results. No `ask()`/`chat()` synonym. | 8, boundaries |
| D02 | Treat requests, responses, configs and model registries as values. Treat clients, jobs, sessions, turn views and credential stores as resources with explicitly shared state. | 11 |
| D03 | Constructors check supplied arguments and return recipes/values only. No environment lookup, file reads, credential callbacks, renewal, writes or connections inside `new_lm()`/`new_router()`. | 2, 17 |
| D04 | Unknown R argument names are errors without forcing unused expressions. Required arguments also require exact named matching. Error messages show safe names/paths, not supplied expressions or values. | 1, 4 |
| D05 | Make `credential`/`credentials` the preferred client/router argument names. Keep `api_key`/`api_keys` as exclusive compatibility aliases. | 17 |
| D06 | Add deferred `credential_auto()` and `credential_env()` recipes; retain tagged credentials and zero-argument credential functions. Use the existing declared auth chain, never a second chain. | 2, 17 |
| D07 | Add `host_settings()`, `request_control()`, `live_control()` and `turn_control()` as strict option values, each with one documented scope. | 12, 17 |
| D08 | Add explicit `prepare_request(lm, request)`. Add pure `build_request(prepared)` for the resulting resolved HTTP request. Keep the old two-argument builder as an effectful compatibility facade. | 2 |
| D09 | Make `explain_auth(client)` and `explain_auth(router, model=)` inspect the actual recipe. Inspection may read local sources, but never calls credential functions, runs commands, refreshes or contacts providers. | 2, 17 |
| D10 | Give values/resources compact structural summaries and safe list-column formatting. No prompt text, tool input, provider data, codes or credentials in automatic display. | 3 |
| D11 | Add structured R input errors with argument and R-indexed field paths; retain canonical provider error classes/codes and canonical zero-based indexes. | 4 |
| D12 | Bless `message_user()`, `message_assistant()`, `message_tool()` and `text()` for ordinary composition. Keep exact `*_part()` constructors as the explicit low-level family; do not add more aliases. | 6 |
| D13 | Add `append_messages(request, messages)`: a value transformation accepting one message or a flat list. No sending, truncation, reconstruction or automatic history. | 7 |
| D14 | Add `response_texts()`, `usage_dbl()` and `usage_chr()` for explicit, type-stable extraction. Keep scalar `response_text()` returning string-or-NULL. | 5, 14 |
| D15 | Add optional `response_table(responses, errors=)` with a fixed schema, complete objects retained, and no automatic numeric conversion or network calls. | 5, 18 |
| D16 | Add `as_json_object()` and `as_json_array()` for existing data. Never repair JSON keys or guess array shape during serialization. | 13 |
| D17 | Keep exact numbers scalar for now. Add explicit `json_number_integer()` and `json_number_double()` extraction, not an incomplete vector type. | 14 |
| D18 | Make provider-data inclusion local to one response; add `responses_json()` for a collection with an explicit inclusion policy. Reject inapplicable serialization options. | 15 |
| D19 | Keep the default canonical reader forward-lenient. Add a separate opt-in `check_canonical_fields()` for authors of JSON. Foreign-format import retains map/pass/refuse behavior. | 19 |
| D20 | Use S3 resource verbs: `snapshot`, `refresh`, `wait`, `result`, `results`, `cancel`, `next_event`, and methods for base `close()`. Their effects depend on documented resource types, never an omitted argument. | 10, 11 |
| D21 | Make `turn(session)` mean only “open a collecting view”; it never sends. Retire the send-and-collect meaning of `session$turn(content)` from the primary API. | 9 |
| D22 | Make job output fetching explicit with `status = "refresh"` or `"snapshot"`. A refresh updates the handle snapshot; no implicit polling or fallback between modes. | 10 |
| D23 | Add callback-based `with_live()` and `with_oauth_callback()` scopes. Always attempt local cleanup, preserve the primary error, and never cancel a remote job as a cleanup shortcut. | 11 |
| D24 | Curate exports and the reference index by audience. Keep optional analysis/docs dependencies separate from execution and keep transport codecs single-sourced. | 16, 18 |

These decisions are intentionally not all “add another helper”. Client-first
calls stay; scalar nullable text stays; explicit schemas stay; the core does
not become a data frame, an agent, or a response cache.

## 2. Vocabulary and signatures

Signatures below describe the proposed primary surface. `...` is reserved for
explicitly documented extension methods; built-in methods reject unused names
without evaluating their values. All optional arguments shown after dots must
be named exactly. Defaults are local values, not actions.

### Canonical values and composition

```r
request(model, messages, ..., system = NULL, tools = list(), config = config())
config(...)
message_user(content, ...)
message_assistant(content, ...)
message_tool(id, content, ..., name = NULL, is_error = FALSE)
text(content, ..., continuation = list())
message(role, parts, ..., continuation = list())
append_messages(request, messages, ...)
```

These promises cover library-owned work. R still evaluates legitimate caller
expressions when their values are needed: `api_key(read_a_key())` can do I/O
because the caller explicitly requested it. Use source recipes or callbacks to
keep acquisition deferred. Built-in entry points reject locally knowable bad
names before forcing unused arguments. S3 generics must not reject arguments
belonging to a selected third-party method merely because the generic itself
does not use them.

The canonical constructors retain their current field meanings. `request()`
accepts one typed message or a flat list; role helpers accept a string, a part,
or a flat list of strings/parts. A character vector passed to a role helper
means several text parts in **one message**, never several requests.

`append_messages()` returns a new validated request. It accepts one message or
a flat list of messages, allows an empty list as an explicit no-op, rejects
nested lists, and preserves every supplied part and continuation value.
The caller must explicitly append `answer$message` before tool results or a
follow-up. It never reads or alters a client.

Use `text()` for common text content; use `image_part()`, `audio_part()`,
`tool_call_part()`, `tool_result_part()` and other exact constructors when their
fields matter. Existing duplicate `text_part()`-style names remain compatibility
constructors, not a second recommended vocabulary. Use `lm15::text()` and
`lm15::message()` when those names would be confused with other R functions.

### Clients and identity

```r
new_lm(provider, ..., credential = credential_auto(), base_url = NULL,
       compat = NULL, settings = host_settings(),
       transport = transport_curl(), env = NULL)
new_router(..., credentials = list(), base_urls = list(), settings = list(),
           catalog = new_model_registry(), transport = transport_curl(),
           env = NULL)
credential_auto()
credential_env(name, ..., kind = "api_key")
host_settings(..., region = NULL, workspace = NULL, project = NULL,
              location = NULL, resource = NULL, authority_host = NULL,
              scope = NULL)
explain_auth(client, ...)
explain_auth(router, ..., model)
resolve(router, model, ...)
router_lm(router, model, ...)
```

`credential` accepts a tagged value, a source recipe, or a zero-argument
function returning a tagged value/string. `credential_env()` accepts exactly
`"api_key"` or `"bearer_token"`; it does not infer a token kind from its text.
Use `credential_auto()` for a provider's declared chain, including cloud
credentials. A plain string remains API-key shorthand but is not recommended
for saved source code.

`credentials` is the router's provider-named map of the same inputs. Existing
shared-key, exact-entry precedence and duplicate-provider-spelling rules remain.
`api_key` and `api_keys` remain compatibility argument aliases. Explicitly
supplying both old and new names is an error, even if one is NULL; there is no
secret-value comparison or precedence guess.

`env = NULL` describes the process environment to consult when resolving a
source, not permission to read it in the constructor. `env = character()` means
no ambient variables. A supplied named vector is captured as explicit input.
Client creation checks all supplied names, types, endpoints and host-setting
combinations first. Values still deferred to an environment/profile are checked
when resolved, before inference. Declared host defaults, including the GCP
location default, do not change; the report shows where they came from.

`host_settings()` is one shared vocabulary, not one loose list per provider.
A provider rejects settings it does not understand. Old named setting lists
can be converted with the same checks. In a router, each `settings` entry is
one provider's `host_settings()` value.

`router_lm()` constructs or reuses a recipe, not a connection. Cache identity
includes all credential/endpoint/settings inputs. A changed identity cannot
reuse another client's token. Credential functions are invoked once per
request preparation, not once per chunk or once per print.

`explain_auth()` may read source presence and credential-file metadata. It
never invokes a custom credential function, refreshes a token or runs a CLI.
Its summary distinguishes “a source is configured”, “the source needs work at
request time” and “the credential was accepted by a provider”. It cannot claim
the last fact without an actual operation. Printing hides values.

### Execution, preparation and controls

```r
complete(lm, request, ..., control = request_control())
stream(lm, request, on_event, ..., control = request_control())
request_control(..., timeout = 120, connect_timeout = 30,
                max_response_bytes = 128 * 1024^2,
                max_sse_line_bytes = 1024^2,
                max_sse_event_bytes = 8 * 1024^2)
prepare_request(lm, request, ..., stream = FALSE, control = request_control())
build_request(prepared, ...)
```

`complete()` returns one canonical response; `stream()` delivers canonical
events and returns the assembled response. Both are S3 extension points.
No text-only shortcut, automatic tool loop, retry or provider fallback is added.

The same named `control = request_control()` is available on one-shot provider
operations: model listing, file/cache operations, image/speech generation,
batch/video submission or attachment, and output retrieval. These retain their
existing primary argument order and canonical result types. Constructors such
as `file_upload_request()` do not acquire execution controls or perform work.

`request_control()` is local execution policy and never part of canonical
`Config` or its JSON. The operation budget starts at entry to `complete`,
`stream` or `prepare_request`, and includes preparation. Each built-in I/O step
receives the remaining budget, additionally capped by its connection/backend
limit. A custom callback or transport must cooperate: arbitrary R code cannot
be forcibly interrupted safely, and overruns must not be reported as completed
successes. Callback time counts toward elapsed operation time.

`prepare_request()` is an **advanced HTTP request-preparation boundary**, not a
new canonical wire type. It resolves sources, credentials, required host data,
local media and the signing time once, after all available local preflight
checks. It may read/write files or make credential-exchange calls. It performs
no inference request. Its result is bound to the resolved HTTP operation and
stream mode, hides secrets, and is neither serializable nor a reusable cache.
Realtime connection/frame preparation keeps its separate existing advanced
surface; it is not misrepresented as a single HTTP request.

`build_request(prepared)` is pure: no callback, clock, file or network lookup.
The old `build_request(lm, request, stream=)` remains an effectful compatibility
facade over preparation plus encoding; it is **not** relabelled pure. The harness
can continue calling its existing entry point. Normal execution prepares fresh
work itself; inspecting a preparation and then calling `complete(lm, request)`
performs a separate fresh preparation, not a send of the inspected object.

### Analysis without flattening the conversation

```r
response_text(response, ...)
response_texts(responses, ..., missing = NA_character_)
usage_dbl(responses, field, ..., missing = NA_real_)
usage_chr(responses, field, ..., missing = NA_character_)
response_table(responses, ..., errors = NULL)
```

The plural helpers take a flat list of responses or NULLs. They preserve list
length and ordering; an empty input produces a typed zero-length output.
`response_texts()` returns character; `usage_dbl()` returns double and refuses
any counter outside exact double-integer range, naming its R row position
without rounding; `usage_chr()` returns exact decimal strings. Missing-value
arguments must be scalar values of the declared output type. Usage field names
are the exact canonical counter names, with no partial matching.

`response_text()` is unchanged: a string or NULL. A refusal or tool-bearing
response does not become plain text. Choosing a missing marker for an analysis
column changes only that column, not the original response.

`response_table()` is an optional tibble adapter. It does not require purrr;
it accepts a parallel list of conditions/NULLs from any caller. `errors = NULL`
means an all-NULL error list. Supplied lists must have equal length. A row cannot
contain both a response and an error. A row with neither is “no result supplied”,
not a fabricated successful attempt. It always returns these columns:

| Column | Type and meaning |
|---|---|
| `.row` | One-based input position, including empty/missing results |
| `has_response`, `has_error` | Logical, never inferred from text availability |
| `text` | Character; NA where the scalar text view is unavailable |
| `finish_reason` | Character; canonical ending or NA if no response |
| `error_code` | Character; LM15 error code or NA for no/non-LM15 error |
| `error_class` | Character; primary condition class or NA |
| `response` | List of original response values/NULLs |
| `usage` | List of usage values/NULLs; counters are not coerced |
| `error` | List of original conditions/NULLs |

It does not print error messages or opaque contents automatically. A caller
adds their own IDs and requests; there is no automatic join, row deletion or
request execution. `tibble`/presentation integration remains optional.

### JSON and exact numbers

```r
json_object(...)
json_array(...)
as_json_object(x, ...)
as_json_array(x, ...)
as_json(x, ..., include_provider_data = FALSE)
responses_json(responses, ..., include_provider_data = FALSE)
from_json(text, kind, ...)
from_dict(value, kind, ...)
check_canonical_fields(x, kind, ...)
integer_value(decimal, ...)
json_number_integer(x, ...)
json_number_double(x, ...)
```

`as_json_object()` requires a named list with unique, non-NA string keys; an
explicit empty-string key is allowed. An empty list explicitly becomes `{}`.
`as_json_array()` accepts an unnamed list or unnamed plain logical/integer/
double/character vector; an empty input becomes `[]`. Named input must be
explicitly stripped of names by the caller. No automatic factor/date/raw-byte
conversion, name repair or recursive empty-value cleanup is allowed.

`as_json()` applies canonical type-local rules; opaque payloads are unchanged.
Provider-data inclusion is an option for a top-level canonical response, not
a recursive instruction for arbitrary JSON containers. An explicitly supplied
inclusion option on an incompatible root is rejected, not ignored. The new
`responses_json()` accepts a list of responses/NULLs and returns **one JSON
array string**, applying the selected policy to each response and retaining
NULL slots. It does not reclean the children. Canonical nested-type rules,
including batch-entry serialization, remain unchanged.

Exact integers remain scalar values. `json_number_integer()` extracts an
integral token without rounding and refuses fractional values.
`json_number_double()` explicitly requests a finite R double interpretation;
ordinary floating-point rounding applies, but integral tokens outside the safe
integer range are refused. Neither operation mutates the stored JSON token.
A complete vctrs numeric vector is deferred, not half-promised by a few methods.

The default canonical readers retain contract leniency for ordinary unknown
fields. `check_canonical_fields()` is a separate authoring check: it accepts
canonical JSON text or an object, checks known fields at typed nodes only,
leaves opaque child keys alone, and returns its input invisibly on success.
It does not replace value/type validation or change the default readers.
Foreign Chat Completions import keeps its separate map/pass/refuse policy.

### Jobs, sessions and lifetime

```r
snapshot(resource, ...)
refresh(job, ..., control = request_control())
wait(job, ..., timeout = 300, poll_every = 5, control = request_control())
wait(listener, ..., timeout = 300)
results(batch_job, ..., status = "refresh", control = request_control())
result(video_job, ..., status = "refresh", control = request_control())
result(turn_view, ...)
cancel(batch_job, ..., control = request_control())
close(resource, ...)
live(lm, config, ..., control = live_control())
live_control(..., connect_timeout = 30, read_timeout = 120,
             max_frame_bytes = 32 * 1024^2, max_queue = 1024L)
next_event(session_or_view, ..., timeout = NULL)
turn(session, ..., control = turn_control())
turn_control(..., timeout = 120, max_events = 1024L,
             max_bytes = 128 * 1024^2)
with_live(lm, config, fn, ..., control = live_control())
with_oauth_callback(fn, ..., expected_state, port = 0L, path = "/callback")
```

- `snapshot(job)` and `snapshot(view)` are local observations returning values.
  Existing `job_info(job)` is a compatibility alias for the job snapshot only.
- `refresh(job)` makes one provider status fetch, updates the shared handle,
  and invisibly returns it. Credential resolution may also do work.
- `wait()` is the only job polling operation. Its total elapsed budget includes
  sleeps and status fetches; each fetch uses the smaller remaining wait budget
  and its request control. Cached terminal jobs return immediately. Timeout zero
  means no waiting; NULL explicitly disables only this wait's overall budget.
- `wait(listener)` awaits one validated loopback OAuth callback and returns its
  protected code/state value. It does not poll an external provider or open a
  browser; its budget starts at the call. This is a typed waiting operation,
  not a job method accepting ignored `poll_every` or request-control arguments.
- Job `result(s)` never poll. `status = "refresh"` fetches status once and
  updates the handle before fetching output. `"snapshot"` requires a cached
  terminal snapshot and uses it as-is, with no automatic refresh fallback.
  A video result specifically requires a completed snapshot; failed/cancelled
  video jobs do not have a successful output to fetch. Batch terminal results
  retain per-entry failures. Output retrieval can still fail because a remote
  resource expired or changed. Both modes capture one status value for that
  output operation; another handle refresh does not retarget an in-progress fetch.
- `cancel()` requests remote batch cancellation. `close(job)` is deliberately
  **not** a cancellation method: jobs are remote work, not local connections.
- `session$send(event)` remains the explicit live-event send operation, not a
  synonym for completion. It does not read a response or start a collecting view.
- `live()` opens a connection. Its control has per-connection/per-read limits,
  not a deadline on the lifetime of the conversation. `next_event()` reads once;
  NULL means closed, not a successful completed turn. An explicit read timeout
  cannot extend a collecting view's remaining budget.
- `turn(session)` sends nothing. It creates a local collecting view. Its total
  collection budget starts on the first read and includes time between reads.
  `result(view)` collects to a terminal event or tool call; it never reads past
  an already yielded tool call. `snapshot(view)` may be incomplete. A result
  cannot turn an incomplete view into success. Raw reading can cross tool-call
  boundaries when the application deliberately supplies results.
- `close(view)` stops only the view. `close(session)`/`close(listener)` attempt
  local cleanup. These closes are idempotent and return invisible NULL. Remote
  generation/billing is not guaranteed to stop. `cancel(batch_job)` instead
  returns the updated handle invisibly; acceptance of cancellation need not
  mean the remote job has already stopped.
- Scoped helpers take an ordinary function, not a data-masked code block.
  They return its value and always attempt cleanup. Cleanup failure emits a
  redacted warning; it does not replace a primary error or replay any operation.
  `with_oauth_callback()` never opens a browser or runs a login by itself.

Canonical part/batch indexes remain zero-based. R access paths and analysis
positions are one-based and are explicitly labelled rather than silently
renumbering canonical data. Files, caches, generation requests, and batch/video
submission constructors keep their existing canonical meanings and names.
Low-level build/parse functions remain public, grouped under advanced APIs.

### Browser counterpart

The JavaScript `Lm15WebR` constructor adopts preferred `credential`, with
`apiKey` retained as an exclusive compatibility alias. Browser credentials are
explicit tagged values/strings or asynchronous functions; desktop auto/env/file
recipes are not supported there. Existing asynchronous method names and
JSON-text methods remain. A browser session exposes `send`, `nextEvent`,
`events`, `interrupt` and `close`; the closure is local and idempotent. Native
browser WebSocket header restrictions still require an explicit connector or a
server-side client. This is the browser counterpart of the terminology change,
not permission to infer a platform capability that does not exist.

### Local metadata and test doubles

```r
new_model_registry(models = list(), ...)
registry_add(registry, model, ..., replace = TRUE)
registry_get(registry, provider, model, ...)
registry_models(registry, ..., provider = NULL)
registry_providers(registry, ...)
discover_model_registry(..., catalogs = NULL, libraries = .libPaths())
fake_lm(responses, ...)
fake_transport(responses, ...)
recorded_requests(fake, ...)
```

Registries become value-like: `registry_add()` returns a new registry, does not
mutate its input, and removes replaced entries' stale aliases. `registry_get()`
checks an exact ID first, then aliases within the named provider. It returns
NULL for no match and raises an ambiguity error for several alias matches,
rather than choosing by insertion order. `registry_models()` returns a typed
model list in registry order; `registry_providers()` returns sorted unique names. `new_router()`
captures the supplied registry value; replace its catalog deliberately to change
it. This is a proposed R behavior change, not an innocuous alias for today's
mutable `$add()` method. Discovery remains explicit, processes named sources in
order, and validates a whole catalog before including it. It may read files or
invoke explicitly supplied catalog functions. Metadata never changes encoding.

Fake clients/transports remain deliberately stateful queues. The same public
execution API consumes one scripted response per attempt. They do not generate
answers, infer schemas or make provider calls. The guides label scripted data.

## 3. Display and error contract

Illustrative display shapes, **not captured output**:

```text
<lm15 request [value]>
  messages: 1 user; parts: 1 text
  config: max_tokens
  content: hidden

<lm15 response [value]>
  finish: tool_call; parts: 2 tool calls
  usage: not reported
  content and provider data: hidden

<lm15 batch job [shared resource]>
  cached status: running
  snapshot() is local; refresh() fetches status
```

Safe summaries expose structural metadata and explicitly reported counters,
not arbitrary supplied identifiers or contents. Detailed content access stays
explicit through fields/accessors/serialization. Unknown usage is displayed as
“not reported”, never zero. Table/list-column formatting follows the same rule.
Only presentation preferences may be global; credentials, providers, model
choices and retry policies may not.

R input errors have class `lm15_input_error` plus ordinary R condition ancestry,
and structured `arg` and `path` fields. Paths describe R indexing, e.g.
`messages[[2]]$parts[[1]]`. These are not new canonical provider errors.
Provider error classes and ErrorCodes remain unchanged. A displayed call names
the operation without rendering the supplied call expressions or values.

## 4. Compatibility and decisions needing approval

| Kind | Proposed handling |
|---|---|
| Shared facts | No changes to canonical field names, omissions, numeric types, continuation, auth precedence, error codes or provider mappings. |
| New R helpers | Additive API, requiring named signatures and explicit exports; they do not create new canonical kinds. |
| Credentials rename | Keep old argument names as exclusive aliases; update docs first, plan deprecation separately. |
| Prepared builder | Add a resolved-input overload; keep the old facade and harness entry point effectful and documented. |
| Resource methods | Keep family verbs; migrate recommended use away from public implementation fields and closure-only access. |
| Turn meaning | Prefer the single view meaning. Deprecation/removal of send-and-collect requires an explicit migration decision. |
| Registry values | Deliberate behavior change needing approval; old mutable registries need an adapter or migration, not a silent change. |
| Serialization flag scope | Reject previously ignored options; this is observable behavior and must be documented as such. |
| Time controls | New whole-operation/collection budgets need a migration plan from existing backend/per-read limits. No promise to preempt arbitrary user R code. |
| Native packaging | Keep verified TLS. Explore optional native components separately; do not promise a pure-R installation before there is a secure design. |

This proposal does not ratify any shared-contract change or declare the current
package feature-complete. Preserve existing APIs during transition where that
is honest; do not leave two equally recommended incompatible paths indefinitely.
The curated reference should distinguish core workflows, analysis views,
resource operations, advanced codecs, credentials and extension interfaces.

## 5. Rewrite map

| Guide | Proposed spine |
|---|---|
| README / Start with a request | Build a value, inspect its safe summary, complete it, append explicit messages. |
| Conversations and tools | Declare a schema, receive calls, run an explicit allowlist, append assistant and tool messages. |
| Work with a table | Capture attempts externally, use a fixed response-table view, choose exact or numeric usage extraction. |
| JSON and media | Explicit shape conversion, local omission rules, checked number extraction and collection serialization. |
| Clients and routing | Deferred identity recipes, source inspection, explicit preparation and pure encoding. |
| Streams and live turns | One callback contract, one turn-view meaning, ordinary scoped cleanup and clear budgets. |
| Files and jobs | Local snapshots, explicit status/fetch policies, state sharing, no implicit polling or cancellation. |

## 6. Deferred on purpose

No prompt mini-language, automatic agent/tool loop, history summarizer, response
cache, retry engine, automatic dataframe flattening, partial numeric vector
implementation, inference region guess, or tool schema inferred from R defaults.
The explicit JSON Schema remains the provider-facing declaration. Improving its
construction with a second schema language is not justified in this proposal.
