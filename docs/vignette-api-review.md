# API review after writing the vignettes

> **Historical review of the earlier implementation-facing drafts.** The guides
> have since been rewritten as a proposed API, defined in
> [api-vision.md](api-vision.md). References below to vignette friction describe
> those earlier drafts. This review still explains the motivation; the decision
> ledger is the source of truth for the new proposal. Neither has changed the
> implementation or been validated by execution.

## Verdict

lm15 has the right foundation: explicit requests and responses, validated
content, replaceable transports, and refusals instead of silent changes to a
paid request. Making it more idiomatic in R should strengthen those properties,
not turn it into a prompt wrapper or an automatic agent.

The main friction is **predictability**, not a lack of pipes. A learner has to
remember which objects are ordinary values, which objects share mutable state,
which functions contact a provider, and which accessors return a scalar versus
NULL. The examples repeatedly work around those differences.

This is a **source-and-documentation review**. The vignette examples, proposed
changes and observations below were not executed, rendered or validated in
this pass. No API implementation was changed. Code marked “proposed” is not
available package syntax.

## Basis and boundaries

The review applies the [Tidy Design Principles](https://design.tidyverse.org/):
reduce the reader's mental load; be consistent; compose small operations;
make inputs and side effects explicit; give functions predictable outputs.
“Tidy” does not mean that every object must become a tibble or that every
function needs a pipe-friendly synonym.

lm15's own constraints take precedence:

- `THEORY.md` sections 1–3 explain the intent: a small, explicit foundation,
  with honest provider translation, not a policy-heavy application framework.
  That document is historical; use the pinned contract for current behavior.
- `lm15-contract/AUTHORITY.md` makes the contract authoritative, not Python or
  an R convenience layer.
- `lm15-contract/playbooks/api-family.md` fixes words such as `complete`,
  `stream`, `request` and `response`, keeps canonical inputs/outputs visible,
  and requires explicit explanations for language-specific deviations.
- `docs/serde-rules.md` and INV-001–048 protect JSON distinctions, declared
  number types, construction-time validation and opaque user data.
- AUTH-1–7 protect credential selection, ownership, secrecy and explainability.
- MAP-1 and MAP-3–10 protect tool ownership, stream endings, continuation data
  and refusals where a provider has no supported mapping.

Here, **R-facing** means a change to learning, display or local composition that
can preserve canonical values and provider requests. It does not automatically
mean “non-breaking”: a public argument name or return type still needs an
explicit compatibility plan.

## Findings that should lead the work

### 1. Reject unused arguments without evaluating them — highest priority

**Where it surfaced:** configuration mistakes in *Start with a request* and
*Choose clients, routes and credentials deliberately*.

**Source:** `.check_dots()` in `R/json.R` uses `length(list(...))`. Constructing
that list evaluates the supplied expressions before reporting an unused
argument. A misspelled option can therefore perform work even though the call
is rejected.

Illustrative, not executed:

```r
new_lm("openai", api_kye = fetch_a_credential())
```

The error message also omits the names of the offending arguments. A learner
gets neither a safe failure boundary nor a useful correction.

**Recommendation:** inspect dots without forcing their promises, then report
only their names. Base R can count unevaluated dots; a condition library can
also help. Do not adopt an error formatter that displays argument expressions
or literal values blindly: an expression can contain a credential.

**Compatibility:** preserve the existing refusals. Improving diagnostics and
preventing rejected expressions from running need not alter canonical JSON or
provider traffic. This serves lm15's “no unexpected work” goal as directly as
it serves tidy API design.

### 2. Make client preparation and credential side effects predictable

**Where it surfaced:** the credential counter example in *Choose clients,
routes and credentials deliberately*. The prose needs to qualify what “build”
and “construct” mean.

**Source:** `new_lm()` in `R/client.R` can call `load_local_credential()` before
validating later compatibility and endpoint settings. An expired stored login
can be renewed and written during construction. `build_request()` invokes
credential providers, and media paths can be read while building payloads.

There are two separate issues: a constructor can do more than its name suggests,
and an invalid later setting can be discovered after earlier credential work.
Calling every request builder “pure” would be false for the current facade.

**Recommendation:** validate the complete local configuration before acquiring
or renewing a credential. Keep credential acquisition in an explicit,
request-time provider where possible. Distinguish a source-resolving preparation
step from the pure codec inside it. If the public `build_request()` facade must
retain source resolution for compatibility, document that accurately rather
than promising no I/O.

**Do not change:** stored subscriptions must still own their selected provider;
failed renewal must never quietly pick a paid environment key. Do not add
another helper that probes by making a paid request.

### 3. Give values useful, privacy-preserving summaries

**Where it surfaced:** `req` in the first vignette prints its type and field
names, so the next lines must inspect fields individually. List columns in
*Work with a table of requests* are hard to scan.

**Source:** `print.lm15_value()` and `str.lm15_value()` in `R/types.R` share one
very generic display. Other object families have separate, uneven summaries.

**Recommendation:** give requests, responses, pages and snapshots compact,
consistent summaries: type, message/part counts, finish reason, reported usage,
item count and continuation presence. Use safe metadata, not a dump of prompt
text or opaque data. Add explicit opt-in content display if useful. For list
columns, consider pillar formatting that identifies the value meaningfully
without serializing or expanding it.

**Boundary:** never print credential contents, signed assertions, authorization
headers or provider payloads by default. A prettier display is not permission
to relax secrecy. Display width and detail can be presentation settings; model
selection and credentials cannot become global display options.

### 4. Improve errors as part of the API, not just the implementation

**Where it surfaced:** every vignette that teaches a failure has to catch the
condition and extract a field or message manually.

**Source:** `.check_dots()`, `.field_error()` and `.coerce_field()` produce terse
native errors; provider errors use a richer LM15 hierarchy. Nested validation
paths use zero-based indexes in places where R users index with `[[1]]`.

**Recommendation:** make errors say what operation failed, which field is
wrong, what shape is required, and the safe next step. Preserve an appropriate
R call location where it helps. Report nested R access paths with R indexing;
keep canonical `part_index` and batch `index` fields zero-based. Add structured
field/path information for programmatic handling rather than asking callers
to parse a sentence.

**Boundary:** preserve canonical error classes and codes. Messages are not the
wire contract and can improve. Do not echo arbitrary values to make an error
“helpful”; this is particularly important at credential and transport boundaries.

## Everyday composition

### 5. Help analysts extract columns without destroying responses

**Where it surfaced:** *Work with a table of requests* defines `text_or_na()` and
`input_tokens()` just to use `map_chr()` and `map_dbl()`.

**Source:** `response_text()` intentionally returns a string or NULL. A refusal,
a tool call and a failed request must not be collapsed into the same empty
string. Usage counters can also be absent or exact large integers.

**Recommendation:** keep the existing scalar accessor's semantics. Consider an
explicit analysis projection or plural accessor that has a declared output type
and an explicit missing-value policy. A response-table projection should retain
the original response in a list column, separate failure conditions from finish
reasons, and preserve exact counters when a double column cannot represent them.

Proposed design direction, not implemented:

```r
# One row per supplied response; `response` remains a list column.
# Text extraction uses an explicitly chosen missing-value policy.
# Large counts remain exact instead of being forced into doubles.
```

Do not make `complete()` return a tibble, automatically unnest message parts,
turn a refusal into plain text, or fill unknown usage with zero. Those changes
would make the first example shorter by making the data less trustworthy.

### 6. Keep constructor names coherent, without multiplying aliases

**Where it surfaced:** `text()` is convenient, while media and tool content use
`image_part()` and `tool_call_part()`. `text_part()` also exists, with a different
argument name from `text()`. The workflow then combines `message_user()` with
the lower-level `message("tool", parts)`.

**Source:** hand-written helpers in `R/types.R` and generated constructors in
`R/constructors.R`. Both levels are exported by `tools/package-api.py`.

**Recommendation:** publish one recommended path for common use, and clearly
label the exact-type constructors as the lower-level family. Explain which
helpers normalize strings and which accept typed parts only. Keep aliases for
compatibility, but do not add a new synonym each time a tutorial feels verbose.
Align argument names in a planned pre-release change where their meanings are
actually identical; retain distinctions where normalization differs.

Names such as `message()` and `text()` also mask familiar R functions. Use
`lm15::message()` or `lm15::text()` where the context is ambiguous, and mention
the collision. Do not casually rename them to `chat()` or `txt()` against the
shared API family.

### 7. Make appending history a small, explicit value operation

**Where it surfaced:** the follow-up example and the tool round trip both use
`req$messages <- c(req$messages, list(...))` and must remind the reader to keep
`answer$message` intact.

**Recommendation:** consider a helper that accepts a request and explicit
messages, appends them without reconstructing content, validates the result,
and returns a new request. Its name should say “append messages”, not “chat”,
“continue” or “run”. The exact public name needs review.

This would remove repeated structural editing while preserving one visible
request value. It must not send a request, decide which tool to run, truncate
history, summarize messages, or attach the next response automatically. It must
preserve continuation data and caller ordering.

### 8. Do not rearrange `complete()` merely to make a pipe prettier

**Where it surfaced:** the first vignette naturally uses
`complete(client, req) |> response_text()`, but a request-first pipeline needs
an adapter or anonymous function.

**Recommendation:** keep the current client-first order for now. A client is a
resource used to perform an operation; that is a defensible R pattern, not an
automatic design error. The shared API family already uses this mental model.
Improve the small value operations around it before introducing another
completion name or silently reversing arguments.

If request-first dispatch is desired before release, make that an explicit R
API-family decision, with a compatibility plan. Do not hide it in vignette code
or introduce `ask("prompt")` returning only text.

## State, time and resource ownership

### 9. Unify the two meanings of a live “turn”

**Where it surfaced:** *Read streams and live turns without losing the ending*
has to explain both:

- `session$turn(content)` sends content, then returns a raw list of events;
- `turn(session)` sends nothing and returns a view whose `result()` is a
  structured materialized turn.

**Source:** `R/live-session.R` and `R/live-turn.R`.

**Recommendation:** choose one primary meaning matching the shared family:
`turn(session)` is a view over the next turn. If the send-and-collect helper is
kept, give it an explicitly different name or deprecate it through a documented
transition. Do not make the same name switch between sending and reading based
on whether an argument happened to be omitted.

Keep the existing distinction between closing a view and closing a session.
Keep `tool_call`, `interrupted`, `error` and `incomplete` distinct from a
successful `turn_end`. A nicer wrapper must not hide those boundaries.

### 10. Make job operations consistent and their network effects visible

**Where it surfaced:** *Separate files, job snapshots and remote work* needs a
verb table and a scripted extra status reply because `results(job)` contacts
the provider again even after `refresh(job)` found completion.

**Source:** `R/jobs.R`: `job_info()` is local, while `results()` calls
`batch_results()`, which rechecks status and fetches output. `result()` serves
both video jobs and turn views. `refresh`, `wait`, `result`, `results` and
`cancel` mostly branch manually rather than form a consistent S3 family.

**Recommendation:** keep the shared words and the singular/plural distinction,
but make them genuine, predictable methods with clear “reads locally”, “fetches
once”, or “polls” documentation. A snapshot accessor should remain local. A
result fetch should state that it may make multiple HTTP requests. If the
caller may intentionally use a known terminal snapshot, expose that choice
explicitly rather than skipping status checks silently.

`list_models()` versus `file_list()`/`batch_list()` is another naming seam. Fix
reference grouping and discovery first; changing frozen public names requires
more than a stylistic preference.

### 11. Explain shared state and give ownership a uniform shape

**Where it surfaced:** copying `req` preserves an independent value, but
`same_handle <- job` shares the mutable job state. Live sessions, turn views,
registries and credential stores also expose closures or shared state through
slightly different interfaces.

**Recommendation:** distinguish *values* from *resources* consistently in
names, printing and reference pages. Consider scoped helpers for sessions and
listeners which guarantee cleanup while preserving the primary error. Keep
snapshot values independent of later refreshes. Hide implementation fields
such as a job's `$state` from the recommended API; callers should not need to
edit them to do normal work.

Do not attempt to make every connection “immutable”, and do not replace ordinary
request values with a hidden stateful conversation object. Mutable resources
are legitimate; unclear ownership is the problem.

### 12. Use one understandable policy for deadlines and limits

**Where it surfaced:** job wait timeouts, HTTP timeouts, live read timeouts and
collector limits all need separate explanations. A wait deadline does not
interrupt an already-running request. Some nested limits are not exposed by a
single high-level option.

**Recommendation:** document whether each timeout bounds the whole operation,
each read, or the next poll. Validate limits before initiating work. Where
options naturally form a strategy, consider typed option values rather than a
large flat list of loosely related numbers. A future deadline mechanism should
pass the remaining time down to supported transports explicitly.

Do not change a per-read timeout into a whole-operation timeout silently. Do
not imply that timing out, closing a stream or cancelling a local wait cancels
a provider job or reverses a charge. Limits that require implementation edits
to change deserve review as explicit public options, not hidden constants.

## Data boundaries and interoperability

### 13. Make JSON shape construction easier, without “tidying” user payloads

**Where it surfaced:** the tool schema is considerably longer than the tool
function. *Preserve meaning in JSON and media* must teach objects, arrays,
NULL, empty values and the difference from ordinary R vectors.

**Source:** `R/json.R` and `R/types.R`. `json_object()` accepts named dots, whereas
conversion of existing list data has no equally obvious public constructor.
The dots helper rejects empty names, although an empty string is a valid JSON
object key and the reader can retain it.

**Recommendation:** provide clear, explicit conversions for existing object
and array data. Distinguish truly unnamed R input from an explicitly supplied
empty JSON key. Reject duplicate keys, but never repair names with
`make.names()` or tidy data-frame name repair. Consider a small JSON-Schema
construction helper only if it reliably reduces repetition without rewriting
schema meaning or introducing a large schema language of its own.

No automatic factor/date conversion, vector simplification, recursive empty
field removal, or automatic tool schema guessed from R defaults. R type
convenience cannot override exact JSON semantics.

### 14. State the limits of exact numbers in ordinary R operations

**Where it surfaced:** *Work with a table of requests* needs explicit numeric
conversion; *Preserve meaning in JSON and media* shows that an opaque parsed
number is not an ordinary R numeric scalar.

**Source:** `R/integer.R`, `R/json.R` and the limited arithmetic methods. Exact
integers and preserved opaque number tokens are two different representations.

**Recommendation:** give them useful formatting and documented extraction
paths. Keep a clear scalar-only contract unless the package is prepared to
support a complete vector type, including subsetting, concatenation, missing
values and coercion. If vctrs methods are added, they must refuse unsafe casts
rather than quietly convert exact values to doubles. Tidy analysis helpers
should be able to retain exact values in list or character columns.

Do not promise that every arithmetic or summary function works simply because
`+` and comparison work. Do not normalize unknown usage to zero to make a column
numeric. Approximate cost estimates and exact counters also need distinct,
well-explained contracts.

### 15. Clarify the scope of serialization options

**Where it surfaced:** the explicit `include_provider_data = TRUE` example.

**Source:** `as_json()` in `R/json.R` applies this flag to a top-level typed
value. A plain list goes straight to `.json_encode()`, whose nested typed values
are serialized with their defaults. Consequently a top-level flag is not an
obvious recursive instruction for a list of responses.

**Recommendation:** document and enforce the option's scope. Do not silently
ignore an option on unsupported input shapes. Prefer an explicit per-response
operation for a response collection, or a clearly specified collection writer.
Any broader propagation must respect type-specific serialization rules,
including nested batch responses, rather than recursively cleaning data.

The canonical serializer and an analysis/export convenience should not become
indistinguishable. Keeping provider data is a deliberate privacy and size
choice, not a formatting toggle to turn on globally.

## Entry points, dependencies and teaching

### 16. Curate discovery instead of exporting by spelling alone

**Where it surfaced:** a short tutorial sits on top of a large exported surface
containing ordinary calls, low-level builders, credential exchanges, browser
bridge operations and contract-adapter entry points.

**Source:** `tools/package-api.py` exports every top-level function matching a
naming pattern. That makes adding a helper an accidental public API decision.

**Recommendation:** maintain an explicit export policy and a grouped reference
index. Separate “start here”, “inspect/convert”, “resources”, “transport
extension points”, and “advanced credential mechanisms”. Keep currently public
advanced functions until a deliberate compatibility decision says otherwise.
Do not remove useful pure build/parse seams to make the function list look small.

### 17. Split request options from client identity more clearly

**Where it surfaced:** the routing vignette must explain that `api_key` can
actually be a bearer token, AWS credentials, or a function. The `settings` list
has provider-dependent fields and the constructor has many unrelated options.

**Recommendation:** keep `api_key` as a compatible entry point but consider a
clearly named credential strategy for new code, with an explicit exclusivity
rule if both are supplied. Give host settings and transport limits better
structured help or typed option constructors where that prevents mistakes.
Do not expose two settings mechanisms whose precedence a learner must guess.

Any alias or options object must still resolve through the one declared auth
chain. No hidden shared key store, default provider, automatic region guess,
secret persistence or global inference options should be added in the name of
convenience.

### 18. Keep teaching dependencies optional; review native setup costs separately

**Where it surfaced:** the table vignette benefits from tibble and purrr, while
most examples need neither. The package also has a native libcurl requirement
even when a reader initially wants only offline canonical values.

**Recommendation:** keep tibble/purrr in examples or an optional integration
layer, not the core execution path. Knitr/rmarkdown belong to documentation
building. Separately review whether native transport code can be optional or
split without weakening TLS or creating divergent codecs. A secure, explicit
system requirement is preferable to an apparently easy install with an unsafe
connection implementation.

Do not add a whole tidyverse dependency just for printing, and do not conceal
browser restrictions behind a desktop-shaped API. Browser JSON-text methods,
header restrictions, local-file limits and cancellation behavior must remain
explicit.

### 19. Separate strict construction, lenient reading and foreign-format import

**Where it surfaced:** the JSON vignette teaches round trips, while the routing
vignette teaches import of another client's request format. These are not the
same operation, even though both start with JSON-shaped data.

**Source:** constructors reject unknown arguments, `.read_value()` in
`R/serde.R` ignores ordinary unknown canonical fields, and
`request_from_openai_chat()` accounts for foreign keys through mapping,
passthrough or refusal. Unknown content discriminators still reject.

**Recommendation:** make those three boundaries explicit in help and errors.
Encourage typed constructors when authoring new requests. A separate opt-in
linting or strict-input tool could help people editing canonical JSON manually,
but must not change the contract reader's default forward-compatibility rules.
Document closed vocabulary choices directly in function help; keep exact
matching rather than introducing `match.arg()` abbreviations for controls.

Do not promise that every misspelling in JSON raises merely because a misspelled
R constructor argument raises. Conversely, do not make foreign-format import
silently ignore keys just to match the canonical reader's leniency.

## What I would change first

1. Prevent effects from rejected dots; improve names-only diagnostics; validate
   all local client configuration before credential work.
2. Improve privacy-preserving printing and grouped reference help so existing
   objects are discoverable without repeated field spelunking.
3. Resolve the two meanings of `turn` and document one resource-lifetime pattern.
4. Add a narrowly specified message-append helper and optional analysis
   projections while preserving complete canonical values.
5. Make job/resource methods consistent, clarify timeout scopes, and curate
   exports before freezing additional public names.
6. Consider deeper vector, settings and packaging changes only after those
   smaller improvements have a clear compatibility story.

## What I would deliberately keep

- Requests, responses, parts and events remain explicit typed values, not
  automatically flattened tibbles or text-only return values.
- `complete` and `stream` stay the shared concepts. No parallel `ask`, `chat`,
  `run` or `generate` vocabulary for the same operation.
- JSON field names, integer/float distinctions, null/absence distinctions,
  continuation state and opaque payloads retain their contract meanings.
- Requests use ordinary R values and explicit strings. A data-masking prompt
  language or implicit environment capture belongs in an application wrapper,
  if needed, and must still produce a visible request.
- Tool execution, retry policy, conversation truncation, approval and budgets
  remain in the application. A two-call teaching example is not a hidden agent.
- Providers can refuse unsupported fields. No default-field dropping, automatic
  model substitution, or silent paid fallback.
- Recorded/fake replies in the vignettes remain openly labelled. They teach
  the API; they are not fabricated provider-quality evidence.

The strongest tidyverse improvement is not to make lm15 look larger or more
magical. It is to make its small, honest foundation easier to inspect, combine
and use correctly.
