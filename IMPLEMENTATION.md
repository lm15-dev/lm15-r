# Implementation and verification record

lm15 for R implements the lm15 contract at [`CONTRACT_PIN`](CONTRACT_PIN)
(`fe5cdf9`, the commit Python 1.1.0, TypeScript and Rust 1.0.0-rc.2 and Go
v1.1.0-rc.2 pin). [`PYTHON_REFERENCE`](PYTHON_REFERENCE) names the Python
checkout used for comparisons outside the corpus (1.1.0, `6aea565`).

## Results — 2026-09-26

| Check | Result |
|---|---|
| Shared contract, all 18 directions, at the pin (`tools/check-contract.py`) | **1,788 pass, 0 fail**; 52 upstream skips (cases with no canonical golden), the same as every SDK |
| Managed sign-in runs (`managed` direction) | 43 of 43 |
| Mixed-language store runs (`tools/managed_crossrun.py python r typescript rust go`) | every rotation of 3 scenarios, and concurrent renewal in all 25 ordered pairs: one refresh per pair |
| Comparisons with Python outside the corpus (`--compare-python`) | 30 agree; 2 known differences (finding F1) |
| R unit tests (`testthat`) | 331 pass |
| `R CMD check --as-cran` with LaTeX, HTML Tidy and checkbashisms (`nix develop .#cran`) | 1 NOTE: "New submission" |
| `R CMD check` with only the required packages installed | OK |
| Live, against a real vLLM 0.25.1 server | chat, streaming, model listing, `UnsupportedModelError`, `AuthError` without the key in the message, and the judgment fallback (the server ignores `logprob_token_ids`, as recorded in the contract) |

Directions: request 464, response 353, stream 52, error 104, serde 129,
auth 60, token 43, models 44, live 24, files 48, batch 41, generation 20,
video 27, cache 11, router 22, ingest 216, mapping 87, managed 43.

## What changed on 2026-09-26 (from 1,498 of 1,788)

* The four open-model hosts (DeepInfra, Together AI, Fireworks AI, Parasail),
  copied as data from the reference tables; `reasoning_off = "lowest"`;
  bare-array model catalogs; flat `cached_tokens`; Parasail's no-such-model form.
* Judgments (MAP-14): `DataPart`, `config(probabilities = )`, the TypeSafe
  provider, the Anthropic and Gemini schema rewrites, and the vLLM
  candidate-sequence driver with its fallback.
* MAP-16: a schema reaches Gemini in the field that can carry it.
* Google Cloud: the project from gcloud's configuration, the credential file,
  the ADC file or the metadata server, each reported with its origin; Vertex
  API keys; token-shaped strings (`ya29.`, JWTs) sent as bearer tokens.
* R3: an unusable or signed-out xAI login blocks the ambient `XAI_API_KEY`.
* Rate-limit diagnostics on errors and in-stream `http_response`;
  `logprobs_complete`; `CollectionLimitError`.
* Managed sign-in (AUTH-12–26), `connect()`, bound clients, named cloud
  identities.

## Defects fixed along the way

* **The credentials lock did not exclude the other languages.** R locked with
  `fcntl` record locks (the filelock package); every other lm15 SDK uses
  `flock`, which Linux keeps separate. An R process and a Python process
  could renew one login at the same time and spend a one-use refresh token
  twice. R now uses `flock` (POSIX) and `LockFileEx` on byte 0 (Windows) on the
  same lock file name as every SDK (first 32 hex digits of SHA-256).
* **The JSON reader was quadratic.** It copied the rest of the text for every
  string and used character offsets, which R computes from the start on
  non-ASCII text. Together's 445 KB model catalog took 83 s; it takes 0.15 s.
* **The curl transport set `headerfunction`**, which the curl package refuses,
  so every real request failed (the website's docs test worked around it).
* A callback error return was accepted without its `state`; AUTH-18 requires
  the state check on error returns too.
* `message()` and `text()` masked `base::message()` and `graphics::text()`;
  they are now `new_message()` and the unexported `text()` (`text_part()` is
  the exported constructor).
* `configure` failed the install without libcurl ≥ 7.86; it now installs
  without the native WebSocket transport and says so when a live session is
  opened.

## Findings for the contract

**F1 — a replayed judgment answer is dropped on the OpenAI wires.** Replaying
an assistant message that holds a data part (the answer to a judgment) on
`openai` or `openai-chat`: Python 1.1.0, TypeScript and Go omit the part from
the request and record nothing; Rust and R send its compact JSON as assistant
text, as types.md prescribes for a data part on a text-only wire and as
Python does on Anthropic and Gemini. Rule 4 of the port playbook forbids an
unrecorded drop. The corpus has no case for it. Reproduce with
`python3 tools/check-contract.py --direction serde --compare-python` (probes
`openai-data-parts-everywhere`, `openai-chat-data-parts-everywhere`). Needs a
`changes/` entry and a pinned case; R keeps sending the text meanwhile.

## Stated deviations

The README's table lists where R's API differs from the family's and why:
client-first S3 generics, callback streams instead of iterators, no async,
`new_message()`, `text_part()`, reader functions instead of properties,
functions over an Auth scope, and a loopback sign-in that is interrupted to
paste the return (R cannot race the listener against a prompt on one thread).

## Remaining before calling every platform verified

1. **Windows and macOS.** Only Linux has run. The Windows lock
   (`LockFileEx`), atomic replacement and permissions, and both platforms'
   libcurl builds, run in CI (`.github/workflows/check.yml`) once pushed.
2. **webR.** The browser bridge was verified in Chromium on 2026-09-13; it was
   not rebuilt after these changes. `curl`, `openssl` and `askpass` are now
   imports; all three are in the webR repository, but `tools/build-webr.sh`
   builds only jsonlite and lm15 and needs updating before the next browser build.
3. **Paid providers.** No paid provider has been called from R. The other
   SDKs' live smoke covered the same wire shapes; R's are pinned by the corpus.

## Reproduce

From `lm15-r/`, with `lm15-contract` checked out at the pin beside it:

```sh
nix develop -c python3 tools/check-contract.py                  # all directions
nix develop -c python3 tools/check-contract.py --direction serde --compare-python
nix develop -c Rscript -e 'testthat::test_local()'
nix develop -c Rscript tools/document.R <library with lm15 installed>   # man/*.Rd
nix develop .#cran -c bash -c 'R CMD build . && R CMD check --as-cran lm15_1.0.0.tar.gz'
```

Mixed-language store runs: add `"r": {"command": ["Rscript", "--vanilla",
"exec/lm15-vet.R"], "cwd": "../lm15-r"}` to a copy of the contract's
`harness/shims.json`, then `python3 tools/managed_crossrun.py python r
typescript rust go`.
