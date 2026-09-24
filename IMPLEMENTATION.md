# Implementation and verification status

This independent R implementation targets `CONTRACT_PIN`. `PYTHON_REFERENCE`
names the reference snapshot used for comparisons. The contract and Python
working trees have not been modified.

## Contract status — 2026-09-24

`CONTRACT_PIN` now names lm15-contract `b87e434` (2026-09-24), the version
Python, TypeScript and Rust pass in full. R passes 1,397 of its cases and fails
43; the CI contract job is red until these are ported. They are unported
features, listed so the red job reads as a to-do list:

| Cases | What R does not have yet |
|---|---|
| 36 | Judgments (MAP-14): `DataPart`, `config$probabilities`, the TypeSafe provider (`jev-*`, 19 of the 36), judgment schemas answered as data |
| 1 | R3: an expired-unrenewable or signed-out xAI login blocks `XAI_API_KEY` (`xai-unusable-login-blocks-env`) |
| 1 | `CachedPrefix$provider` (2026-09-20) |
| 2 | `logprobs_complete` on responses and deltas |
| 2 | `ErrorDetail$http_response` diagnostics (2026-09-19) |
| 1 | the live-collection limit error's serde form |

At the previous pin (`cfed007`, 2026-09-11) R failed 5 cases, all because it
already implements MAP-15 (a provider's "no such model" is
`UnsupportedModelError`), which that contract predates.

## Latest verified results — 2026-09-13

| Check | Result |
| --- | --- |
| Shared contract, all 16 directions | **1,380 pass, 0 fail, 2 upstream skips** |
| Additional request comparisons with Python | **12 pass** |
| Native R unit assertions | **252 pass, 0 failures/warnings/skips** |
| Node browser-bridge tests | **8 pass** |
| Compiled package in real Chromium/webR | **18 assertions pass** |
| Native WebSocket/TLS integration | All checks pass; details below |
| Source build, install, installed-package tests, `R CMD check --no-manual` | **Status: OK**, no warnings or notes |

The two shared-runner skips remain visible: `openai.computer_use` lacks a
canonical request and a response golden. They are incomplete upstream cases,
not implementation skip rules, and are not counted as successes.

The native environment is pinned by `flake.lock`: R 4.6.1 and a
WebSocket-enabled libcurl 8.22.0. The browser artifact uses the official webR
cross-compilation image pinned by digest in `WEBR_IMAGE`, R 4.6.0 and the
checksummed jsonlite source in `tools/webr-sources.json`.

### Real browser verification

`tools/test-webr.mjs` installs the actual compiled R package and jsonlite into
webR in Chromium 152. It tests completion, incremental streaming, model listing,
large-integer JSON-text paths, error redaction, live WebSockets, rejection of
browser-forbidden authentication headers, cancellation during body reads and
idle live waits, and state cleanup. It checks that the installed package
contains exactly the JavaScript bridge being exercised.

The latest run also used an operating-system network namespace with only
loopback enabled. Six HTTP requests and live WebSocket exchanges went to local
fixture servers; there were **zero external requests**. This is not merely a
simulated R worker. The earlier Node mock tests are retained as faster tests.

Browser artifacts are in `dist/webr/`; the deployable package repository is
`dist/webr/repo/bin/emscripten/contrib/4.6/`. The matching runtime is in
`dist/webr/runtime/`.

### Native transport security verification

Source review found that the previous R `websocket` dependency did not enable
TLS certificate verification. It was removed rather than trusted with keys.
The replacement in `src/websocket.c` uses libcurl with TLS 1.2 minimum,
certificate-chain verification, hostname verification and no redirects.
An explicit CA bundle supports private services; there is no insecure bypass.

`tools/test-native-websocket.mjs` verifies with an actual local TLS server:

- an untrusted certificate is refused before HTTP credentials are sent;
- a trusted certificate for the wrong hostname is likewise refused;
- an explicitly trusted matching certificate succeeds;
- fragmented messages, large messages and ping/pong control frames work;
- oversized incoming frames are refused;
- unsuccessful handshake bodies cannot print an echoed credential;
- a rejected HTTP authentication handshake has the typed authentication error;
- connections can be closed repeatedly without replaying requests.

## Work completed since the previous checkpoint

- Model registries, data-file catalog discovery, explicit catalog loaders,
  advisory pricing and integration with router lookup.
- Provider-client reuse with identity/configuration invalidation. Rotating
  credentials remain per-request calls, not cached credential results.
- Chat Completions migration routing, including the copied foreign-prefix
  table, shared keys, client-option refusals and the correct default OpenAI
  Chat Completions endpoint.
- Automatic realtime completion/streaming, with pure request construction and
  stream assembly shared with the existing canonical types.
- Loopback OAuth callbacks with path/state checks and duplicate-parameter
  refusals, exercised through a real local HTTP listener.
- Protected credential-store entry reads, mutation and removal.
- Fake models and HTTP transports, recorded-request access, and lossless
  response-to-event conversion where the delta vocabulary permits it.
- S3 completion/streaming extension points for application-defined clients.
- Live turn views with partial snapshots, text/audio/tool results, field-wise
  usage accumulation, tool-call boundaries, and event/memory limits.
- Real WebAssembly package and dependency builds, a package repository,
  browser live sessions and real-browser integration tests.
- Verified native TLS transport, replacing an unsafe dependency assumption.
- Additional API reference pages and CI jobs for native package checks,
  contract comparisons, native TLS integration and webR/Chromium execution.

## Remaining release gates — not a 100% certification

1. **Windows and macOS execution.** Only Linux and Chromium on Linux have been
   executed here. The cross-platform workflow has been written but not run.
   Windows file replacement/permissions/locking and both platforms' libcurl
   builds still need real execution, including TLS tests with WebSocket support.
2. **Repository/CI publication.** `lm15-r/` is not yet a Git checkout. The GitHub
   CLI could not resolve `lm15-dev/lm15-r`. Publishing or attaching the package
   to an appropriate repository needs the owner's approval before remote CI
   can be run; no repository was created or pushed by this work.
3. **Complete API parity sign-off.** The shared contract is green and the
   implementation areas above have tests, but the Python suite has not been
   translated test-for-test. The supported R equivalents and deliberate
   refusals below still need final review against the intended release scope.
   Test counts are evidence for the checks performed, not proof of every
   possible combination or every provider deployment.
4. **Independent security review.** The transport review already found and
   fixed a real dependency issue. Windows permissions and the remaining cloud
   source variants deserve independent review before production certification.

No actual provider login, paid inference request or cloud metadata service was
contacted. The contract uses recorded provider exchanges; fresh provider smoke
tests are separate from the offline release gates and require authorization.

## Deliberate boundaries and trade-offs

- **R protocol/concurrency idioms:** `complete` and `stream` are S3 generics.
  Native streaming uses callbacks on the R thread rather than Python iterator
  classes. Browser operations are asynchronous. The pure transformations are
  shared across these surfaces.
- **Catalog discovery:** R packages ship `inst/lm15/model-catalog.json`, or the
  application supplies named loader functions. Discovery does not execute
  arbitrary package startup code. Metadata never rewrites provider requests.
- **No silent loss:** realtime calls refuse ordinary config fields without a
  supported live mapping. Response-to-event conversion refuses fields such as
  image paths/detail that the delta vocabulary cannot preserve. Repeated final
  realtime tool arguments are reconciled rather than duplicated. Replacing
  registry entries removes stale aliases instead of retaining old metadata.
- **No silent identity fallback:** malformed configured GCP files and partial
  AWS environment credentials fail closed rather than selecting another
  principal. These cases have local regression tests; the contract, not an
  implementation's permissive behavior, takes precedence.
- **No retries or reconnect replay:** an ambiguous paid send is not repeated.
  Local cancellation does not guarantee stopped provider generation or billing.
- **Credential storage:** renewal runs under a canonical-path cooperative lock
  with a second read after acquisition. A slow renewal blocks sibling writers
  up to the lock timeout. Foreign tools do not take this lock. POSIX writes use
  private temporary files, file synchronization, atomic replacement and parent
  directory synchronization. Windows uses exclusive creation, synchronized
  writes and write-through replacement; its inherited permissions still need
  platform-specific review.
- **Native networking requirements:** libcurl must be at least 7.86 and built
  with WebSocket support. The Nix shell chooses one matching library for the
  whole R process. Other systems need equivalent development/runtime libraries.
  Receive operations are pull-based and preserve partial frames across read
  timeouts. `openssl`, `filelock`, `processx`, `xml2`, `httpuv` and `later` supply
  their respective optional platform mechanisms; missing dependencies fail
  explicitly, without homemade cryptography or unprotected storage.
- **Memory/precision:** HTTP bodies default to 128 MiB; SSE lines/events to
  1/8 MiB; live messages to 32 MiB; turn collection to 1,024 events and 128 MiB.
  Exact large integers use decimal values with addition, subtraction and
  comparisons; unsafe ordinary numeric conversion is refused. Browser object
  methods reject unsafe integers; JSON-text methods preserve exact tokens.
- **Browser restrictions:** default WebSockets work only when required auth
  can be expressed by the browser. Header-authenticated endpoints require an
  explicit connector or server-side client. No credentials are persisted in
  browser storage, and desktop CLI files/metadata sources are not exposed.
- **Build tooling:** the image-pinned rwasm low-level builder is used to avoid
  network-dependent package resolution. It can print a non-fatal host dependency
  lookup error in offline mode. Host jsonlite is checked first; both target
  packages are compiled and then actually loaded in Chromium. Development can
  reuse already-built dependencies from the same image; full builds rebuild
  them by default.

The reference snapshot also explicitly refuses AWS DPoP login renewal (use
`aws login`), Azure Service Fabric thumbprint identity, GCP AWS external-account
sources and certain uncommon GCP credential types, and binary AWS event-stream
framing. Those refusals are not hidden R fallbacks.

## Reproduce

From `lm15-r/` with the pinned contract/reference as sibling directories:

```sh
nix develop -c Rscript -e 'testthat::test_local()'
nix develop -c node --test tests/browser/*.test.mjs
nix develop -c python3 tools/check-contract.py --compare-python
npm ci --ignore-scripts
nix develop -c node tools/test-native-websocket.mjs
nix develop -c bash tools/build-webr.sh
nix develop -c node tools/test-webr.mjs
nix develop -c R CMD build .
nix develop -c R CMD check --no-manual lm15_0.0.1.9000.tar.gz
```

`CHROMIUM_BIN` selects a browser executable outside the default NixOS location.
`CONTAINER_ENGINE=docker` selects Docker instead of Podman. Linux can additionally
run the browser test through `unshare -rn` after enabling its loopback interface.
Logs and machine-readable reports are saved under `test-results/` and are not
included in the R package.
