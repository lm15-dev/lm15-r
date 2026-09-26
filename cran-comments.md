## Submission

First submission of lm15, an R implementation of a provider-neutral
interface to language model APIs. The same contract is implemented in
Python, TypeScript, Rust and Go.

## Test environments

* Linux (NixOS), R 4.6.1: `R CMD check --as-cran`
* Linux, R 4.6.1, with only the packages in Imports installed
  (`_R_CHECK_FORCE_SUGGESTS_=false`)

## R CMD check results

0 errors | 0 warnings | 1 note

* New submission.

## Notes for the reviewer

* Examples, tests and vignettes make no network requests: they use the
  package's scripted transports (`fake_transport()`, `fake_lm()`).
  Examples that need a person or a provider account are in `\dontrun{}`.
* Nothing is written outside `tempdir()`; the default credentials location
  (`~/.config/lm15`) is used only when a user signs in.
* libcurl with WebSocket support is optional: `configure` installs the
  package without the native WebSocket transport when it is absent.
