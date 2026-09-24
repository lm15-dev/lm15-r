#!/usr/bin/env python3
"""Write package exports and the shared API help index from public functions."""
import pathlib
import re
root = pathlib.Path(__file__).resolve().parents[1]
exports = set()
methods = []
for path in sorted((root / 'R').glob('*.R')):
    source = path.read_text()
    exports.update(re.findall(r'^([a-z][a-z0-9_]*) <- function\(', source, re.M))
    for generic, cls in re.findall(r'^(print|str|conditionMessage|Ops|as\.double|as\.character|complete|stream)\.([A-Za-z0-9_]+) <- function\(', source, re.M):
        methods.append((generic, cls))
methods += [('$', 'lm15_value'), ('$<-', 'lm15_value'), ('[[<-', 'lm15_value'), ('[<-', 'lm15_value'), ('$', 'lm15_json_object')]
namespace = ['# Written by tools/package-api.py.', 'importFrom(utils,str)', 'importFrom(stats,setNames)', 'useDynLib(lm15, .registration=TRUE)']
namespace += [f'export({name})' for name in sorted(exports)]
namespace += [f'S3method("{generic}",{cls})' for generic, cls in sorted(set(methods))]
(root / 'NAMESPACE').write_text('\n'.join(namespace) + '\n')
(root / 'man').mkdir(exist_ok=True)
documented = set()
for page in (root / 'man').glob('*.Rd'):
    if page.name != 'lm15.Rd':
        documented.update(re.findall(r'\\alias\{([^}]+)\}', page.read_text()))
help_text = r'''\name{lm15}
\alias{lm15}
ALIASES
\title{Provider-neutral foundation model conversations}
\description{
Construct typed requests and content parts, translate provider wire formats,
and obtain typed responses and incremental stream events. This development
version is implementation work in progress, not a claim of conformance.
}
\details{
Use \code{new_router()} for model-name routing or \code{new_lm("openai")}
for a provider directly. Construct a \code{request()} with a list of
\code{message_user()}, \code{message_assistant()}, and \code{message_tool()}
values. Settings are constructed with \code{config()}; optional arguments
following dots must be named, and unused arguments are errors.

\code{complete(lm, request)} returns a Response. The R streaming idiom is
\code{stream(lm, request, on_event)}: the callback receives typed events as
they arrive, and the function returns the assembled Response after completion.
Callbacks execute on the R thread. An interruption releases the HTTP transfer;
it does not guarantee that provider generation or billing has stopped.
\code{replay_stream()} decodes a recorded stream without networking.

\code{as_json()} and \code{from_json(text, kind)} use canonical JSON.
\code{json_object()} and \code{json_array()} distinguish empty objects from
empty arrays. Named lists are objects, unnamed lists are arrays, and atomic
values must be length-one scalars. Use lists for arrays, not vector guessing.
\code{NULL} denotes absent optional fields; zero and FALSE are actual data.
NA, non-finite numbers, factors and arbitrary R objects are not JSON values.
Typed integer fields preserve large integers as decimal values. Use
\code{integer_value("9007199254740993")} to construct one without rounding.
Large values support exact addition, subtraction and comparisons; coercion to
an R double refuses when it would lose precision. Opaque numbers read from
JSON retain their original decimal tokens.

Canonical types have snake-case constructors matching the contract's type
names: \code{tool_call_part()}, \code{cache_config()},
\code{image_generation_request()}, etc. Content helpers \code{text()},
\code{thinking()} and role-specific message constructors cover common calls.
\code{validate()} revalidates an object. Ordinary dollar and bracket
replacement of canonical objects also validates before returning a new value.

\code{build_request()} and resource \code{*_op_build()} functions construct
wire requests without sending them. Credential providers are invoked at
request-build time. Explicit raw wire values contain credentials: do not log
or save them. Printing and structure displays hide them.
\code{parse_response()} and \code{*_op_parse()} functions transform recorded
bodies. Model listing is advisory and does not alter request-building rules.

File, cache, batch, image, speech and video operations are supported where the
provider access policy allows them. Job handles returned by \code{batch()}
and \code{video_generate()} retain a snapshot. \code{job_info()} reads it;
\code{refresh()} fetches a new snapshot; only \code{wait()} polls. Local wait
timeouts have class \code{lm15_wait_timeout}, distinct from provider timeouts.
A wait's deadline bounds polling, not an already-running HTTP request.

\code{api_key()}, \code{bearer_token()} and \code{aws_credentials()} create
credential values. A credential callback may return a value or a key string.
\code{sigv4_sign()}, \code{jwt_rs256()} and PKCE helpers use the optional
openssl package. Credential serialization is deliberately explicit and is
not redacted. Stored logins renew under a cross-process lock and are written
privately and atomically. Expired subscriptions never fall back to paid keys.
Cloud credential providers cache tokens per client identity. The offline
\code{explain_auth()} report never contacts cloud endpoints. \code{login("xai")}
runs the owned device-code flow; other providers give their CLI or console hint.
\code{live()} returns a session with send, next_event, turn, interrupt, and close
functions. Its codec is shared with recorded live-conversation replay.

The browser module at \code{system.file("browser/lm15.mjs", package="lm15")}
uses browser fetch and the R codecs through webR. It requires an initialized
webR worker with this package and jsonlite already installed. It does not use
curl, local CLI credentials, or persistent browser credential storage.
Provider CORS rules and forbidden browser headers still apply.

See the package README and IMPLEMENTATION.md for current limitations and
verification status. Tests cover the installed package, offline contract
transformations, credential lifecycle, live sessions and the browser bridge.
The WebAssembly build has been exercised in real Chromium with local HTTP
and WebSocket test servers. Desktop live TLS is separately tested for untrusted
certificates, hostname mismatches, valid trust, fragmentation and frame limits.
Windows and macOS release verification remain separate from local Linux results.
}
\value{
Constructors return typed S3 list values. Client and router constructors return
configuration objects. Network calls return the canonical response type for
the endpoint. Deletion returns invisible NULL. JSON serialization returns one
string. Pure parsers return canonical values or lists of values. Conditions
inherit from LM15Error and the corresponding contract error classes.
}
\examples{
r <- request("openai:gpt-4.1-mini", list(message_user("Hello")),
             config = config(max_tokens = 100L))
as_json(r)
# Networking is explicit, not part of these examples:
# answer <- complete(new_router(), r)
# response_text(answer)
}
'''.replace('ALIASES', '\n'.join('\\alias{' + name + '}' for name in sorted(exports - documented)))
(root / 'man/lm15.Rd').write_text(help_text)
