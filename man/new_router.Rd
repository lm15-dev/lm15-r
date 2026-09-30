\name{new_router}
\alias{new_router}
\alias{new_lm}
\alias{providers}
\alias{canonical_provider}
\alias{resolve}
\alias{router_lm}
\alias{transport_curl}
\title{Clients, routers and transports}
\description{

A router picks the provider from the model string and builds a client for it
on first use. A client talks to one provider. Constructing either reads no file
and contacts no server; credentials are resolved when a request is sent.

}
\usage{
new_router(..., api_keys = list(), base_urls = list(), settings = list(),
  catalog = list(), rules = NULL, env = NULL, transport = NULL,
  live_connect = NULL, adaptations = "note", auth = NULL, credentials = list())

new_lm(provider, ..., api_key = NULL, credential = NULL, base_url = NULL,
  compat = NULL, settings = list(), transport = NULL, env = NULL,
  account_id = NULL, clock = Sys.time, credentials_path = NULL,
  live_connect = NULL, adaptations = "note")

providers()

canonical_provider(name)

resolve(router, model, ...)

router_lm(router, model, ...)

transport_curl(..., timeout = 120, connect_timeout = 30,
  max_response_bytes = 128 * 1024^2)

}
\arguments{
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{api_keys}{Explicit credentials by provider: \code{list(openai = "sk-...")}. A string is an API key; \code{bearer_token()} and credential functions are also accepted. An explicit entry wins over everything else.}
\item{base_urls}{Address overrides by provider.}
\item{settings}{Settings by provider (for new_router), or for this client (for new_lm): a cloud provider's host settings, such as \code{list(region = "us-east-1")}, or a subscription provider's \code{client_version} (the Claude Code or Codex release it names; \code{LM15_CLAUDE_CODE_VERSION} and \code{LM15_CODEX_CLIENT_VERSION} when a router reads the environment). A setting the provider does not read is an error.}
\item{catalog}{Model metadata used to route bare model names: a list of \code{model_info()} values or a registry from \code{new_model_registry()}.}
\item{rules}{Routing rules as a list of \code{c(prefix, provider)}; \code{NULL} uses the built-in ones (\code{claude-} to Anthropic, \code{gpt-} to OpenAI, ...).}
\item{env}{A named character vector read instead of the process environment. \code{NULL} reads the environment; \code{character()} reads nothing.}
\item{transport}{A function that sends one wire request and returns \code{list(status, headers, body)}; see \code{transport_curl()} and \code{fake_transport()}. \code{NULL} uses curl.}
\item{live_connect}{A function that opens live sessions, replacing the native WebSocket connector.}
\item{adaptations}{What to do when the wire cannot take a setting: \code{"note"} (adapt and record it on the response), \code{"silent"} (adapt, record nothing) or \code{"refuse"} (raise instead).}
\item{auth}{A sign-in scope (\code{local_auth()}, ...) whose saved connections this router uses, or \code{NULL}. With a scope, environment keys are never used.}
\item{credentials}{Named cloud identities by provider: \code{list(bedrock_chat = "cli")}. One of \code{"platform"}, \code{"workload"}, \code{"environment"}, \code{"cli"}.}
\item{provider}{A provider id, such as \code{"openai"}, \code{"anthropic"} or \code{"xai"}. \code{providers()} lists them; an underscore is read as a hyphen.}
\item{api_key}{The credential: a key string, \code{api_key()}, \code{bearer_token()}, \code{aws_credentials()}, or a function returning one (called for every request). \code{NULL} uses the provider's declared chain.}
\item{credential}{A named cloud identity (\code{"platform"}, \code{"workload"}, \code{"environment"} or \code{"cli"}) instead of walking the cloud chain.}
\item{base_url}{The server's address, overriding the provider's.}
\item{compat}{For servers speaking another provider's wire (Chat Completions, Responses, Messages): a preset name such as \code{"ollama"} or \code{"groq"}, or a policy list.}
\item{account_id}{The ChatGPT account id for \code{openai-codex}, when it cannot be read from the token.}
\item{clock}{A function returning the current time; for tests.}
\item{credentials_path}{Where the subscription login is stored, for providers that use one.}
\item{name}{A provider name.}
\item{router}{A router from \code{new_router()}.}
\item{model}{A model string.}
\item{timeout}{Seconds for the whole transfer.}
\item{connect_timeout}{Seconds to connect.}
\item{max_response_bytes}{The largest reply accepted.}
}
\details{

Credentials, in order: an explicit \code{api_keys} entry or \code{api_key};
then, by provider, a subscription login (xAI, Claude Code, Codex), the
provider's environment variables (\code{OPENAI_API_KEY}, ...), or the cloud's
own chain (Azure, AWS, Google Cloud). A failed or signed-out subscription is
never replaced by a paid key on its own. \code{\link{explain_auth}()} shows
the walk.

\code{resolve()} says which provider and model a string routes to, without
sending anything. \code{router_lm()} returns the client a router would use.
The curl transport never follows redirects and never retries.

}
\value{
A router, a client, a transport function, a character vector of provider ids, or for \code{resolve()} a list with \code{provider}, \code{model} and \code{source}.
}
\examples{

router <- new_router(api_keys = list(openai = "sk-example"))
resolve(router, "gpt-5-mini")
resolve(router, "groq:llama-3.3-70b-versatile")
head(providers())
local <- new_lm("ollama")  # a keyless local server at its default address

}
