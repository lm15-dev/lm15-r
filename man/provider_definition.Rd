\name{provider_definition}
\alias{provider_definition}
\title{Declare a provider lm15 does not list}
\description{

A gateway, a hosting service lm15 has not tested, a second OpenAI-compatible
vendor: describe it once and pass it to \code{new_router(providers = list(...))}.
The router then treats it like a built-in provider, by name
(\code{"id:model"} and each alias), with \code{api_keys}, \code{base_urls} and
the declared environment variables, in that router only. Nothing is registered
globally. \code{new_lm()} also accepts a definition directly.

}
\usage{
provider_definition(id, ...,
  dialect = c("openai-chat", "openai-responses", "anthropic"), base_url,
  env_keys = character(), compat = list(), supports = c("complete", "stream"),
  auth_scheme = c("bearer", "x-api-key"), headers = character(),
  aliases = character(), placeholder_key = NULL, console_url = NULL, note = "")

}
\arguments{
\item{id}{The provider name: lower case, words joined by \code{-} (\code{"nebius"}). It is what goes before \code{:} in a model string.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{dialect}{The wire the server speaks: \code{"openai-chat"} (Chat Completions, the usual one), \code{"openai-responses"} or \code{"anthropic"} (Messages).}
\item{base_url}{The server's API root, such as \code{"https://api.tokenfactory.nebius.com/v1"}.}
\item{env_keys}{Environment variables that may hold the key, read in order.}
\item{compat}{How the server differs from the dialect's defaults: a named list of knobs (lm15's compat names, such as \code{list(max_tokens_field = "max_tokens")}) or the name of a preset lm15 knows. An empty list means the dialect's defaults; an unknown knob name is an error.}
\item{supports}{What the server offers: any of \code{"complete"}, \code{"stream"}, \code{"models"}, \code{"files"}, \code{"batches"}, \code{"images"}, \code{"speech"}, \code{"video"}, \code{"live"}, \code{"responses_api"}, \code{"caches"}.}
\item{auth_scheme}{How the key travels: \code{"bearer"} (an Authorization header) or \code{"x-api-key"}.}
\item{headers}{A named character vector of headers sent on every request.}
\item{aliases}{Extra spellings accepted before \code{:}; never emitted.}
\item{placeholder_key}{For a keyless local server, the key to send when none is configured. Not combined with \code{env_keys}.}
\item{console_url}{Where a person gets a key.}
\item{note}{One line describing the provider.}
}
\details{

A declaration is not a receipt. lm15's own providers are pinned from live
captures; a declared provider is your word, and \code{resolve()} says so with
\code{declared = TRUE}. A key-based server only: a subscription login or a
cloud identity chain needs lm15's own support. When a declared provider gains
lm15 support, its name becomes a built-in one and the declaration is refused:
delete it.

}
\value{
A provider definition for \code{new_router(providers = )} or \code{new_lm()}.
}
\examples{

nebius <- provider_definition(
  "nebius",
  base_url = "https://api.tokenfactory.nebius.com/v1",
  env_keys = "NEBIUS_API_KEY",
  supports = c("complete", "stream", "models"),
  aliases = "tokenfactory"
)
router <- new_router(providers = list(nebius), env = c(NEBIUS_API_KEY = "example"))
resolve(router, "tokenfactory:deepseek-ai/DeepSeek-R1-0528")

}
