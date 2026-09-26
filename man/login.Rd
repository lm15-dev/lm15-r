\name{login}
\alias{login}
\alias{logout}
\alias{status}
\alias{connections}
\alias{configure}
\alias{set_api_key}
\alias{cancel_login}
\alias{verify}
\alias{request_auth}
\alias{login_providers}
\alias{login_methods}
\title{Sign in and manage saved connections}
\description{

Sign in to a subscription (xAI, Claude, ChatGPT, GitHub Copilot, Kimi Code,
Meta, OpenRouter) or save a key, a key's environment variable name, a cloud
identity or a local server, once, in a private file every lm15 language
reads. A router given \code{auth = } then uses the saved connection, renewing
it when due.

}
\usage{
login(provider, method = NULL, ..., auth = local_auth(), ui = NULL,
  settings = list(), answers = list(), replace = NULL, lifetime = 15 * 60,
  allow_unverified = FALSE)

logout(target, ..., auth = local_auth())

status(provider, ..., auth = local_auth())

connections(..., auth = local_auth())

configure(provider, method, ..., answers = list(), settings = list(),
  replace = NULL, auth = local_auth())

set_api_key(provider, key, ..., replace = NULL, auth = local_auth())

cancel_login(provider, ..., auth = local_auth())

verify(provider, ..., auth = local_auth())

request_auth(provider, ..., pinned = NULL, auth = local_auth())

login_providers(..., auth = local_auth())

login_methods(provider, ..., auth = local_auth())

}
\arguments{
\item{provider}{A provider id, such as \code{"openai"}, \code{"anthropic"} or \code{"xai"}. \code{providers()} lists them; an underscore is read as a hyphen.}
\item{method}{The login method id (see \code{login_methods()}); \code{NULL} asks when several are available.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{auth}{The sign-in scope: \code{local_auth()} (the default: the private credentials file), \code{memory_auth()}, or \code{new_auth()}.}
\item{ui}{How to talk to the person: \code{terminal_ui()} (the default in an interactive session) or your own list of \code{prompt} and \code{notify} functions.}
\item{settings}{Host settings for a cloud provider, such as \code{list(region = "us-east-1")} or \code{list(project = "my-project")}. A setting given here wins over the environment.}
\item{answers}{Answers to the method's fields, such as \code{list(name = "GEMINI_API_KEY")}.}
\item{replace}{The id of the saved connection this one replaces. Without it, a provider that already has a connection is refused.}
\item{lifetime}{Seconds the sign-in may take.}
\item{allow_unverified}{Allow methods that exist but have no live receipt yet.}
\item{target}{A provider id or a connection id.}
\item{key}{The API key, saved as typed.}
\item{pinned}{\code{c(connection_id, identity_generation)} a request must still match, or \code{NULL}.}
}
\details{

A saved connection is never overwritten without \code{replace}. \code{logout()}
forgets it locally (it contacts no provider) and keeps a marker, so a signed-out
subscription is not replaced by an environment key after a restart.
\code{status()} and \code{connections()} read the store only; \code{verify()}
lists models as a check (possibly metered). A renewal that may have spent a
one-use token is reported \code{"indeterminate"} and never retried. Methods
marked unverified need \code{allow_unverified = TRUE}.

The file is \code{LM15_CREDENTIALS_PATH}, else
\code{$XDG_CONFIG_HOME/lm15/credentials.json}, else
\code{~/.config/lm15/credentials.json}. It is written with mode 0600 and
atomically, under a lock shared with the other lm15 languages. It is not
encrypted.

}
\value{
A Connection (\code{login()}, \code{configure()}, \code{set_api_key()}), a status, a list of connections, a logout result, \code{"cancelled"}/\code{"complete"}/\code{"none"} for \code{cancel_login()}, a verification, request authentication, or descriptors.
}
\seealso{
\code{\link{connect}}, \code{\link{local_auth}}, \code{\link{explain_auth}}
}
\examples{

auth <- memory_auth()                      # a scope that lives in this R session
key <- set_api_key("openai", "sk-example", auth = auth)
status("openai", auth = auth)$usability
configure("gemini", "env", answers = list(name = "GEMINI_API_KEY"), auth = auth)
length(connections(auth = auth))
logout("openai", auth = auth)
\dontrun{
login("xai")          # a device code to approve in your browser
router <- new_router(auth = local_auth())
}

}
