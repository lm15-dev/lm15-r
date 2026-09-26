\name{connect}
\alias{connect}
\alias{bind_model}
\alias{model_choices}
\alias{terminal_ui}
\title{Connect interactively and bind a model}
\description{

\code{connect()} asks which saved connection or provider to use, signs in or
saves a key if needed, lists the account's models, and returns a client bound
to that connection and model. It never sends a prompt. Without an interactive
session and without \code{ui}, it stops before reading anything.

}
\usage{
connect(provider = NULL, model = NULL, ..., auth = local_auth(), ui = NULL,
  capability = NULL, open_browser = FALSE, allow_unverified = FALSE,
  transport = NULL)

bind_model(provider, model, ..., auth = local_auth(), transport = NULL,
  adaptations = "note")

model_choices(provider, ..., auth = local_auth(), refresh = FALSE,
  capability = NULL, include_unknown = FALSE, registry = NULL,
  transport = NULL)

terminal_ui(..., open_browser = FALSE)

}
\arguments{
\item{provider}{A provider id to skip the provider question, or \code{NULL}.}
\item{model}{A model id to skip the model question, or \code{NULL}.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{auth}{The sign-in scope: \code{local_auth()} (the default: the private credentials file), \code{memory_auth()}, or \code{new_auth()}.}
\item{ui}{A UI; \code{NULL} uses \code{terminal_ui()} in an interactive session.}
\item{capability}{Offer only models known to support \code{"reasoning"}, \code{"vision"} or \code{"structured-output"}.}
\item{open_browser}{Open sign-in pages in the browser (https only).}
\item{allow_unverified}{Offer methods that have no live receipt yet, labelled as such.}
\item{transport}{A function that sends one wire request and returns \code{list(status, headers, body)}; see \code{transport_curl()} and \code{fake_transport()}. \code{NULL} uses curl.}
\item{adaptations}{As for \code{new_router()}.}
\item{refresh}{Fetch the account's own model list now (\code{TRUE}) or read \code{registry}.}
\item{include_unknown}{Also offer models whose support for \code{capability} is unknown.}
\item{registry}{A registry from \code{new_model_registry()}.}
}
\details{

A bound client takes \code{complete(client, "text")}, a message, a list of
messages, or a request for its model. It follows its connection's renewals
and nothing else: after a replacement or a logout it stops with
\code{connection_changed} or \code{login_required} rather than switch who pays.
A sign-in completed by \code{connect()} stays saved even if you then cancel the
model question.

}
\value{
A bound client, a list of model choices, or a UI.
}
\examples{

auth <- memory_auth()
set_api_key("openai", "sk-example", auth = auth)
client <- bind_model("openai", "gpt-5-mini", auth = auth)
client
client$request("Hello")$model
\dontrun{
client <- connect()           # at a console
response_text(complete(client, "Hello"))
}

}
