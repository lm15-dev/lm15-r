\name{complete}
\alias{complete}
\alias{stream}
\alias{plan}
\alias{adaptation}
\alias{build_request}
\alias{parse_response}
\alias{replay_stream}
\alias{materialize_response}
\alias{response_to_events}
\title{Send a request}
\description{

\code{complete()} sends a request and returns the response. \code{stream()}
calls \code{on_event} with each event as it arrives and returns the assembled
response at the end. \code{plan()} says, without sending anything, what the
provider's wire would adapt or refuse.

}
\usage{
complete(lm, request, ...)

stream(lm, request, on_event, ...)

plan(lm, request, ..., stream = FALSE)

adaptation(field, action, reason, ..., asked = NULL, applied = NULL)

build_request(lm, request, ..., stream = FALSE)

parse_response(lm, request, body, ..., status = 200L, headers = list())

replay_stream(lm, request, body, ...)

materialize_response(events, request, ...)

response_to_events(response, ..., on_event = NULL)

}
\arguments{
\item{lm}{A client from \code{new_lm()}, a router from \code{new_router()}, a bound client from \code{connect()} or \code{bind_model()}, or any object with a method.}
\item{request}{A request from \code{request()}. A bound client also takes messages or a string.}
\item{\dots}{Arguments for methods. The package's own clients and routers take none; a bound client takes \code{system}, \code{tools} and \code{config} when \code{request} is messages.}
\item{on_event}{For \code{stream()}: a function called with each event. For \code{response_to_events()}: an optional function called with each event.}
\item{stream}{Whether to build or plan the streaming form.}
\item{field}{The request path that was adapted, such as \code{"config.top_k"}.}
\item{action}{What was done: \code{"dropped"}, \code{"clamped"}, \code{"substituted"}, \code{"client_side"}, \code{"satisfied"} or \code{"defaulted"}.}
\item{reason}{Why, in words.}
\item{asked}{What was asked for, or \code{NULL}.}
\item{applied}{What was applied instead, or \code{NULL}.}
\item{body}{A recorded reply body (JSON text, raw bytes, or for streams the SSE text).}
\item{status}{The reply's HTTP status.}
\item{headers}{HTTP headers as a named list.}
\item{events}{A list of stream events, in order.}
\item{response}{A canonical response, as returned by \code{complete()}.}
}
\details{

Nothing is retried: a request that fails raises its error once. Interrupting a
stream releases the connection; it cannot promise that the provider stopped
generating or billing. \code{build_request()} returns the exact HTTP request
(credentials included: do not log it); \code{parse_response()} and
\code{replay_stream()} turn recorded replies into responses without a network.
\code{response_to_events()} turns a response into the events a stream would
have produced.

}
\value{
\code{complete()}, \code{stream()}, \code{parse_response()} and \code{materialize_response()} return a Response; \code{plan()} a list of Adaptation values; \code{build_request()} a wire request; \code{replay_stream()} a list of \code{events} and \code{response}; \code{response_to_events()} a list of events.
}
\examples{

req <- request("openai:gpt-5-mini", list(message_user("Hi")),
               config = config(top_k = 40L, max_tokens = 20L))
plan(new_router(), req)      # OpenAI's wire has no top_k: dropped, and said so
client <- fake_lm(list("Hello!", "Hello again!"))
response_text(complete(client, req))
invisible(stream(client, req, function(event) cat(event$type, "")))

}
