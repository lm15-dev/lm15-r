\name{adaptation}
\alias{adaptation}
\alias{plan}
\title{Adapted settings: the record, and a preview without sending}
\description{When a provider cannot take a setting as asked, lm15 adapts the request (drops, clamps or substitutes the setting, or applies it on the client) and records what it changed. \code{plan()} returns that record for a request without sending it; \code{adaptation()} builds one record.}
\usage{
adaptation(field, action, reason, ..., asked = NULL, applied = NULL)
plan(lm, request, ..., stream = FALSE)
}
\arguments{
\item{field}{The setting's path, for example \code{"config.temperature"}.}
\item{action}{What was done: \code{"dropped"}, \code{"clamped"}, \code{"substituted"}, \code{"client_side"}, \code{"satisfied"} or \code{"defaulted"}.}
\item{reason}{Why, in words.}
\item{...}{Must be empty.}
\item{asked, applied}{The value asked for and the value sent, when there is one.}
\item{lm}{A client from \code{new_lm()} or a router from \code{new_router()}.}
\item{request}{A \code{request()} value.}
\item{stream}{Whether to plan the streaming form of the request.}
}
\details{A client or router's \code{adaptations} setting decides what happens: \code{"note"} (the default) adapts and records; \code{"silent"} adapts the same way with no record on the response; \code{"refuse"} raises \code{UnsupportedFeatureError} before sending, naming the setting, instead of dropping, clamping, substituting or applying it on the client. The record rides the response and its first stream event.

\code{plan()} is offline: it sends nothing and reads no credential, so a router plans a route even without its key. It raises what the call would raise, and returns the full record under every policy, \code{"silent"} included.}
\value{\code{adaptation()} returns one record. \code{plan()} returns a list of records, empty when nothing would be adapted.}
\examples{
router <- new_router(env = character())
r <- request("anthropic:claude-haiku-4-5", list(message_user("hello")),
  config = config(temperature = 1.5))
for (a in plan(router, r)) cat(a$field, a$action, "\n")
}
