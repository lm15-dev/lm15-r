\name{internal}
\alias{internal}
\alias{vet_handle}
\alias{browser_dispatch}
\alias{surface_dump}
\title{Internal entry points}
\description{
Entry points for the lm15 conformance harness (\code{vet_handle()}), the webR browser bridge (\code{browser_dispatch()}) and introspection (\code{surface_dump()}). Not for direct use.
}
\usage{
vet_handle(line, ...)

browser_dispatch(action, id, payload = "{}", ...)

surface_dump()

}
\arguments{
\item{line}{One JSON line of the harness protocol.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{action}{The bridge action.}
\item{id}{The bridge call id.}
\item{payload}{The call's JSON payload.}
}
\value{
A JSON string, or a list describing the canonical types.
}
\keyword{internal}
