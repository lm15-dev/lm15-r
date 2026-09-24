\name{fake_lm}
\alias{fake_lm}
\alias{fake_transport}
\alias{recorded_requests}
\alias{response_to_events}
\title{Offline application tests and response-to-event conversion}
\description{Test applications without provider keys or network calls, using the same public completion and streaming functions as real clients.}
\usage{
fake_lm(responses, ...)
fake_transport(responses, ...)
recorded_requests(x, ...)
response_to_events(response, ..., on_event = NULL)
}
\arguments{
\item{responses}{For fake models, responses, text strings or error conditions consumed in order. For fake transports, a list of HTTP reply lists with \code{status}, \code{headers}, and \code{body} or \code{chunks}, or error conditions. Bodies and chunks are raw bytes or text.}
\item{...}{Must be empty.}
\item{x}{A fake model or fake transport.}
\item{response}{A canonical \code{response()} value to convert.}
\item{on_event}{Optional callback receiving each canonical event. Without it, return the event list.}
}
\details{Fake models work with \code{complete()} and \code{stream()}. Queued text becomes an assistant response with model \code{"fake"}. Exhausting the queue is an error. Requests are recorded before consumption. Fake transports emit successful chunks to the callback; unsuccessful response bodies remain available for ordinary error classification. Recorded wire requests contain explicit credentials and must not be logged when they use real keys.

Response conversion preserves content and continuation data represented by the delta vocabulary. It refuses unsupported parts, image paths or detail fields, non-inline audio, and log probabilities without text, instead of silently discarding them. When a callback is supplied, the whole conversion is checked before the first event is delivered.}
\value{A fake client, callable transport, recorded request list, or canonical event list. Callback conversion invisibly returns its input response.}
\examples{
client <- fake_lm(c("first answer", "second answer"))
r <- request("example", list(message_user("hello")))
response_text(complete(client, r))
length(recorded_requests(client))
}
