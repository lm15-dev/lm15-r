\name{stream-events}
\alias{stream-events}
\alias{stream_start_event}
\alias{stream_delta_event}
\alias{stream_end_event}
\alias{stream_error_event}
\alias{text_delta}
\alias{thinking_delta}
\alias{tool_call_delta}
\alias{image_delta}
\alias{audio_delta}
\alias{citation_delta}
\alias{continuation_delta}
\alias{error_detail}
\title{Stream events}
\description{
The events \code{stream()} delivers: one start, deltas, and one end (or an error). Deltas carry a fragment and the index of the part it belongs to.
}
\usage{
stream_start_event(..., id = NULL, model = NULL, adaptations = list())

stream_delta_event(delta, ...)

stream_end_event(..., finish_reason = NULL, usage = NULL, provider_data = NULL)

stream_error_event(error, ...)

text_delta(text, ..., part_index = 0L, logprobs = list(),
  logprobs_complete = TRUE)

thinking_delta(text, ..., part_index = 0L)

tool_call_delta(input, ..., part_index = 0L, id = NULL, name = NULL)

image_delta(..., data = NULL, url = NULL, file_id = NULL, part_index = 0L,
  media_type = NULL)

audio_delta(..., data = NULL, url = NULL, file_id = NULL, part_index = 0L,
  media_type = NULL)

citation_delta(..., text = NULL, url = NULL, title = NULL, part_index = 0L)

continuation_delta(provider, kind, ..., data = json_object(),
  part_index = NULL)

error_detail(code, ..., message = "", provider_code = NULL,
  http_response = NULL)

}
\arguments{
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{id}{An id, or \code{NULL}.}
\item{model}{A model identifier.}
\item{adaptations}{Adaptations made to the request, sent with the start event.}
\item{delta}{A delta.}
\item{finish_reason}{Why generation stopped, or \code{NULL}.}
\item{usage}{Token counts, or \code{NULL}.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{error}{An \code{error_detail()} value.}
\item{text}{Text.}
\item{part_index}{The index of the part this fragment belongs to; for continuation deltas \code{NULL} means the whole message.}
\item{logprobs}{Token log-probabilities for this fragment.}
\item{logprobs_complete}{\code{FALSE} when local editing left text without scores.}
\item{input}{Tool-call arguments: a JSON object (\code{json_object()}); a delta carries a text fragment of the JSON instead.}
\item{name}{A tool name, or \code{NULL}.}
\item{data}{For media deltas: base64 data; for continuation deltas: a JSON object.}
\item{url}{A URL the provider fetches itself.}
\item{file_id}{The id of a file already uploaded to the provider.}
\item{media_type}{The media type, such as \code{"image/png"}.}
\item{title}{A citation title.}
\item{provider}{The dialect the state belongs to.}
\item{kind}{The continuation kind.}
\item{code}{The error code, such as \code{"rate_limit"}.}
\item{message}{The error message.}
\item{provider_code}{The provider's own error code, or \code{NULL}.}
\item{http_response}{Diagnostics from the HTTP handshake (\code{request_id}, \code{retry_after}, \code{rate_limit_headers}), or \code{NULL}.}
}
\value{
A stream event, delta or ErrorDetail value.
}
\examples{

events <- list(stream_start_event(model = "m"),
               stream_delta_event(text_delta("Hel")),
               stream_delta_event(text_delta("lo")),
               stream_end_event(finish_reason = "stop"))
response_text(materialize_response(events, request("m", list(message_user("hi")))))

}
