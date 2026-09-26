\name{content-parts}
\alias{content-parts}
\alias{text_part}
\alias{thinking_part}
\alias{thinking}
\alias{refusal_part}
\alias{refusal}
\alias{citation_part}
\alias{data_part}
\alias{image_part}
\alias{audio_part}
\alias{video_part}
\alias{document_part}
\alias{binary_part}
\alias{tool_call_part}
\alias{tool_result_part}
\alias{continuation_state}
\alias{continuation_data}
\alias{media_bytes}
\title{Content parts}
\description{

The pieces a message is made of. Media parts take exactly one of \code{data},
\code{url}, \code{file_id} or \code{path} (a local file read when the request is
sent). A data part carries any JSON value: as input it is structured data, and
in an assistant message it is the answer to a judgment request
(\code{\link{judgments}()}).

}
\usage{
text_part(text, ..., continuation = list())

thinking_part(text, ..., continuation = list())

thinking(content, ..., continuation = list())

refusal_part(text, ..., continuation = list())

refusal(content, ..., continuation = list())

citation_part(..., url = NULL, title = NULL, text = NULL,
  continuation = list())

data_part(value, ..., probabilities = NULL, method = NULL,
  continuation = list())

image_part(..., media_type = "image/png", data = NULL, url = NULL,
  file_id = NULL, path = NULL, detail = NULL, continuation = list())

audio_part(..., media_type = "audio/wav", data = NULL, url = NULL,
  file_id = NULL, path = NULL, continuation = list())

video_part(..., media_type = "video/mp4", data = NULL, url = NULL,
  file_id = NULL, path = NULL, continuation = list())

document_part(..., media_type = "application/pdf", data = NULL, url = NULL,
  file_id = NULL, path = NULL, continuation = list())

binary_part(..., media_type = "application/octet-stream", data = NULL,
  url = NULL, file_id = NULL, path = NULL, continuation = list())

tool_call_part(id, name, ..., input = json_object(), continuation = list())

tool_result_part(id, content, ..., name = NULL, is_error = FALSE,
  continuation = list())

continuation_state(provider, kind, ..., data = json_object())

continuation_data(x, provider, kind)

media_bytes(part, ...)

}
\arguments{
\item{text}{Text.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{continuation}{Provider continuation state that must travel with this value when it is sent back (a list of \code{continuation_state()} values). Keep what a response gave you; do not build it by hand.}
\item{content}{The result: a string, a part, or a list of parts (text, media, data).}
\item{url}{A URL the provider fetches itself.}
\item{title}{A title.}
\item{value}{Any JSON value; \code{NULL} is JSON null.}
\item{probabilities}{For an answer only: a JSON object mapping each judgment to a distribution over its keys, or \code{NULL}.}
\item{method}{How \code{probabilities} were measured: \code{"provider_classification"} or \code{"candidate_sequence_likelihood"}; present exactly when \code{probabilities} is.}
\item{media_type}{The media type, such as \code{"image/png"}.}
\item{data}{Base64 data or raw bytes; for \code{continuation_state()}, a JSON object.}
\item{file_id}{The id of a file already uploaded to the provider.}
\item{path}{A local file, read when the request is sent.}
\item{detail}{Image detail for providers that take it: \code{"low"}, \code{"high"} or \code{"auto"}.}
\item{id}{The tool call's id (for a result: the id of the call it answers).}
\item{name}{The tool's name.}
\item{input}{Tool-call arguments: a JSON object (\code{json_object()}); a delta carries a text fragment of the JSON instead.}
\item{is_error}{Whether the tool failed.}
\item{provider}{The dialect the state belongs to, such as \code{"anthropic"}.}
\item{kind}{The continuation kind, such as \code{"thought_signature"}.}
\item{x}{A message, a part, or a list of continuation states.}
\item{part}{A media part.}
}
\details{
\code{thinking()} and \code{refusal()} are shorter spellings of \code{thinking_part()} and \code{refusal_part()}. \code{continuation_data()} returns the \code{data} of the first matching state. \code{media_bytes()} returns a part's bytes (decoded data or the file's contents).
}
\value{
A part; \code{continuation_data()} returns a JSON object or \code{NULL}; \code{media_bytes()} returns a raw vector.
}
\examples{

photo <- image_part(data = as.raw(c(0x89, 0x50, 0x4e, 0x47)), media_type = "image/png")
msg <- message_user(list("What is in this picture?", photo))
as_json(data_part(json_object(city = "Oslo", temperature = 12L)))

}
