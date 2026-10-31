\name{response}
\alias{response}
\alias{usage}
\alias{response_text}
\alias{tool_calls}
\alias{citations}
\alias{parse_json}
\alias{token_logprob}
\alias{top_logprob}
\alias{tool_call_info}
\title{Read a response}
\description{

A response holds the assistant's message, the finish reason, token usage, and
any adaptations made to the request. Usage counts the provider did not report
are \code{NULL}, never zero. Each count means the same thing on every
provider: input is every token read, cached ones included; output every token
written, reasoning included; the cache and audio counts are parts of input or
output, and total is input plus output. The provider's own numbers are in
\code{provider_data}.

}
\usage{
response(model, message, finish_reason, ..., id = NULL,
  usage = .new_value("Usage", list()), logprobs = list(),
  logprobs_complete = TRUE, provider_data = NULL, adaptations = list())

usage(..., input_tokens = NULL, output_tokens = NULL, total_tokens = NULL,
  cache_read_tokens = NULL, cache_write_tokens = NULL,
  reasoning_tokens = NULL, input_audio_tokens = NULL,
  output_audio_tokens = NULL)

response_text(response, ...)

tool_calls(response, ...)

citations(response, ...)

parse_json(response, ...)

token_logprob(token, logprob, ..., bytes = list(), token_id = NULL,
  top = list())

top_logprob(token, logprob, ..., bytes = list(), token_id = NULL)

tool_call_info(id, name, ..., input = json_object())

}
\arguments{
\item{model}{A model identifier.}
\item{message}{The assistant's message.}
\item{finish_reason}{Why generation stopped: \code{"stop"}, \code{"length"}, \code{"tool_call"}, \code{"content_filter"} or \code{"error"}.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{id}{An id.}
\item{usage}{Token counts, from \code{usage()}.}
\item{logprobs}{Token log-probabilities, a list of \code{token_logprob()} values.}
\item{logprobs_complete}{\code{FALSE} when local editing left text without its original scores.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{adaptations}{A list of \code{adaptation()} values.}
\item{input_tokens}{Every token read, cached ones included.}
\item{output_tokens}{Every token written, reasoning included.}
\item{total_tokens}{Input plus output.}
\item{cache_read_tokens}{The part of input read from cache.}
\item{cache_write_tokens}{The part of input written to cache.}
\item{reasoning_tokens}{The part of output spent reasoning, when counted.}
\item{input_audio_tokens}{The part of input that is audio.}
\item{output_audio_tokens}{The part of output that is audio.}
\item{response}{A canonical response, as returned by \code{complete()}.}
\item{token}{The token's text.}
\item{logprob}{Its log-probability.}
\item{bytes}{Its UTF-8 bytes, as a list of integers.}
\item{token_id}{The token's id, or \code{NULL}.}
\item{top}{Alternatives, a list of \code{top_logprob()} values.}
\item{name}{The tool's name.}
\item{input}{Tool-call arguments: a JSON object (\code{json_object()}); a delta carries a text fragment of the JSON instead.}
}
\details{
\code{response_text()} joins the text parts, or returns \code{NULL} when the answer is not plain text. \code{parse_json()} parses it as JSON (numbers keep their spelling).
}
\value{
A Response or Usage value; \code{response_text()} a string or \code{NULL}; \code{tool_calls()} and \code{citations()} lists of parts; \code{parse_json()} a JSON value.
}
\examples{

answer <- complete(fake_lm(list("A list column holds a list in each row.")),
                   request("m", list(message_user("What is a list column?"))))
response_text(answer)
answer$finish_reason
answer$usage$input_tokens   # NULL: the stand-in reported no usage

}
