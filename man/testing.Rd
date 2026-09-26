\name{testing}
\alias{testing}
\alias{fake_lm}
\alias{fake_transport}
\alias{recorded_requests}
\title{Test without a network}
\description{
\code{fake_lm()} answers requests from a script; \code{fake_transport()} answers HTTP requests from a script, so the real request building and reply parsing run. Both record what they were sent.
}
\usage{
fake_lm(responses, ...)

fake_transport(responses, ...)

recorded_requests(x, ...)

}
\arguments{
\item{responses}{For \code{fake_lm()}: a list of strings, responses or conditions, used in order. For \code{fake_transport()}: a list of \code{list(status, body, headers, chunks)} replies or conditions.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{x}{A fake from \code{fake_lm()} or \code{fake_transport()}.}
}
\value{
A fake client, a transport function, or the list of recorded requests.
}
\examples{

transport <- fake_transport(list(list(status = 200L, body =
  '{"id":"r","model":"gpt-5-mini","output":[{"type":"message","role":"assistant",
    "content":[{"type":"output_text","text":"hi"}]}]}')))
client <- new_lm("openai", api_key = "sk-example", transport = transport)
response_text(complete(client, request("gpt-5-mini", list(message_user("hello")))))
recorded_requests(transport)[[1]]$url

}
