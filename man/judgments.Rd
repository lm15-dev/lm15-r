\name{judgments}
\alias{judgments}
\alias{choice}
\alias{yes_no}
\alias{score}
\alias{judgments_in_schema}
\alias{request_judgments}
\alias{response_data}
\alias{response_probabilities}
\alias{response_method}
\alias{response_expected}
\alias{expected_level}
\title{Judgments: declared answers with probabilities}
\description{

A judgment is a question whose answer is one of the keys you declare: yes or
no, a choice among labels, or an ordered score. \code{judgments()} writes the
JSON Schema response format; the answer comes back as a data part holding the
model's JSON, and, where the provider can measure it, a probability for every
key.

}
\usage{
judgments(..., name = "judgments", strict = TRUE)

choice(instruction, options, ...)

yes_no(instruction, ...)

score(instruction, levels, ...)

judgments_in_schema(schema, ...)

request_judgments(request, ...)

response_data(response, ...)

response_probabilities(response, ...)

response_method(response, ...)

response_expected(response, field, ...)

expected_level(distribution, ...)

}
\arguments{
\item{\dots}{For \code{judgments()}: the judgment properties, named (from \code{yes_no()}, \code{choice()}, \code{score()} or written by hand). Elsewhere unused.}
\item{name}{The response format's name.}
\item{strict}{Whether the provider must follow the schema strictly.}
\item{instruction}{The question, in words.}
\item{options}{The choices: a character vector of keys, or a named vector of descriptions (\code{NA} for none).}
\item{levels}{The ordered levels, lowest first: a character vector of descriptions, optionally named with a short title. At least two, at most ten.}
\item{schema}{A JSON Schema object.}
\item{request}{A canonical request from \code{request()}.}
\item{response}{A canonical response, as returned by \code{complete()}.}
\item{field}{The name of an ordered judgment.}
\item{distribution}{A named list of level to probability.}
}
\details{

\code{config(probabilities = )} says what to do when the provider cannot
measure a distribution: \code{"off"} asks for nothing; \code{"if_available"}
answers with the pick only and records that probabilities were dropped;
\code{"required"} refuses before sending. TypeSafe's \code{jev-} models return
their own classification; a vLLM server scores every key from token
log-probabilities (one batched call); other providers return the pick only.
\code{response_data()} reads the answer (or the parsed JSON text of a plain
structured-output reply); \code{response_expected()} is the probability-weighted
level of an ordered judgment. No number the model wrote in prose is ever read
as a probability.

}
\value{
\code{judgments()} returns a response format; \code{choice()}, \code{yes_no()}, \code{score()} a schema property; the readers return JSON values, a distribution, a method string or a number, or \code{NULL}.
}
\examples{

wine <- judgments(
  quality = score("How good is this wine?", c(poor = "Faulty", ok = "Sound", great = "Superb")),
  style = choice("Which style?", c("fruit", "oak", "mineral")),
  ageing = yes_no("Will it improve with age?"))
req <- request("jev-latest", list(message_user("Ripe cassis, firm tannins, long finish.")),
               config = config(response_format = wine, probabilities = "if_available"))
names(request_judgments(req))

}
