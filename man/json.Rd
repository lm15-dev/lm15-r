\name{json}
\alias{json}
\alias{as_json}
\alias{from_json}
\alias{as_dict}
\alias{from_dict}
\alias{json_object}
\alias{json_array}
\alias{validate}
\alias{integer_value}
\title{Canonical JSON}
\description{

Every canonical value converts to and from the contract's JSON exactly, in
every lm15 language. Empty objects and empty arrays stay distinct
(\code{json_object()}, \code{json_array()}), numbers keep their spelling, and
large integers are exact.

}
\usage{
as_json(x, ..., include_provider_data = FALSE)

from_json(text, kind, ...)

as_dict(x, ..., include_provider_data = FALSE)

from_dict(value, kind, ...)

json_object(...)

json_array(...)

validate(x, ...)

integer_value(decimal, ...)

}
\arguments{
\item{x}{A canonical lm15 value.}
\item{\dots}{For \code{json_object()} and \code{json_array()}: the members. Elsewhere unused.}
\item{include_provider_data}{Whether to include the provider's raw reply fields.}
\item{text}{A JSON string.}
\item{kind}{The canonical kind, such as \code{"request"}, \code{"response"}, \code{"message"}, \code{"part"} or \code{"config"}.}
\item{value}{A parsed JSON object.}
\item{decimal}{An integer written in decimal, such as \code{"9007199254740993"}.}
}
\details{
Named lists are objects, unnamed lists are arrays; \code{NULL} means absent, and zero and \code{FALSE} are data. \code{validate()} re-checks a value; assigning a field with \code{$<-} also validates.
}
\value{
\code{as_json()} returns a string; \code{as_dict()} a JSON object; \code{from_json()} and \code{from_dict()} a canonical value; \code{integer_value()} an exact integer.
}
\examples{

req <- request("openai:gpt-5-mini", list(message_user("hi")))
json <- as_json(req)
json
identical(as_json(from_json(json, "request")), json)
as_json(json_object(empty_object = json_object(), empty_array = json_array()))

}
