\name{batch}
\alias{batch}
\alias{batch_job}
\alias{batches}
\alias{batch_submit}
\alias{batch_status}
\alias{batch_results}
\alias{batch_cancel}
\alias{batch_list}
\alias{batch_op_build}
\alias{batch_op_parse}
\alias{batch_request}
\alias{batch_entry}
\alias{batch_job_info}
\title{Batches}
\description{
Submit many requests as one provider batch (cheaper, slower) and fetch the results later. \code{batch()} returns a job handle; see \code{\link{wait}}.
}
\usage{
batch(lm, request, ...)

batch_job(lm, id, ...)

batches(lm, ..., limit = 20L)

batch_submit(lm, request, ...)

batch_status(lm, id, ...)

batch_results(lm, id, ...)

batch_cancel(lm, id, ...)

batch_list(lm, ..., limit = 20L)

batch_op_build(lm, action, ..., request = NULL, id = NULL, limit = 20L,
  upload_body = NULL, status_body = NULL)

batch_op_parse(lm, kind, body = NULL, ..., status_body = NULL,
  fetched = list())

batch_request(requests, ..., model = NULL, label = NULL, extensions = NULL)

batch_entry(index, outcome, ..., response = NULL, error = NULL)

batch_job_info(id, status, ..., label = NULL, created_at = NULL,
  provider_data = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{request}{A \code{batch_request()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{id}{The provider's identifier for the object.}
\item{limit}{The largest number of entries to ask for.}
\item{action}{\code{"submit"}, \code{"status"}, \code{"results"}, \code{"cancel"} or \code{"list"}.}
\item{upload_body}{The reply of the upload step, for providers that upload first.}
\item{status_body}{The JSON body of a status reply the operation depends on, or \code{NULL}.}
\item{kind}{\code{"job"}, \code{"list"} or \code{"results"}.}
\item{body}{A reply body: a JSON string, raw bytes, or an already parsed JSON object.}
\item{fetched}{Result bodies already fetched, keyed as the operation's builder asked for them.}
\item{requests}{A list of requests.}
\item{model}{The model all requests use; defaults to the first request's.}
\item{label}{A label to show, or \code{NULL}.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{index}{The request's position (0-based).}
\item{outcome}{\code{"succeeded"}, \code{"errored"}, \code{"cancelled"} or \code{"expired"}.}
\item{response}{The response of a succeeded entry.}
\item{error}{The error of an errored entry.}
\item{status}{\code{"queued"}, \code{"running"}, \code{"cancelling"}, \code{"completed"}, \code{"failed"}, \code{"cancelled"} or \code{"expired"}.}
\item{created_at}{An RFC 3339 timestamp, or \code{NULL}.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
}
\value{
A job handle, a BatchJobInfo, a list of BatchEntry values or jobs, or a wire request.
}
\examples{

reqs <- lapply(c("red", "green"), function(colour)
  request("openai:gpt-5-mini", list(message_user(paste("A fruit that is", colour)))))
b <- batch_request(reqs, label = "fruit")
b$model

}
