\name{video}
\alias{video}
\alias{video_generate}
\alias{video_job}
\alias{video_jobs}
\alias{video_submit}
\alias{video_status}
\alias{video_result}
\alias{video_list}
\alias{video_op_build}
\alias{video_op_parse}
\alias{video_generation_request}
\alias{video_job_info}
\title{Video generation}
\description{
Video is a job: submission returns a ticket. \code{video_generate()} returns a handle; \code{wait()} polls it and \code{result()} fetches the video.
}
\usage{
video_generate(lm, request, ...)

video_job(lm, id, ...)

video_jobs(lm, ..., limit = 20L, model = NULL)

video_submit(lm, request, ...)

video_status(lm, id, ...)

video_result(lm, id, ...)

video_list(lm, ..., limit = 20L, model = NULL)

video_op_build(lm, action, ..., request = NULL, id = NULL, status_body = NULL,
  limit = 20L, model = NULL)

video_op_parse(lm, kind, body = NULL, ..., id = NULL, status_body = NULL,
  fetched = NULL)

video_generation_request(model, prompt, ..., seconds = NULL, images = list(),
  extensions = NULL)

video_job_info(id, status, ..., progress = NULL, created_at = NULL,
  model = NULL, provider_data = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{request}{A \code{video_generation_request()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{id}{The provider's identifier for the object.}
\item{limit}{The largest number of entries to ask for.}
\item{model}{A model, or \code{NULL} for all.}
\item{action}{\code{"submit"}, \code{"status"}, \code{"result"} or \code{"list"}.}
\item{status_body}{The JSON body of a status reply the operation depends on, or \code{NULL}.}
\item{kind}{\code{"job"}, \code{"list"} or \code{"result"}.}
\item{body}{A reply body: a JSON string, raw bytes, or an already parsed JSON object.}
\item{fetched}{The downloaded video, when already fetched.}
\item{prompt}{What to generate.}
\item{seconds}{The length in seconds.}
\item{images}{Input frames, a list of image parts.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{status}{\code{"queued"}, \code{"running"}, \code{"completed"}, \code{"failed"} or \code{"cancelled"}.}
\item{progress}{Percent done.}
\item{created_at}{An RFC 3339 timestamp, or \code{NULL}.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
}
\value{
A job handle, a VideoJobInfo, a video part, a list, or a wire request.
}
\examples{

v <- video_generation_request("sora-2", "A paper boat on a rainy street", seconds = 4L)
wire <- video_op_build(new_lm("openai", api_key = "sk-example"), "submit", request = v)
wire[[1]]$method

}
