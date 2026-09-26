\name{files}
\alias{files}
\alias{file_upload}
\alias{file_get}
\alias{file_list}
\alias{file_delete}
\alias{file_download}
\alias{file_op_build}
\alias{file_op_parse}
\alias{file_upload_request}
\alias{file_info}
\alias{file_page}
\title{Provider files}
\description{
Upload, list, fetch and delete files stored by a provider, for use in requests by \code{file_id}.
}
\usage{
file_upload(lm, request, ...)

file_get(lm, id, ...)

file_list(lm, ..., limit = 20L, cursor = NULL)

file_delete(lm, id, ...)

file_download(lm, id, ...)

file_op_build(lm, action, ..., upload_request = NULL, file_id = NULL,
  limit = 20L, cursor = NULL)

file_op_parse(lm, body, ..., page = FALSE)

file_upload_request(filename, ..., bytes_data = NULL,
  media_type = "application/octet-stream", extensions = NULL, path = NULL)

file_info(id, ..., filename = NULL, media_type = NULL, size_bytes = NULL,
  created_at = NULL, expires_at = NULL, readiness = "ready",
  downloadable = NULL, provider_data = NULL)

file_page(..., items = list(), next_cursor = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{request}{A \code{file_upload_request()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{id}{The provider's identifier for the object.}
\item{limit}{The largest number of entries to ask for.}
\item{cursor}{A paging cursor returned by a previous page, or \code{NULL} for the first page.}
\item{action}{\code{"upload"}, \code{"get"}, \code{"list"}, \code{"delete"} or \code{"download"}.}
\item{upload_request}{A \code{file_upload_request()} for uploads.}
\item{file_id}{A file id.}
\item{body}{A reply body: a JSON string, raw bytes, or an already parsed JSON object.}
\item{page}{Whether the body is a listing page.}
\item{filename}{The file's name.}
\item{bytes_data}{The file's bytes, or \code{NULL} when \code{path} is given.}
\item{media_type}{The media type, such as \code{"image/png"}.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{path}{A local file to upload.}
\item{size_bytes}{The file's size.}
\item{created_at}{An RFC 3339 timestamp, or \code{NULL}.}
\item{expires_at}{An RFC 3339 timestamp with a time zone, or \code{NULL} when unknown.}
\item{readiness}{\code{"pending"}, \code{"ready"} or \code{"failed"}.}
\item{downloadable}{Whether its bytes can be downloaded.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{items}{A list of values on this page.}
\item{next_cursor}{The cursor of the next page, or \code{NULL}.}
}
\details{
\code{file_op_build()} and \code{file_op_parse()} are the pure halves: they build the HTTP request and read the reply, without networking.
}
\value{
A FileInfo, a FilePage, raw bytes for a download, a wire request, or invisible \code{NULL} for a deletion.
}
\examples{

up <- file_upload_request("notes.txt", bytes_data = charToRaw("hello"), media_type = "text/plain")
wire <- file_op_build(new_lm("openai", api_key = "sk-example"), "upload", upload_request = up)
wire$method

}
