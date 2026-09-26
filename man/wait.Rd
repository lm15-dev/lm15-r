\name{wait}
\alias{wait}
\alias{job_info}
\alias{refresh}
\alias{result}
\alias{results}
\alias{cancel}
\title{Job handles}
\description{
A handle holds a snapshot of a batch or video job. Reading it never contacts the provider; \code{refresh()} fetches a new snapshot; only \code{wait()} polls.
}
\usage{
job_info(job)

refresh(job, ...)

wait(job, ..., poll_every = 5, timeout = 300, cancelled = function() FALSE)

result(job, ...)

results(job, ...)

cancel(job, ...)

}
\arguments{
\item{job}{A job handle from \code{batch()}, \code{batch_job()}, \code{video_generate()} or \code{video_job()}.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{poll_every}{Seconds between polls.}
\item{timeout}{Seconds to wait before giving up (an error of class \code{lm15_wait_timeout}).}
\item{cancelled}{A function returning \code{TRUE} to stop waiting.}
}
\details{
A wait's deadline bounds polling, not an HTTP request already running. \code{cancel()} asks the provider to cancel a batch.
}
\value{
\code{job_info()} returns the snapshot; \code{refresh()}, \code{wait()} and \code{cancel()} the updated handle; \code{result()} and \code{results()} the output.
}
\examples{

\dontrun{
job <- batch(new_lm("openai"), batch_request(list(request("gpt-5-mini", list(message_user("hi"))))))
job <- wait(job, poll_every = 30, timeout = 3600)
results(job)
}

}
