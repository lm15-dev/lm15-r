\name{local_auth}
\alias{local_auth}
\alias{memory_auth}
\alias{new_auth}
\alias{file_store}
\alias{memory_store}
\title{Sign-in scopes and stores}
\description{
A scope is where saved connections live: the private local file (\code{local_auth()}), this R session only (\code{memory_auth()}), or a store you choose (\code{new_auth()}). Constructing one reads and writes nothing.
}
\usage{
local_auth(path = NULL, ...)

memory_auth(...)

new_auth(store, ..., clock = function() as.numeric(Sys.time()),
  monotonic = function() proc.time()[["elapsed"]], http = NULL, sleep = NULL)

file_store(path = NULL, ..., lock_timeout = 30)

memory_store()

}
\arguments{
\item{path}{The credentials file, or \code{NULL} for the default.}
\item{\dots}{For \code{local_auth()} and \code{memory_auth()}: arguments passed to \code{new_auth()}. Elsewhere unused.}
\item{store}{A store from \code{file_store()} or \code{memory_store()}.}
\item{clock}{A function returning the current time in seconds since the epoch.}
\item{monotonic}{A function returning a monotonic time in seconds.}
\item{http}{A function that sends one sign-in HTTP request; for tests. \code{NULL} uses curl.}
\item{sleep}{A function that waits a number of seconds; for tests.}
\item{lock_timeout}{Seconds to wait for another process holding the credentials file's lock.}
}
\value{
An Auth scope, or a store.
}
\examples{

auth <- new_auth(file_store(file.path(tempdir(), "credentials.json")))
auth

}
