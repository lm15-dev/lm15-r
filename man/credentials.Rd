\name{credentials}
\alias{credentials}
\alias{api_key}
\alias{bearer_token}
\alias{aws_credentials}
\alias{credential_expired}
\alias{select_auth_scheme}
\alias{explain_auth}
\alias{credentials_path}
\alias{load_local_credential}
\alias{credential_store}
\alias{write_credentials}
\alias{cloud_credential_provider}
\title{Credentials and the authentication doctor}
\description{

Credential values, and \code{explain_auth()}: which credential a request
would use, rung by rung, and where each host setting comes from, without
reading a secret aloud or contacting anything.

}
\usage{
api_key(value, ...)

bearer_token(value, ..., expires_at = NULL)

aws_credentials(access_key_id, secret_access_key, ..., session_token = NULL,
  expires_at = NULL)

credential_expired(credential, ..., now = Sys.time(), skew_seconds = 300)

select_auth_scheme(credential, schemes, provider = NULL)

explain_auth(provider, ..., api_keys = list(), env = NULL, path = NULL,
  now = Sys.time(), settings = list(), credential = NULL, auth = NULL)

credentials_path(..., env = NULL)

load_local_credential(provider, ..., path = NULL, env = NULL,
  now = Sys.time(), transport = NULL, lock_timeout = 30)

credential_store(..., path = credentials_path(env = env), env = NULL,
  lock_timeout = 30)

write_credentials(value, ..., path = credentials_path(env = env), env = NULL,
  lock_timeout = 30)

cloud_credential_provider(provider, ..., env = NULL, settings = list(),
  transport = NULL, run = NULL, clock = Sys.time, named = NULL)

}
\arguments{
\item{value}{The secret, or for \code{write_credentials()} the whole JSON document.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{expires_at}{An RFC 3339 timestamp with a time zone, or \code{NULL} when unknown.}
\item{access_key_id}{The AWS access key id.}
\item{secret_access_key}{The AWS secret key.}
\item{session_token}{The AWS session token, or \code{NULL}.}
\item{credential}{A credential value; for \code{explain_auth()}, a named cloud identity.}
\item{now}{The current time, as a \code{POSIXct}. A fixed value makes the result reproducible.}
\item{skew_seconds}{Treat a credential as expired this many seconds early.}
\item{schemes}{The schemes the provider accepts, such as \code{c("bearer", "x-api-key")}.}
\item{provider}{A provider id, such as \code{"openai"}, \code{"anthropic"} or \code{"xai"}. \code{providers()} lists them; an underscore is read as a hyphen.}
\item{api_keys}{Explicit credentials by provider, as for \code{new_router()}.}
\item{env}{A named character vector read instead of the process environment. \code{NULL} reads the environment; \code{character()} reads nothing.}
\item{path}{The credentials file.}
\item{settings}{Host settings for a cloud provider, such as \code{list(region = "us-east-1")} or \code{list(project = "my-project")}. A setting given here wins over the environment.}
\item{auth}{A sign-in scope to explain instead of the unmanaged chain, or \code{NULL}.}
\item{transport}{A function that sends one wire request and returns \code{list(status, headers, body)}; see \code{transport_curl()} and \code{fake_transport()}. \code{NULL} uses curl.}
\item{lock_timeout}{Seconds to wait for another process holding the credentials file's lock.}
\item{run}{A function that runs a command, for tests.}
\item{clock}{A function returning the current time.}
\item{named}{A named cloud identity, or \code{NULL} for the whole chain.}
}
\details{
\code{explain_auth()} walks the same chain a request uses and never calls a credential function, runs a command or contacts a server. A cloud chain's metadata-server rung is shown as unprobed offline.
}
\value{
A credential value, \code{TRUE}/\code{FALSE}, a scheme name, an explanation report (print it), a path, a stored credential, a store, or a credential function.
}
\examples{

explain_auth("openai", env = c(OPENAI_API_KEY = "sk-example"))
explain_auth("vertex", env = c(GOOGLE_CLOUD_PROJECT = "my-project", NO_GCE_CHECK = "1",
                               HOME = tempdir()))

}
