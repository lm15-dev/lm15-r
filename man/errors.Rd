\name{errors}
\alias{errors}
\alias{lm15_error}
\alias{normalize_error}
\alias{retryable}
\title{Errors}
\description{

lm15 signals conditions with the contract's classes, all inheriting from
\code{LM15Error}: \code{AuthError}, \code{BillingError}, \code{RateLimitError},
\code{InvalidRequestError}, \code{ContextLengthError}, \code{TimeoutError},
\code{ServerError}, \code{UnsupportedModelError}, \code{UnsupportedFeatureError},
\code{NotConfiguredError}, \code{UnknownModelError}, \code{AmbiguousModelError},
\code{TransportError}, \code{LockTimeoutError}, \code{StreamAssemblyError},
\code{CollectionLimitError}, \code{AuthOperationError} and \code{ProviderError}.
Catch them with \code{tryCatch()} by class.

}
\usage{
lm15_error(message, ..., code = "provider", provider = NULL,
  provider_code = NULL, status = NULL, request_id = NULL, retry_after = NULL,
  partial = NULL, part_index = NULL, model = NULL, providers = NULL,
  credential_hint = NULL, path = NULL, lock_path = NULL, feature = NULL,
  rate_limit_headers = NULL)

normalize_error(lm, status, body, ..., headers = json_object(),
  now = Sys.time())

retryable(error)

}
\arguments{
\item{message}{The message.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{code}{The contract error code, such as \code{"rate_limit"}.}
\item{provider}{A provider id, such as \code{"openai"}, \code{"anthropic"} or \code{"xai"}. \code{providers()} lists them; an underscore is read as a hyphen.}
\item{provider_code}{The provider's own code.}
\item{status}{The HTTP status.}
\item{request_id}{The provider's request id.}
\item{retry_after}{The provider's wait advice in seconds.}
\item{partial}{A partial response, when one exists.}
\item{part_index}{The part a stream error concerns.}
\item{model}{The model string that failed to route.}
\item{providers}{The providers a model string matched.}
\item{credential_hint}{How to fix the credential, shown under the message.}
\item{path}{The credentials file.}
\item{lock_path}{The lock file.}
\item{feature}{The request path of a refused feature.}
\item{rate_limit_headers}{Rate-limit headers the provider sent, kept verbatim.}
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{body}{The reply body.}
\item{headers}{HTTP headers as a named list.}
\item{now}{The current time, as a \code{POSIXct}. A fixed value makes the result reproducible.}
\item{error}{A condition.}
}
\details{

\code{retryable()} is \code{TRUE} for rate limits, timeouts, server errors,
transport failures and lock contention. lm15 never retries by itself; a retry
loop is yours to write, with \code{error$retry_after} as advice.
\code{AuthOperationError} (sign-in lifecycle) carries \code{reason},
\code{stage}, \code{commit_state} and \code{recovery}.

}
\value{
A condition object; \code{retryable()} returns \code{TRUE} or \code{FALSE}.
}
\examples{

client <- fake_lm(list(lm15_error("Slow down.", code = "rate_limit", retry_after = 2)))
err <- tryCatch(complete(client, request("m", list(message_user("hi")))),
                RateLimitError = identity)
retryable(err)
err$retry_after

}
