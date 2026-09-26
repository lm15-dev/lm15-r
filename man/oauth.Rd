\name{oauth}
\alias{oauth}
\alias{generate_pkce}
\alias{pkce_challenge}
\alias{jwt_rs256}
\alias{sigv4_sign}
\alias{token_exchange_build}
\alias{token_exchange_parse}
\alias{oauth_callback_listener}
\title{Protocol building blocks}
\description{
The protocol pieces lm15's sign-in and cloud credentials are made of, for applications that need them directly: PKCE, RS256 JWTs, AWS Signature Version 4, token exchanges and a loopback callback listener.
}
\usage{
generate_pkce()

pkce_challenge(verifier)

jwt_rs256(header, claims, private_key_pem)

sigv4_sign(method, url, credential, ..., region, service, headers = list(),
  body = raw(), now = Sys.time())

token_exchange_build(provider, rung, input, ..., now = Sys.time(),
  settings = list())

token_exchange_parse(provider, rung, status, body, ..., now = Sys.time())

oauth_callback_listener(..., expected_state = NULL, host = "127.0.0.1",
  port = 0L, path = "/callback")

}
\arguments{
\item{verifier}{A PKCE verifier.}
\item{header}{The JWT header, a JSON object.}
\item{claims}{The JWT claims, a JSON object.}
\item{private_key_pem}{An RSA private key in PEM.}
\item{method}{The HTTP method.}
\item{url}{The URL.}
\item{credential}{An \code{aws_credentials()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{region}{The AWS region.}
\item{service}{The AWS service name.}
\item{headers}{HTTP headers as a named list.}
\item{body}{The request body (raw), or for \code{token_exchange_parse()} the reply.}
\item{now}{The current time, as a \code{POSIXct}. A fixed value makes the result reproducible.}
\item{provider}{A provider id, such as \code{"openai"}, \code{"anthropic"} or \code{"xai"}. \code{providers()} lists them; an underscore is read as a hyphen.}
\item{rung}{The chain rung, such as \code{"web-identity"}.}
\item{input}{The rung's input (a credential file's contents, ...).}
\item{settings}{Host settings for a cloud provider, such as \code{list(region = "us-east-1")} or \code{list(project = "my-project")}. A setting given here wins over the environment.}
\item{status}{An HTTP status code.}
\item{expected_state}{The OAuth state the return must carry.}
\item{host}{The address to listen on: only 127.0.0.1.}
\item{port}{The port, or 0 for any free one.}
\item{path}{The callback path.}
}
\value{
A PKCE pair or challenge, a JWT, signed headers, a token request, a credential, or a listener with \code{redirect_uri}, \code{wait()} and \code{close()}.
}
\examples{

pkce_challenge("dBjftJeZ4CVP-mJ92K9pVlxdFKrJK1GZkDy0BNIAVQM") # RFC 7636 Appendix B

}
