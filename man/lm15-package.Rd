\name{lm15-package}
\alias{lm15-package}
\alias{lm15}
\title{lm15: provider-neutral foundation model conversations}
\description{

lm15 turns one request into the right call for each provider (OpenAI, Anthropic,
Google, xAI, Azure, AWS, Google Cloud, open-model hosts and local servers) and
turns every reply into the same response. You build a request, ask a client to
send it, and keep the whole response. Nothing is sent, retried, truncated or
remembered behind your back.

This R package implements the shared lm15 contract, the same one the Python,
TypeScript, Rust and Go packages implement, and passes the same conformance
suite. Names follow R conventions; the words are the family's.

}
\details{

Start here:
\itemize{
\item \code{\link{request}()} and \code{\link{message_user}()} build a request.
\item \code{\link{new_router}()} picks a provider from the model name;
\code{\link{new_lm}()} talks to one provider directly.
\item \code{\link{complete}()} returns a response; \code{\link{stream}()}
calls your function with each event as it arrives.
\item \code{\link{response_text}()}, \code{\link{tool_calls}()} and
\code{\link{response_data}()} read a response.
\item \code{\link{login}()} and \code{\link{connect}()} sign in to a
subscription or save a key; \code{\link{explain_auth}()} says which
credential a request would use and why.
}
When a provider's wire cannot take something you asked for, lm15 adapts it
and records what it did on the response (\code{response$adaptations}), or
refuses before sending; see \code{\link{plan}()}. Errors are conditions with
contract classes (\code{RateLimitError}, \code{AuthError}, ...); see
\code{\link{lm15_error}()}.

Networking uses the optional \pkg{curl} package. Live sessions need a libcurl
with WebSocket support at install time. Sign-in, PKCE and request signing use
\pkg{openssl}; the loopback sign-in listener uses \pkg{httpuv} and \pkg{later}.
Missing optional packages fail with a message naming them.

}
\value{
Not applicable.
}
\examples{

req <- request("openai:gpt-5-mini", list(message_user("Say hello.")),
               config = config(max_tokens = 50L))
client <- fake_lm(list("Hello."))       # a scripted stand-in; no network
response_text(complete(client, req))

}
\docType{package}
