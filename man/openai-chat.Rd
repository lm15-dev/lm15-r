\name{openai-chat}
\alias{openai-chat}
\alias{request_from_openai_chat}
\alias{response_from_openai_chat}
\alias{complete_from_openai_chat}
\alias{stream_from_openai_chat}
\alias{route_openai_chat}
\alias{resolve_openai_chat}
\alias{openai_chat_model_string}
\title{Code written for the OpenAI Chat Completions format}
\description{

For code that already builds OpenAI Chat Completions bodies (or litellm
calls): read such a body into a canonical request, or send it through a router
to any provider. Every field is mapped, passed through as an extension, or
refused by name; nothing is dropped silently.

}
\usage{
request_from_openai_chat(body, ..., compat = NULL, provider = "openai-chat")

response_from_openai_chat(body, ..., model = NULL, choice = NULL)

complete_from_openai_chat(lm, body, ..., streaming = isTRUE(body$stream),
  on_event = function(event) invisible(NULL))

stream_from_openai_chat(lm, body, on_event, ...)

route_openai_chat(router, body, ...)

resolve_openai_chat(router, model, ...)

openai_chat_model_string(model, ...)

}
\arguments{
\item{body}{A Chat Completions request body as a JSON object (or JSON text); for \code{response_from_openai_chat()}, a response body.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{compat}{The server dialect the body was written for, such as \code{"groq"}.}
\item{provider}{The provider the body was written for.}
\item{model}{The model, when the body carries none; for model-string functions, the model string (\code{provider:model} or litellm's \code{provider/model}).}
\item{choice}{Which choice to read when the body has several.}
\item{lm}{A router from \code{new_router()} or a client from \code{new_lm()}.}
\item{streaming}{Whether to stream; by default the body's \code{stream} field.}
\item{on_event}{A function called with each event while streaming.}
\item{router}{A router from \code{new_router()}.}
}
\value{
A Request, a Response, a routing list, or a model string.
}
\examples{

body <- json_object(model = "gpt-5-mini",
  messages = json_array(json_object(role = "user", content = "Hello")),
  max_tokens = 50L)
req <- request_from_openai_chat(body)
as_json(req)
openai_chat_model_string("groq/llama-3.3-70b-versatile")

}
