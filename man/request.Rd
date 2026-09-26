\name{request}
\alias{request}
\alias{message_user}
\alias{message_assistant}
\alias{message_developer}
\alias{message_tool}
\alias{new_message}
\title{Build a request and its messages}
\description{

A request is a model, a list of messages, and optionally a system prompt, tools
and settings. Messages hold parts (text, images, tool calls, data, ...).
\code{message_user()} and friends take a string, a part, or a list of strings
and parts; a character vector is several text parts in one message.

}
\usage{
request(model, messages, ..., system = NULL, tools = list(),
  config = .new_value("Config", list()))

message_user(content, ...)

message_assistant(content, ...)

message_developer(content, ...)

message_tool(id, content, ..., is_error = FALSE, name = NULL)

new_message(role, parts, ..., continuation = list())

}
\arguments{
\item{model}{The model: \code{"provider:model"} (for example \code{"anthropic:claude-haiku-4-5"}) or a bare model name a router can route.}
\item{messages}{A message, or a list of messages from \code{message_user()}, \code{message_assistant()}, \code{message_tool()} or \code{new_message()}.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{system}{A system prompt: a non-empty string or a list of prompt parts, or \code{NULL}.}
\item{tools}{A list of tools from \code{function_tool()} or \code{builtin_tool()}.}
\item{config}{Settings from \code{config()}.}
\item{content}{Content: a string, a content part, or a list of strings and parts.}
\item{id}{The id of the tool call this result answers (\code{tool_calls(response)[[i]]$id}).}
\item{is_error}{Whether the tool failed; the provider is told so.}
\item{name}{The tool's name, needed by some providers when the result is sent back; \code{NULL} lets lm15 find it from the matching call.}
\item{role}{One of \code{"user"}, \code{"assistant"}, \code{"tool"}, \code{"developer"}.}
\item{parts}{A list of content parts.}
\item{continuation}{Provider continuation state that must travel with this value when it is sent back (a list of \code{continuation_state()} values). Keep what a response gave you; do not build it by hand.}
}
\details{

Requests are values: changing a field returns a new, validated request. A
request that breaks a contract rule (a tool result in a user message, an empty
message list, duplicate tool names, ...) is refused when it is built.
\code{new_message()} is the general constructor; it is named with the
package's \code{new_} prefix because \code{message()} would mask
\code{base::message()}.

}
\value{
\code{request()} returns a Request; the others return a Message.
}
\seealso{
\code{\link{content-parts}}, \code{\link{config}}, \code{\link{complete}}
}
\examples{

req <- request("openai:gpt-5-mini",
               list(message_user("What is 2 + 2?")),
               system = "Answer with a number.")
# Continue a conversation: append the assistant's message, then yours.
answer <- complete(fake_lm(list("4")), req)
req$messages <- c(req$messages, list(answer$message, message_user("And 3 + 3?")))
length(req$messages)

}
