\name{config}
\alias{config}
\alias{reasoning}
\alias{tool_choice}
\alias{cache_config}
\alias{function_tool}
\alias{builtin_tool}
\title{Settings and tools}
\description{

\code{config()} holds generation settings. Leave a setting \code{NULL} to use the
provider's default. A setting the provider's wire cannot take is adapted and
recorded, or refused, never silently dropped (see \code{\link{plan}()}).

}
\usage{
config(..., max_tokens = NULL, temperature = NULL, top_p = NULL, top_k = NULL,
  seed = NULL, frequency_penalty = NULL, presence_penalty = NULL,
  stop = list(), response_format = NULL, tool_choice = NULL, reasoning = NULL,
  cache = NULL, service_tier = NULL, user_id = NULL, store = NULL,
  logprobs = NULL, probabilities = NULL, extensions = NULL)

reasoning(effort, ..., thinking_budget = NULL, summary = NULL)

tool_choice(..., mode = "auto", allowed = list(), parallel = NULL)

cache_config(..., mode = "auto", retention = NULL, key = NULL,
  prefix_until_index = NULL, prefix = NULL, resource = NULL)

function_tool(name, ..., description = NULL,
  parameters = json_object(type = "object", properties = json_object()))

builtin_tool(name, ..., config = NULL)

}
\arguments{
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{max_tokens}{The most tokens to generate.}
\item{temperature}{Sampling temperature, 0 or more.}
\item{top_p}{Nucleus sampling, between 0 and 1.}
\item{top_k}{Top-k sampling.}
\item{seed}{A sampling seed.}
\item{frequency_penalty}{Between -2 and 2.}
\item{presence_penalty}{Between -2 and 2.}
\item{stop}{A list of stop sequences.}
\item{response_format}{\code{json_object(type = "json_object")} for any JSON, or a JSON Schema format such as \code{judgments()} returns.}
\item{tool_choice}{A \code{tool_choice()} value.}
\item{reasoning}{A \code{reasoning()} value.}
\item{cache}{A \code{cache_config()} value.}
\item{service_tier}{The provider's service tier word, such as \code{"flex"}.}
\item{user_id}{An opaque end-user identifier for the provider's abuse attribution.}
\item{store}{Whether the provider may store the response (\code{FALSE} opts out).}
\item{logprobs}{Ask for token log-probabilities: 0 for the chosen tokens, n for n alternatives too.}
\item{probabilities}{For judgment requests: \code{"off"}, \code{"if_available"} or \code{"required"}; see \code{\link{judgments}}.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{effort}{One of \code{"off"}, \code{"minimal"}, \code{"low"}, \code{"medium"}, \code{"high"}, \code{"xhigh"}, \code{"max"}.}
\item{thinking_budget}{A reasoning token budget for providers that take one.}
\item{summary}{Show reasoning: \code{"auto"}, \code{"concise"} or \code{"detailed"}.}
\item{mode}{For \code{tool_choice()}: \code{"auto"}, \code{"required"} or \code{"none"}; for \code{cache_config()}: \code{"auto"} or \code{"off"}.}
\item{allowed}{A list of tool names the model may choose from.}
\item{parallel}{Whether several tool calls may come at once, or \code{NULL} for the default.}
\item{retention}{Cache lifetime: \code{"short"} or \code{"long"}.}
\item{key}{A cache key.}
\item{prefix_until_index}{Cache the messages up to this index (0-based).}
\item{prefix}{\code{"stable"} or \code{"history"}: which prefix to cache.}
\item{resource}{The id of a stored cache to read (see \code{\link{cache}}).}
\item{name}{The tool's name.}
\item{description}{What the tool does, for the model.}
\item{parameters}{The tool's arguments as a JSON Schema object; the default takes none.}
\item{config}{For \code{builtin_tool()}: the provider's own settings as a JSON object.}
}
\details{
Nothing derives a tool from an R function: write the schema. \code{builtin_tool()} names a provider-hosted tool such as \code{"web_search"}.
}
\value{
A Config, Reasoning, ToolChoice, CacheConfig, FunctionTool or BuiltinTool value.
}
\examples{

weather <- function_tool("weather", description = "Current weather in a city.",
  parameters = json_object(type = "object",
    properties = json_object(city = json_object(type = "string")),
    required = json_array("city")))
cfg <- config(max_tokens = 200L, temperature = 0.2, reasoning = reasoning("low"),
              tool_choice = tool_choice(mode = "auto"))
req <- request("anthropic:claude-haiku-4-5", list(message_user("Weather in Oslo?")),
               tools = list(weather), config = cfg)

}
