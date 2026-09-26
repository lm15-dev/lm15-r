\name{models}
\alias{models}
\alias{list_models}
\alias{build_models_request}
\alias{parse_models_response}
\alias{model_info}
\alias{model_origin}
\alias{inference_model_info}
\alias{inference_pricing}
\alias{new_model_registry}
\alias{discover_model_registry}
\alias{estimate_cost}
\title{Model listings and metadata}
\description{
List a provider's models, describe models with metadata, and collect metadata in a registry a router can use to route bare model names. Metadata is advisory: it never changes how a request is built.
}
\usage{
list_models(lm, ...)

build_models_request(lm, ...)

parse_models_response(lm, body, ...)

model_info(id, provider, api_family, ..., aliases = list(),
  origin = .new_value("ModelOrigin", list()), inference = NULL,
  extensions = NULL)

model_origin(..., type = "provider", id = NULL, base_model = NULL,
  provider_data = NULL)

inference_model_info(..., input_modalities = list("text"),
  output_modalities = list("text"), context_window = NULL,
  max_output_tokens = NULL, supports_reasoning = FALSE,
  reasoning_efforts = list(), pricing = NULL, extensions = NULL)

inference_pricing(..., input_per_million = NULL, output_per_million = NULL,
  cache_read_per_million = NULL, cache_write_per_million = NULL,
  currency = "USD", dimensions = NULL)

new_model_registry(models = list(), ...)

discover_model_registry(..., catalogs = NULL, libraries = .libPaths())

estimate_cost(pricing, usage, ...)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{body}{A reply body: a JSON string, raw bytes, or an already parsed JSON object.}
\item{id}{The provider's identifier for the object.}
\item{provider}{The provider id.}
\item{api_family}{The wire family, such as \code{"openai_chat"}.}
\item{aliases}{Other names for the model.}
\item{origin}{Where the model comes from, a \code{model_origin()} value.}
\item{inference}{An \code{inference_model_info()} value, or \code{NULL}.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{type}{\code{"provider"} or another origin type.}
\item{base_model}{The model this one derives from.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{input_modalities}{What the model reads, such as \code{list("text", "image")}.}
\item{output_modalities}{What it writes.}
\item{context_window}{Its context window in tokens.}
\item{max_output_tokens}{Its largest output.}
\item{supports_reasoning}{Whether it reasons.}
\item{reasoning_efforts}{The effort words it accepts.}
\item{pricing}{An \code{inference_pricing()} value.}
\item{input_per_million}{Price per million input tokens.}
\item{output_per_million}{Price per million output tokens.}
\item{cache_read_per_million}{Price per million cached tokens read.}
\item{cache_write_per_million}{Price per million tokens written to cache.}
\item{currency}{The currency code.}
\item{dimensions}{Other price dimensions, as a JSON object.}
\item{models}{A list of \code{model_info()} values.}
\item{catalogs}{Files to read (JSON catalogs), or \code{NULL} to discover them in installed packages.}
\item{libraries}{Library paths to search for packages that ship \code{lm15/model-catalog.json}.}
\item{usage}{Token counts, from a response.}
}
\value{
A list of ModelInfo values, a wire request, a ModelInfo/ModelOrigin/pricing value, a registry, or an estimated cost (a number, or \code{NULL} when unknown).
}
\examples{

gpt <- model_info("gpt-5-mini", "openai", "openai_responses",
  inference = inference_model_info(pricing = inference_pricing(input_per_million = 0.25,
                                                              output_per_million = 2)))
registry <- new_model_registry(list(gpt))
estimate_cost(gpt$inference$pricing, usage(input_tokens = 1000L, output_tokens = 500L))
resolve(new_router(catalog = registry), "gpt-5-mini")

}
