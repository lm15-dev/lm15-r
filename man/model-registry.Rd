\name{new_model_registry}
\alias{new_model_registry}
\alias{discover_model_registry}
\alias{estimate_cost}
\title{Optional model catalogs and cost estimates}
\description{Keep validated model metadata separately from provider request rules. Catalogs never change request serialization.}
\usage{
new_model_registry(models = list(), ...)
discover_model_registry(..., catalogs = NULL, libraries = .libPaths())
estimate_cost(pricing, usage, ...)
}
\arguments{
\item{models}{A list of canonical \code{model_info()} values.}
\item{...}{Must be empty; misspelled arguments are errors.}
\item{catalogs}{A uniquely named list of JSON file paths or zero-argument functions returning a list of canonical model dictionaries or typed model values. NULL discovers packaged data files.}
\item{libraries}{R library directories to search for \code{PACKAGE/lm15/model-catalog.json}. No package startup code is executed.}
\item{pricing}{An \code{inference_pricing()} value.}
\item{usage}{A \code{usage()} value containing known or absent token counts.}
}
\details{The registry exposes \code{add(model, replace = TRUE)}, \code{get(provider, model)}, \code{resolve(model, provider = NULL)}, \code{list(provider = NULL)} and \code{providers()}. Exact model IDs take precedence over aliases. Resolution without a provider returns NULL when several entries match. Replacing a model removes its old aliases.

Discovery processes catalogs in name order. The first entry for a provider/model pair wins. A malformed catalog is skipped with a warning as a whole, not partially imported. An application may pass the registry to \code{new_router(catalog = registry)}; later registry additions are visible to routing.

Cost estimation skips unknown counts and missing rates. The result is a lower bound when any dimension is missing, not an invoice. Currency is the pricing value's currency. Large integer counts that cannot convert exactly to an R number are refused.}
\value{Registry constructors return an object with the functions described above. \code{estimate_cost()} returns one numeric estimate.}
\examples{
registry <- new_model_registry(list(model_info("example", "openai", "openai_responses")))
registry$get("openai", "example")
estimate_cost(inference_pricing(input_per_million = 2), usage(input_tokens = 1000L))
}
