\name{generation}
\alias{generation}
\alias{image_generate}
\alias{speech_generate}
\alias{generation_build}
\alias{generation_parse}
\alias{image_generation_request}
\alias{image_generation_response}
\alias{speech_generation_request}
\alias{speech_generation_response}
\title{Image and speech generation}
\description{
Generate images and speech on providers that sell them. The build and parse halves work without a network.
}
\usage{
image_generate(lm, request, ...)

speech_generate(lm, request, ...)

generation_build(lm, request, ...)

generation_parse(lm, request, body, ..., headers = list())

image_generation_request(model, prompt, ..., size = NULL, images = list(),
  extensions = NULL)

image_generation_response(images, ..., text = NULL, id = NULL, model = NULL,
  usage = .new_value("Usage", list()), provider_data = NULL)

speech_generation_request(model, prompt, ..., voice = NULL, format = NULL,
  extensions = NULL)

speech_generation_response(audio, ..., id = NULL, model = NULL,
  usage = .new_value("Usage", list()), provider_data = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{request}{An \code{image_generation_request()} or \code{speech_generation_request()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{body}{A reply body: a JSON string, raw bytes, or an already parsed JSON object.}
\item{headers}{HTTP headers as a named list.}
\item{model}{A model identifier.}
\item{prompt}{What to generate or say.}
\item{size}{The image size, such as \code{"1024x1024"}.}
\item{images}{For requests: images to edit; for responses: the generated images.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{text}{Text the provider returned with the images.}
\item{id}{The provider's identifier for the object.}
\item{usage}{Token counts, from \code{usage()}.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{voice}{The voice.}
\item{format}{The audio format, such as \code{"mp3"}.}
\item{audio}{The generated audio part.}
}
\value{
An ImageGenerationResponse or SpeechGenerationResponse, a request value, or a wire request.
}
\examples{

img <- image_generation_request("gpt-image-1", "A lighthouse at dusk", size = "1024x1024")
wire <- generation_build(new_lm("openai", api_key = "sk-example"), img)
wire$method

}
