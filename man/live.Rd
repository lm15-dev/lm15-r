\name{live}
\alias{live}
\alias{live_config}
\alias{audio_format}
\alias{turn}
\alias{materialize_turn}
\alias{build_live_completion}
\alias{live_connection_request}
\alias{live_setup_frames}
\alias{live_encode}
\alias{live_decode}
\alias{websocket_connect}
\title{Live sessions}
\description{

A live session is a two-way WebSocket conversation (OpenAI Realtime, Gemini
Live): send text, audio or images, and read events as they arrive.
\code{turn()} opens a view that collects one turn's events up to its boundary.

}
\usage{
live(lm, config, ..., connect = lm$live_connect \%||\% .websocket_connection,
  timeout = 30, max_queue = 1024L, max_frame_bytes = 32 * 1024^2,
  max_turn_bytes = 128 * 1024^2)

live_config(model, ..., system = NULL, tools = list(), voice = NULL,
  input_format = NULL, output_format = NULL, extensions = NULL)

audio_format(encoding, sample_rate, ..., channels = 1L)

turn(session, ..., max_events = 1024L, max_bytes = 128 * 1024^2, timeout = 120)

materialize_turn(events, ...)

build_live_completion(lm, request, ...)

live_connection_request(lm, config, ...)

live_setup_frames(lm, config, ...)

live_encode(lm, config, event, ...)

live_decode(lm, frame, ...)

websocket_connect(url, headers = list(), timeout = 30, max_queue = 1024L,
  max_frame_bytes = 32 * 1024^2, ..., ca_bundle = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{config}{A \code{live_config()} value.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{connect}{A function that opens the connection (the native WebSocket by default).}
\item{timeout}{Seconds to wait.}
\item{max_queue}{The most unread events kept.}
\item{max_frame_bytes}{The largest frame accepted.}
\item{max_turn_bytes}{The most bytes one turn may collect.}
\item{model}{The model.}
\item{system}{A system prompt.}
\item{tools}{A list of tools.}
\item{voice}{The voice.}
\item{input_format}{An \code{audio_format()} for audio you send.}
\item{output_format}{An \code{audio_format()} for audio you receive.}
\item{extensions}{A JSON object of provider-specific fields passed through unchanged (\code{json_object()}), or \code{NULL}.}
\item{encoding}{\code{"pcm16"}, \code{"opus"}, \code{"mp3"} or \code{"aac"}.}
\item{sample_rate}{Samples per second.}
\item{channels}{The number of channels.}
\item{session}{A session from \code{live()}.}
\item{max_events}{The most events a turn may collect.}
\item{max_bytes}{The most bytes a turn may collect.}
\item{events}{A list of stream events, in order.}
\item{request}{A canonical request from \code{request()}.}
\item{event}{A client event.}
\item{frame}{A received frame (JSON text).}
\item{url}{A \code{ws://} or \code{wss://} URL.}
\item{headers}{Handshake headers as a named list.}
\item{ca_bundle}{A file of trusted certificates for a private server, or \code{NULL}.}
}
\details{
A turn over its budget raises \code{CollectionLimitError}, keeping the events it accepted. TLS certificates and host names are always verified.
}
\value{
A session, a LiveConfig/AudioFormat value, a turn view, a Turn, frames, events, or a connection.
}
\examples{

cfg <- live_config("gpt-realtime", system = "Be brief.")
frames <- live_setup_frames(new_lm("openai", api_key = "sk-example"), cfg)
length(frames)

}
