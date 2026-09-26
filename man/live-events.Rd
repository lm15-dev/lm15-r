\name{live-events}
\alias{live-events}
\alias{live_client_turn_event}
\alias{live_client_audio_event}
\alias{live_client_image_event}
\alias{live_client_text_event}
\alias{live_client_tool_result_event}
\alias{live_client_interrupt_event}
\alias{live_client_end_audio_event}
\alias{live_server_audio_event}
\alias{live_server_text_event}
\alias{live_server_tool_call_event}
\alias{live_server_tool_call_delta_event}
\alias{live_server_interrupted_event}
\alias{live_server_turn_end_event}
\alias{live_server_usage_event}
\alias{live_server_error_event}
\title{Live session events}
\description{
Events you send to a live session (\code{live_client_*}) and events it sends back (\code{live_server_*}).
}
\usage{
live_client_turn_event(parts, ..., turn_complete = TRUE)

live_client_audio_event(data, ..., media_type = "audio/pcm;rate=16000")

live_client_image_event(data, ..., media_type = "image/jpeg")

live_client_text_event(text, ...)

live_client_tool_result_event(id, content, ...)

live_client_interrupt_event(...)

live_client_end_audio_event(...)

live_server_audio_event(data, ..., media_type = NULL)

live_server_text_event(text, ...)

live_server_tool_call_event(id, name, ..., input = json_object())

live_server_tool_call_delta_event(input_delta, ..., id = NULL, name = NULL)

live_server_interrupted_event(...)

live_server_turn_end_event(..., usage = .new_value("Usage", list()))

live_server_usage_event(..., usage = .new_value("Usage", list()))

live_server_error_event(error, ...)

}
\arguments{
\item{parts}{The turn's parts.}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{turn_complete}{Whether the turn is finished.}
\item{data}{Base64 media.}
\item{media_type}{The media type, such as \code{"image/png"}.}
\item{text}{Text.}
\item{id}{The tool call's id.}
\item{content}{The tool's result parts.}
\item{name}{A name.}
\item{input}{Tool-call arguments: a JSON object (\code{json_object()}); a delta carries a text fragment of the JSON instead.}
\item{input_delta}{A fragment of the call's JSON arguments.}
\item{usage}{Token counts, from \code{usage()}.}
\item{error}{An \code{error_detail()} value.}
}
\value{
A live event value.
}
\examples{

as_json(live_client_text_event("Hello there"))

}
