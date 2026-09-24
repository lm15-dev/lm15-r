\name{turn}
\alias{turn}
\alias{materialize_turn}
\title{Collect one live turn without owning the live session}
\description{Buffer a bounded view of live events and obtain text, decoded audio, tool calls, usage and the reason the turn ended. These are application values, not new canonical wire types.}
\usage{
turn(session, ..., max_events = 1024L, max_bytes = 128 * 1024^2,
  timeout = 120)
materialize_turn(events, ...)
}
\arguments{
\item{session}{A live session. Send the desired input before creating a turn view.}
\item{...}{Must be empty.}
\item{max_events}{Maximum number of buffered events.}
\item{max_bytes}{Maximum estimated R object size of buffered events. Larger or latency-sensitive streams should use the session's raw event reader.}
\item{timeout}{Maximum seconds to wait for each next event.}
\item{events}{A list of canonical live server events.}
}
\details{A view exposes \code{next_event()}, \code{snapshot()}, \code{result()} and \code{close()}. The shared \code{result(view)} function also obtains the result. A result stops at a tool call so the application can answer it; manual reading can continue across tool calls. Already-read events remain in the result. Closing a view does not close the underlying live session. An incomplete view cannot produce a successful result; inspect its snapshot instead.

A collected turn has \code{ended_by}, \code{ok}, \code{text}, raw \code{audio}, \code{audio_media_type}, \code{tool_calls}, \code{usage}, \code{error} and \code{events}. Only \code{turn_end} is successful. Usage sums all usage-bearing events field by field; an absent counter remains unknown rather than becoming zero. Different audio media types cannot be concatenated silently. Materialization buffers data; consume individual events for immediate playback.}
\value{A turn view, or a materialized turn.}
