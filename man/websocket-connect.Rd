\name{websocket_connect}
\alias{websocket_connect}
\title{Verified native WebSocket connections}
\description{Open a pull-based WebSocket connection through libcurl. This is the default desktop connector used by live sessions and realtime completions.}
\usage{
websocket_connect(url, headers = list(), timeout = 30, max_queue = 1024L,
  max_frame_bytes = 32 * 1024^2, ..., ca_bundle = NULL)
}
\arguments{
\item{url}{A \code{ws://} or \code{wss://} URL. Use TLS for credentials except on explicitly chosen local endpoints.}
\item{headers}{Uniquely named list of header strings. These may contain credentials and must not be logged.}
\item{timeout}{Connection and send timeout in seconds, at most 3600.}
\item{max_queue}{Connector-interface compatibility argument. This pull-based connector does not maintain an incoming event queue; the higher-level live session enforces its event queue limit.}
\item{max_frame_bytes}{Positive byte limit for outgoing frames, incoming frames and assembled incoming messages.}
\item{...}{Must be empty.}
\item{ca_bundle}{Optional path to a PEM certificate-authority bundle, for example for a private service. NULL uses libcurl's default trust configuration. Certificate and hostname verification cannot be disabled.}
}
\details{Requires libcurl 7.86 or newer built with WebSocket support. TLS connections require TLS 1.2 or newer and verify both the certificate chain and hostname. Redirects are not followed, credentials are never retried, and unsuccessful handshake bodies are discarded. Fragmented messages are reassembled and control frames do not become response content.

The connection returns \code{send(frame)}, \code{receive(timeout)} and \code{close()}. Send accepts text; receive returns a complete message as raw bytes, or NULL after closure. A read deadline does not discard fragments already received: they remain available for a later read. Close is idempotent. Always close connections in a \code{finally} block. Browser sessions use the JavaScript bridge rather than this native connector.}
\value{A list containing send, receive and close functions.}
