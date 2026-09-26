\name{cache}
\alias{cache}
\alias{cache_create}
\alias{cache_get}
\alias{cache_list}
\alias{cache_update}
\alias{cache_delete}
\alias{cache_op_build}
\alias{cached_request}
\alias{cached_prefix}
\alias{cache_info}
\alias{cache_page}
\title{Stored prompt caches}
\description{
Create a provider-side cache of a request prefix (Gemini's cached contents; automatic or marker caching elsewhere) and send later requests that reuse it.
}
\usage{
cache(lm, prefix, ..., ttl_seconds = NULL, label = NULL)

cache_create(lm, prefix, ..., ttl_seconds = NULL, label = NULL)

cache_get(lm, id, ...)

cache_list(lm, ..., limit = 20L, cursor = NULL)

cache_update(lm, id, ..., ttl_seconds)

cache_delete(lm, id, ...)

cache_op_build(lm, action, ..., prefix = NULL, id = NULL, limit = 20L,
  cursor = NULL, ttl_seconds = NULL, label = NULL)

cached_request(cached, messages, ..., config = NULL)

cached_prefix(prefix, ..., resource = NULL, provider = NULL)

cache_info(id, model, ..., tokens = NULL, created_at = NULL,
  expires_at = NULL, label = NULL, provider_data = NULL)

cache_page(..., items = list(), next_cursor = NULL)

}
\arguments{
\item{lm}{A provider client from \code{new_lm()}. Operations that route a model string also accept a router from \code{new_router()}.}
\item{prefix}{A request whose messages are the prefix to cache (its settings must be the defaults).}
\item{\dots}{Unused. Any argument given here is an error, so a misspelled option name is caught instead of ignored.}
\item{ttl_seconds}{The cache's lifetime in seconds.}
\item{label}{A label to show, or \code{NULL}.}
\item{id}{The provider's identifier for the object.}
\item{limit}{The largest number of entries to ask for.}
\item{cursor}{A paging cursor returned by a previous page, or \code{NULL} for the first page.}
\item{action}{\code{"create"}, \code{"get"}, \code{"list"}, \code{"update"} or \code{"delete"}.}
\item{cached}{A \code{cached_prefix()} from \code{cache()}.}
\item{messages}{The messages that follow the cached prefix.}
\item{config}{Settings for the new request.}
\item{resource}{The stored cache, a \code{cache_info()} value.}
\item{provider}{The provider the cache belongs to, when a router created it.}
\item{model}{A model identifier.}
\item{tokens}{Tokens in the cache.}
\item{created_at}{An RFC 3339 timestamp, or \code{NULL}.}
\item{expires_at}{An RFC 3339 timestamp with a time zone, or \code{NULL} when unknown.}
\item{provider_data}{The provider's own reply fields, kept verbatim for inspection, or \code{NULL}.}
\item{items}{A list of values on this page.}
\item{next_cursor}{The cursor of the next page, or \code{NULL}.}
}
\value{
A CachedPrefix, CacheInfo, CachePage, Request, wire request, or invisible \code{NULL}.
}
\examples{

prefix <- request("gemini-2.5-flash", list(message_user("A long document ...")))
cached <- cached_prefix(prefix)   # no stored resource: the request is sent whole
req <- cached_request(cached, list(message_user("Summarise it.")))
length(req$messages)

}
