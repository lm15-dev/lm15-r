\name{oauth_callback_listener}
\alias{oauth_callback_listener}
\alias{credential_store}
\title{Local authorization callbacks and protected credential storage}
\description{Receive an OAuth authorization code on loopback or update an entry in a credential store under a cross-process lock. No browser is opened automatically.}
\usage{
oauth_callback_listener(..., expected_state = NULL, host = "127.0.0.1",
  port = 0L, path = "/callback")
credential_store(..., path = credentials_path(env = env), env = NULL,
  lock_timeout = 30)
}
\arguments{
\item{...}{Must be empty.}
\item{expected_state}{Expected OAuth state string. Supply a cryptographically random state and include it in the authorization URL. NULL disables the state comparison and should only be used when the calling application's flow does not use state.}
\item{host}{Must be \code{"127.0.0.1"}; public-interface listeners are refused.}
\item{port}{TCP port, or zero to choose an available port.}
\item{path}{Callback URL path for the listener, or credential file path for the store. Home-relative store paths resolve against the supplied environment.}
\item{env}{Named environment vector; NULL uses the process environment. An empty vector disables ambient environment lookup.}
\item{lock_timeout}{Maximum seconds to wait for the credential file's cooperative lock.}
}
\details{The listener exposes \code{redirect_uri}, \code{wait(timeout = 300)} and \code{close()}. Waiting processes HTTP callbacks on the R thread. Wrong paths, wrong state, missing codes and duplicate parameters are refused without accepting a login. A provider error ends the wait with an authentication error, without printing provider diagnostics. Always close the listener in a \code{finally} block. Callback results hide the authorization code in printing and structure displays.

The store exposes \code{read()}, \code{get(provider)}, \code{mutate(provider, update)} and \code{remove(provider)}. Reads return actual secrets and must not be logged. An update function receives the current entry (or NULL) while the lock is held. Return a JSON object to replace it, or NULL to leave it unchanged. Other entries are preserved. Invalid existing files are not overwritten. Removal deletes only the named entry. Files are saved through the same private atomic writer used for login renewal. The lock coordinates lm15 processes, not foreign credential writers.}
\value{A listener or store object with the functions described above. Listener wait returns a protected result containing \code{code} and \code{state}.}
