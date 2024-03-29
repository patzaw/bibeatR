% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zzz.R
\name{connectToZotero}
\alias{connectToZotero}
\title{Connect Zotero}
\usage{
connectToZotero(
  baseURL = NULL,
  key = NULL,
  excludedTypes = "attachment",
  connection = 1,
  remember = TRUE,
  importPath = NULL
)
}
\arguments{
\item{baseURL}{a character string (e.g. "https://api.zotero.org").}

\item{key}{a character string (see https://www.zotero.org/settings/keys)}

\item{connection}{the id of the connection already registered to use. By
default the first registered connection is used.}

\item{remember}{if TRUE the connection is registered. All the registered
connections can be listed with \link{lsZoteroConnections} and any of
them can be forgotten with \link{forgetZoteroConn}.}
}
\value{
This function does not return any value. It prepares the zotapir
environment to allow transparent Zotero bibliography calls.
}
\description{
Connect Zotero
}
\details{
Be carefull that you should reconnect to Zotero each time
the environment is reloaded.

@seealso \link{forgetZoteroConn}, \link{checkZoteroConn}, \link{lsZoteroConnections}
}
