\name{ENVISIONQuery.loop}
\alias{ENVISIONQuery.loop}
\title{Perform queries in chunks to comply with potential limitations...}
\usage{ENVISIONQuery.loop(ids, chunk, queryFun, verbose, ...)}
\description{Perform queries in chunks to comply with potential limitations 
on the query size for Envision Web query system.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{ids}{Character vector representing the input ID set.}
\item{chunk}{maximum size of the portion of the input ID list to be used in a single query Fun call.}
\item{queryFun}{a functions submitting the query to the Envision Web query system.}
\item{verbose}{if TRUE enables diagnostic messages. Default is FALSE.}
\item{...}{additional arguments to be passed to queryFun.}}
