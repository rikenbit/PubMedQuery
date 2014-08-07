\name{catalogDAVIDResultsByTool}
\alias{catalogDAVIDResultsByTool}
\title{ Create a catalog of types of DAVID results. }
\description{
Loops through values of \code{tool}, for the specified value of \code{annot}.  Runs each DAVID query, and saves the result to create a catalog of types of DAVID results. }

\usage{
catalogDAVIDResultsByTool(annot = NULL, sleepSeconds = 10, details = FALSE, ...)
}
\arguments{
  \item{annot}{ See  \code{\link{DAVIDQuery}} . }
  \item{sleepSeconds}{ \code{\link{DAVIDQueryLoop}} .}
  \item{details}{ If TRUE, a list of intermediate results is returned for each catalog item;  otherwise, just the final query result. Default is FALSE.}
  \item{...}{ Extra args passed to DAVIDQuery. }
}
\details{
The purpose is to check comprehensively whether there are results that could be better formatted than the default output or the reformatting provided  by \code{\link{formatDAVIDResult}}.
}
\value{
A list of outputs from DAVIDQuery. Automatically assigned to the name \code{catalogOfDAVIDResultsByTool.ANNOT} where ANNOT is replaced by the \code{annot} argument.
}
\author{ Roger Day }
\keyword{ database }
