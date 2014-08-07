\name{uniprotToAffy}
\alias{uniprotToAffy}
\title{ Obtain Affymetrix probeset IDs for given Uniprot IDs. }
\description{
  Obtain Affymetrix probeset IDs for given Uniprot IDs, using DAVIDQuery.
}
\usage{
uniprotToAffy(uid = "O00161", ...)
}
\arguments{
  \item{uid}{ Uniprot IDs, either a string with the IDs separated by commas, or else a character vector. }
  \item{\dots}{ Args to be passed to \code{DAVIDQuery()}. }
}
\value{
	The output of \code{\link{DAVIDQuery}}.  If only the DAVIDQueryResult component is desired, include the arg \code{details=FALSE}.  If probesets from a specific chip are desired, then you can intersect these results with the results of \code{\link{getAffyProbesetList}}. 
}
\note{ There is currently no provision for using \code{\link{DAVIDQueryLoop}} .}
\author{ Roger Day }
\seealso{  \code{\link{DAVIDQuery}} }
\keyword{ database }
