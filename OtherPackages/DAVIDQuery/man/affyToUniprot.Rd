\name{affyToUniprot}
\alias{affyToUniprot}
\title{ Obtain Affymetrix probeset IDs for given Uniprot IDs.  }
\description{
Obtain Affymetrix probeset IDs for given Uniprot IDs, using DAVIDQuery.
}
\usage{
affyToUniprot(ids = "88736_AT", ...)
}
\arguments{
  \item{ids}{ Affymetrix probeset IDs. }
  \item{\dots}{ Args to be passed to \code{DAVIDQuery()}.  }
}
\value{
The output of DAVIDQuery.  If only the DAVIDQueryResult component is desired, include the arg details=FALSE.}
\author{ Roger Day }
\note{ There is currently no provision for using \code{\link{DAVIDQueryLoop}} .}
\seealso{  \code{\link{DAVIDQuery}} }
\keyword{ database }
