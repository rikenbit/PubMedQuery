\name{pmidQuery}
\alias{pmidQuery}
\title{A function to query PubMed}
\description{
  Given a PMID, will create a URL which can be used to open a
  browser and retrieve the specified information from PubMed.
}
\usage{
pmidQuery(query)
}
\arguments{
  \item{query}{ The PubMed ID (or IDs)}
}
\details{
  Using ublished details from NCBI we construct an appropriate string
  for directing a web browser to the information available at the NCBI.
}
\value{
  A character string containing the appropriate URL
}
\references{NCBI, \url{http://www.ncbi.nih.gov/} }
\author{Jeff Gentry}

\seealso{\code{\link{UniGeneQuery}}}

\examples{
  a <- "9695952"
  pmidQuery(a)
}
\keyword{interface}

