\name{entrezGeneByID}
\alias{entrezGeneByID}
\title{Create a Query String for an Entrez Gene Identifier}
\description{
Given a set of UniGene identifiers this function creates a set of URLs
that an be used to either open a browser to the requested location or
that can be used as anchors in the construction of HTML output.
}
\usage{
entrezGeneByID(query)
}
\arguments{
  \item{query}{Entrez Gene identifiers.}
}
\details{
  Using NCBI we construct appropriate strings for directing a web
  browser to the Entrez Genes specified by their IDs.
}
\value{
A character vector containing the query string.
}
\references{NCBI, \url{http://www.ncbi.nih.gov/} }
\author{Marc Carlson}
\note{Be very careful about automatically querying this resource. It is
  considered antisocial behavior by the owners. }

\examples{
  q1<-entrezGeneByID(c("100", "1002"))
  q1
  if( interactive())
    browseURL(q1[1])

}
\keyword{interface}
