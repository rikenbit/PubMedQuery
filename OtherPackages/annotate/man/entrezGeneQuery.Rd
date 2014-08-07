\name{entrezGeneQuery}
\alias{entrezGeneQuery}
\title{Create a Query String for Entrez Genes}
\description{
Given a set of search terms this function creates a set of URLs
that an be used to either open a browser to the requested location or
that can be used as anchors in the construction of HTML output.
}
\usage{
entrezGeneQuery(query)
}
\arguments{
  \item{query}{The UniGene identifiers. }
}
\details{
  Using NCBI we construct an appropriate string for directing a web
  browser to information about genes of that type at NCBI.
}
\value{
A character vector containing the query string.
}
\references{NCBI, \url{http://www.ncbi.nih.gov/} }
\author{Marc Carlson}
\note{Be very careful about automatically querying this resource. It is
  considered antisocial behavior by the owners. }

\examples{
  q1<-entrezGeneQuery(c("leukemia", "Homo sapiens"))
  q1
  if( interactive())
    browseURL(q1[1])

}
\keyword{interface}
