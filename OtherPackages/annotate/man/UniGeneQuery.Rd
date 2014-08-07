\name{UniGeneQuery}
\alias{UniGeneQuery}
\title{Create a Query String for a UniGene Identifier  }
\description{
Given a set of UniGene identifiers this function creates a set of URLs
that an be used to either open a browser to the requested location or
that can be used as anchors in the construction of HTML output.
}
\usage{
UniGeneQuery(query, UGaddress="UniGene/", type="CID")
}
\arguments{
  \item{query}{The UniGene identifiers. }
  \item{UGaddress}{ The address of UniGene, within the NCBI repository.}
  \item{type}{What type of object is being asked for; eithe CID or UGID }
}
\details{
  Using published details from NCBI we construct an appropriate string
  for directing a web browser to the information available at the NCBI
  for that genomic product (usually an EST).
}
\value{
A character vector containing the query string.
}
\references{NCBI, \url{http://www.ncbi.nih.gov/} }
\author{Robert Gentleman}
\note{Be very careful about automatically querying this resource. It is
  considered antisocial behavior by the owners. }

\examples{
  q1<-UniGeneQuery(c("Hs.293970", "Hs.155650"))
  q1
  if( interactive())
    browseURL(q1[1])

}
\keyword{interface}
