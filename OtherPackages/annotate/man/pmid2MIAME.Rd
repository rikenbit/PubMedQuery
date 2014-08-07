\name{pmid2MIAME}
\alias{pmid2MIAME}
\title{use web to populate MIAME instance with pubmed details  }
\description{use web to populate MIAME instance with pubmed details
}
\usage{
pmid2MIAME(pmid)
}
\arguments{
  \item{pmid}{ string encoding PMID }
}
\details{
uses XML library to decode parts of the query response and
load a MIAME object
}
\value{
An instance of class \code{\link[Biobase:class.MIAME]{MIAME}}
}
\author{Vince Carey <stvjc@channing.harvard.edu>}

\examples{
if (interactive()) pmid2MIAME("9843569")
}
\keyword{ models }
