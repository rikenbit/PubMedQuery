\name{getPMInfo}
\alias{getPMInfo}
\title{extract publication details and abstract from annotate::pubmed function output  }
\description{
extract publication details and abstract from annotate::pubmed function output  
}
\usage{
getPMInfo(x)
}
\arguments{
  \item{x}{ an object of class xmlDocument; assumed to be result of a pubmed() call}
}
\details{
uses xmlDOMApply to extract and structure key features of the XML tree returned
by annotate::pubmed()
}
\value{
a list with one element per pubmed id processed by pubmed.  Each element of
the list is in turn a list with elements for author list, title, journal
info, and abstract text.
}
\author{Vince Carey <stvjc@channing.harvard.edu>}
\note{this should be turned into a method returning an instance of
a formal class representing articles.    }


\examples{
demo <- pubmed("11780146", 
    "11886385", "11884611")
getPMInfo(demo)
}
\keyword{ models }
