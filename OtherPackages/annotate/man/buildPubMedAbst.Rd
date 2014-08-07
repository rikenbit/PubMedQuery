\name{buildPubMedAbst}
\alias{buildPubMedAbst}
\title{A function to generate an instantiation of a pubMedAbst class }
\description{
  This function will take in a XML tree object and will create an
  instance of a pubMedAbst class.  This instance is returned to the caller.
}
\usage{
  buildPubMedAbst(xml)
}
\arguments{
  \item{xml}{A XMLTree object that corresponds to a Pubmed abstract.}
}
\value{
  This function returns an instantiation of a pubMedAbst object to the
  caller.
}

\author{ Jeff Gentry }
\seealso{\code{\link{pubmed}},\code{\link{genbank}}}
\examples{
   x <- pubmed("9695952","8325638","8422497")
   a <- xmlRoot(x)
   numAbst <- length(xmlChildren(a))
   absts <- list()
   for (i in 1:numAbst) {
      absts[[i]] <- buildPubMedAbst(a[[i]])
   }

}
\keyword{ utilities }
