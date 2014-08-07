\name{pm.titles}
\alias{pm.titles}
\title{Obtain the titles of the PubMed abstracts.}
\description{
 This function returns the titles from a list of PubMed abstracts.
}
\usage{
pm.titles(absts)
}
\arguments{
  \item{absts}{The list of PubMed abstracts.}
}
\details{
  It simply uses \code{sapply}.
}
\value{
A character vector of length equal to the number of abstracts. Each
element is the title of the corresponding abstract.
}
\author{Robert Gentleman}
\seealso{\code{\link{pm.abstGrep}}}
\examples{
  library("hgu95av2.db")
  hoxa9 <- "37806_at"
  absts <- pm.getabst(hoxa9, "hgu95av2")
  pm.titles(absts)[[1]][[1]]
}
\keyword{manip}
