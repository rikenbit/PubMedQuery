\name{getEvidence}
\alias{getEvidence}
\title{Get the Evidence codes for a set of GO terms.}
\description{
  For each mapping of a gene to a GO term there are a set of evidence
  codes that are used. Genes can be mapped using one, or more evidence
  codes and this function obtains the evidence codes for all genes
  provided in the input list.
}
\usage{
getEvidence(inlist)
}
\arguments{
  \item{inlist}{A list of GO identifers.}
}
\value{
  A \code{list} of the same length as the input list, each element is a
  vector of evidence codes.
}
\author{R. Gentleman}

\seealso{\code{\link{getOntology}}, \code{\link{dropECode}}}

\examples{
 library("hgu95av2.db")
 bb <- hgu95av2GO[["39613_at"]]
 getEvidence(bb)
}
\keyword{manip}
