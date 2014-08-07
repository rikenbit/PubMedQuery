\name{PMIDAmat}
\alias{PMIDAmat}
\title{A function to compute the probe to PubMed id incidence matrix.}
\description{
  For a given chip or a given set of genes, it computes the mapping from
  probes to PubMed id.
}
\usage{
PMIDAmat(pkg, gene=NULL)
}
\arguments{
  \item{pkg}{The package name of the chip for which the incidence matrix should
    be computed.}
  \item{gene}{A character vector of interested probe set ids or NULL (default).}
}
\details{
  Not much to say, just find which probes are associated with which
  PubMed ids and return the incidence matrix, with PubMed ids as rows and probes as
  columns. 

  To specify a set of probes to use, let the argument \code{gene} to be
  a vector of probe ids.   Bt this way, the calculations are not
  involved with non-interested genes/PubMed ids so that the whole
  process could finish soon.  
  }
\value{
 A matrix containing zero or one, depending on whether the probe
  (column) is associated with a PubMed id (row).
}
\author{R. Gentleman}
\examples{
  library("hgu95av2.db")
  probe <- names(as.list(hgu95av2ACCNUM))
  Amat <- PMIDAmat("hgu95av2", gene=sample(probe, 10))
}
\keyword{manip}
