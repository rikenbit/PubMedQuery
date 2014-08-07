\name{p2LL}
\alias{p2LL}
\title{A function to map from probes to unique Entrez Gene IDs}
\description{
  For any chip, this function computes the map from  unique Entrez Gene
  ID to all probes.
}
\usage{
p2LL(data)
}
\arguments{
  \item{data}{ The character string naming the chip.}
}
\details{
  This function is deprecated.

  This is essentially the computation of the reverse map, we store probe
  to Entrez gene information in the \code{ENTREZID} environment. This is
  used to compute the inverse mapping.
}
\value{
  A list, with length equal to the number of unique Entrez Gene IDs on
  the chip, the elements correspond to the probes that map to the Gene ID.
}
\author{R. Gentleman}
\seealso{\code{\link{getEG}}}
\examples{
\dontrun{
  library("hgu95av2.db")
  x <- p2LL("hgu95av2")
  table(sapply(x, length))
}
}
\keyword{manip}
