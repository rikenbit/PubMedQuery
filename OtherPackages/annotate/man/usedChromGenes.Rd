\name{usedChromGenes}
\alias{usedChromGenes}
\title{A function to select used genes on a chromosome from an ExpressionSet.}
\description{
  Given an instance of an \code{ExpressionSet}, a \code{chromLocation} object
  and the name of a chromosome this function returns all genes represented
  in the \code{ExpressionSet} on the specified chromosome.
}
\usage{
usedChromGenes(eSet, chrom, specChrom)
}
\arguments{
  \item{eSet}{An instance of an \code{ExpressionSet} object.}
  \item{chrom}{The name of the chromosome of interest.}
  \item{specChrom}{An instance of a \code{chromLocation} object.}
}
\value{
  Returns a vector of gene names that represent the genes from the
  \code{ExpressionSet} that are on the specified chromosome.
}
\author{ Jeff Gentry}
\examples{
    data(sample.ExpressionSet)
    data(hgu95AProbLocs)
    usedChromGenes(sample.ExpressionSet, "1", hgu95AProbLocs)
}
\keyword{interface}
