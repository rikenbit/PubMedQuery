\name{updateSymbolsToValidKeys}
\alias{updateSymbolsToValidKeys}
\title{Take a list of symbols and translate them into the best possible
  ID for a package.}
\description{
  Given a list of gene symbols and a package, find a valid ID for that
  package.  If there isn't a valid ID, then return the original symbol.
}
\usage{
updateSymbolsToValidKeys(symbols, pkg)
}
\arguments{
  \item{symbols}{A character vector containing gene symbols that you
    wish to try and translate into valid IDs.}
  \item{pkg}{The package name of the chip for which we wish to validate IDs.}
}
\details{
  This is a convenience function for getting from a possibly varied list
  of gene symbols mapped onto something that is a nice concrete ID such as an
  entrez gene ID.  When such an ID cannot be found, the original symbol
  will come back to prevent the loss of any information.
}
\value{
  This function returns a vector of IDs corresponding to the symbols
  that were input.  If the symbols don't have a valid ID, then they come
  back instead.
}
\author{Marc Carlson}
\seealso{\code{\link{isValidKey}}}
\examples{
\dontrun{
  ## one "bad" ID, one that can be mapped onto a valid ID, and a 3rd
  ## which already is a valid ID
  syms <- c("15S_rRNA_2","21S_rRNA_4","15S_rRNA")
  updateSymbolsToValidKeys(syms, "org.Sc.sgd")

  ## 3 symbols and a 4th that will NOT be valid
  syms <- c("MAPK11","P38B","FLJ45465", "altSymbol")
  updateSymbolsToValidKeys(syms, "org.Hs.eg")
}
}
\keyword{manip}
