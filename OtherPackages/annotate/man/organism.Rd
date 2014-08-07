\name{organism}
\alias{organism}
\alias{organism,character-method}
\title{Convenience function for getting the organism from an object or package}

\description{
  The most basic organism method just takes a character string (which
  represents a particular annotation package) and returns the organism
  that said package is based upon.
}

\usage{
  organism(object)
}

\arguments{
  \item{object}{a character string that names a package}
}

\value{
  The name of the organism used for this package or object
}
  

\author{Marc Carlson}

\examples{
  require(hgu95av2.db)
  ## get the organism for this annotation package
  organism("hgu95av2")

  ## get the organism this object refers to
  ## (for a ChromLocation object)
  z <- buildChromLocation("hgu95av2")
  organism(z)

}

\keyword{manip}

