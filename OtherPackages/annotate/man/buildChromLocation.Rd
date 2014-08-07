\name{buildChromLocation}
\alias{buildChromLocation}
\title{A function to generate an instantiation of a chromLocation class}
\description{
  This function will take the name of a data package and build a
  chromLocation object representing that data set.
}
\usage{
  buildChromLocation(dataPkg)
}
\arguments{
  \item{dataPkg}{The name of the data package to be used}
}
\details{
  The requested data set must be available in the user's
  \code{.libPaths()}, and the function will throw an error if this is
  not the case.

  If the data package is present, the necessary information will be
  extracted from the data package and a \code{chromLocation} object will
  be created.
}
\value{
  A \code{chromLocation} object representing the specified data set.
}
\author{Jeff Gentry}
\examples{
  library("hgu95av2.db")
  z <- buildChromLocation("hgu95av2")
}
\keyword{utilities}
