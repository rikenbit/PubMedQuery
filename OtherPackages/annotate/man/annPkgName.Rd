\name{annPkgName}
\alias{annPkgName}

\title{Get annotation package name from chip name}
\description{
  This function returns the name of the Bioconductor annotation data
  package that corresponds to the specified chip or genome.  The
  \code{type} argument is used to request an annotation package with a
  particular backing store.
}
\usage{
annPkgName(name, type = c("db", "env"))
}

\arguments{
  \item{name}{string specifying the name of the chip or genome.  For
      example, \code{"hgu133plus2"}}
  \item{type}{Either \code{"db"} or \code{"env"}.  This will determine
      whether the package name returned corresponds to the SQLite-based
      annotation package or environment-based package, respectively.}
}

\value{
  a string giving the name of the annotation data package
}

\author{Seth Falcon}
\seealso{
  \code{\link{getAnnMap}}
}
\examples{
annPkgName("hgu133plus2", type="db")
annPkgName("hgu133plus2", type="env")
}

\keyword{manip}

