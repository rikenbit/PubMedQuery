\name{probesByLL}
\alias{probesByLL}

\title{A function that does reverse the mappings between probe ids and
  the corresponding values}
\description{  
  This  function takes the name of a platform specific annotation data
  package (e. g. "hgu95av2") and a character string for the name of an
  environment stored in the data directory and returns a list of vectors of
  character string with values of the environment as the names of the lists of
  vectors and probe ids corresponding to the values of the environment
  as values. 
}
\usage{
probesByLL(baseName, what = "ENTREZID")
}
\arguments{
  \item{baseName}{\code{baseName} a character string for the name of
    platform specific annotation data package}
  \item{what}{\code{what} a character string for the name of the
    environment object, which is the last part of an environment name
    excluding the package name}
}
\details{
  Each platform specific annotation data package has mappings between
  probe ids to other values. This function does
  reverse mappings by grouping probe ids under unique values of a given
  environmentand returns the mappings as a list.
  
  Values for the environment object for which the reverse mappings are
  sought must be vectors.
}
\value{
  This function returns a list of vectors of character strings with
  unique values of an environment as the names and probe ids
  corresponding to the values as values.
}
\author{Jianhua Zhang}
\examples{
  library("hgu95av2.db")
  tt <- probesByLL("hgu95av2", "ENTREZID")
  tt[1:4]
}
\keyword{manip}
