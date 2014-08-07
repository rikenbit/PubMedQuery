\name{mapOrgs}
\alias{mapOrgs}
\alias{getOrgNameNCode}
\title{Functions to map to organism IDs used by NCBI homology.}
\description{
 These functions help map to organism identifiers used at the NCBI.
}
\usage{
mapOrgs(toMap, what = c("code","name"))
getOrgNameNCode()
}
\arguments{
  \item{toMap}{\code{vect} a vector of character strings}
  \item{what}{\code{what} a character string that can either be "code"
    or "name".}
}
\details{
  mapOrgs converts organism codes to scientific names.
}
\value{
  mapOrgs returns a vector of character strings.
}
\references{\url{ftp://ftp.ncbi.nih.gov/pub/HomoloGene/README}}
\author{Jianhua Zhang}

\keyword{manip}
