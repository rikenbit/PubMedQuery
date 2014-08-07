\name{ACCNUMStats}
\alias{ACCNUMStats}
\alias{whatACC}

\title{Provides statistics on the types of ids used for the ACCNUM
  environment of a given data package}
\description{
  Given a data package name, ACCNUMStats counts how many of the probe
  ids are mapped to GenBank Accession numbers, UniGene ids, RefSeq ids,
  or Image clone ids.
}
\usage{
ACCNUMStats(pkgName)
whatACC(accs)
}
\arguments{
  \item{pkgName}{\code{pkgName} a character string for the name of a
    BioC data package}
  \item{accs}{\code{accs} a vector of character string for the ids
    whose type will be determined}
}
\details{
  The ACCNUM environment of each BioC data package contains mappings
  between probe ids and a set of public ids based on which mappings of
  probe ids to other annotation data can be obtained using public data
  sources. The set of ids were provided by a manufacturer or user at the
  time when the data package was built. The manufacturer/user provided
  ids can be of different types of public ids, such as GenBank Accession
  number, UniGene ids, etc..

  ACCNUMStats counts the number of probes that are mapped to different
  types of public ids and have the results presented in a table.
}
\references{The ACCNUM environment of a platform dependent BioC data package}
\author{Jianhua Zhang}
\examples{
  library("hgu95av2.db")
  ACCNUMStats("hgu95av2")
}
\keyword{misc}
