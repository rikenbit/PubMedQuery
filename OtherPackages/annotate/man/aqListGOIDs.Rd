\name{aqListGOIDs}
\alias{aqListGOIDs}

\title{List GO Identifiers by GO Ontology}
\description{
  This function returns a character vector of all GO identifiers in the
  specified ontologies: Biological Process (BP), Cellular Component
  (CC), Molecular Function (MF).
}
\usage{
aqListGOIDs(ont)
}

\arguments{
  \item{ont}{A character vector specifying the two-letter codes of the
    ontologies from which all GO IDs will be retrieved.  Entries must be
  one of \code{"BP"}, \code{"CC"}, or \code{"MF"}.}
}

\value{
  A character vector of GO IDs.  The vector will contain all GO IDs in
  the GO ontologies specified by the \code{ont} argument.
}

\author{Seth Falcon}
\examples{
## all GO IDs in BP
bp_ids = aqListGOIDs("BP")
length(bp_ids)

## all GO IDs in BP or CC
bp_or_cc_ids = aqListGOIDs(c("BP", "CC"))
length(bp_or_cc_ids)
}

\keyword{manip}

