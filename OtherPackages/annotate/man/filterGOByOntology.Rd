\name{filterGOByOntology}
\alias{filterGOByOntology}

\title{Filter GO terms by a specified GO ontology}
\description{
  Given a character vector containing GO identifiers, return a logical
  vector indicating which GO IDs are in the specified ontology (BP,
  CC, or MF).
}
\usage{
filterGOByOntology(goids, ontology = c("BP", "CC", "MF"))
}

\arguments{
  \item{goids}{a character vector of GO IDs}
  \item{ontology}{One of "BP", "CC", or "MF"}
}
\value{
A logical vector with length equal to \code{goids}.  A \code{TRUE}
indicates that the corresponding GO ID in \code{goids} is a member
of the ontology specified by \code{ontology}.
}
\author{Seth Falcon}
\examples{
haveGO <- suppressWarnings(require("GO.db"))
if (haveGO) {
    ids <- c("GO:0001838", "GO:0001839")
    stopifnot(all(filterGOByOntology(ids, "BP")))
    stopifnot(!any(filterGOByOntology(ids, "MF")))
} else cat("Sorry, this example requires the GO package\n")

}

\keyword{manip}
