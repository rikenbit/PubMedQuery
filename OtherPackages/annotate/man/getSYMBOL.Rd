\name{getSYMBOL}
\alias{getSYMBOL}
\alias{getGO}
\alias{getGOdesc}
\alias{getPMID}
\alias{getLL}
\alias{getEG}
\alias{lookUp}
\alias{getUniqAnnItem}
\title{Functions to deal with Data Packages}
\description{
  The functions documented here are intended to make it easier to map
  from a set of manufacturers identifiers (such as you will get from the
  chips etc) to other identifiers.
}
\usage{
getSYMBOL(x, data)
getLL(x, data)
getEG(x, data)
getGO(x, data)
getPMID(x, data)
getGOdesc(x, which)
lookUp(x, data, what, load = FALSE)
getUniqAnnItem()
}
\arguments{
  \item{x}{The identifiers to be mapped (usually manufacturer)}
  \item{data}{The basename of the meta-data package to be used.}
  \item{what}{\code{what} a character string for the name of an
    annotation element of an annotation data package}
  \item{which}{\code{which} a character string in the form of MF, BP,
    CC, or ANY to indicated the GO categories of interest}
  \item{load}{A logical value indicating whether to attempt to load the
  required annotation data package if it isn't already loaded.}
}
\details{
  Users must supply the basename of the meta-data package that they
  want to use to provide the mappings. The name of the meta-data
  package is the same as the basename.
  
  Appropriate translations are done. In some cases such as \code{getEG}
  and \code{getSYMBOL} there will only be one match and a vector is
  returned. In other cases such as \code{getPMID} and \code{getGO} there
  may be multiple matches and a list is returned.

  For \code{getGOdesc} \code{x} contains GO identifiers (not
  manufacturer identifiers) and the output is a list of GOTerms objects,
  if \code{which} specifies some subset of the ontologies (MF, BP or CC) 
  then only terms for that ontology are retained.

  \code{lookUp} is a general function that can be used to look
  up matches. All other translation functions use \code{lookUp}

  A BioC annotation data package contains annotation data environments
  whose names are package name (e. g. hgu95av2) + element name
  (e. g. PMID). \code{what} must be one of the element names for the
  given data package. 

  \code{getUniqAnnItem} keeps track of the annotation elements that have
  one to one mappings.   
}
\value{
  Either a vector or a list depending on whether multiple values per
  input are possible.
}
\author{R. Gentleman}
\seealso{\code{\link{mget}}}
\examples{
  library("hgu95av2.db")
  library("GO.db")

  data(sample.ExpressionSet)
  gN <- featureNames(sample.ExpressionSet)[100:105]
  lookUp(gN, "hgu95av2", "SYMBOL")

  # Same as lookUp for SYMBOL except the return is a vector
  getSYMBOL(gN,"hgu95av2" )
  gg <- getGO(gN, "hgu95av2")
  lookUp(gg[[2]][[1]][["GOID"]], "GO", "TERM")

  # Same as lookUp for TERM
  getGOdesc(gg[[2]][[1]][["GOID"]], "ANY")

  # For BP only
  getGOdesc(gg[[2]][[1]][["GOID"]], "BP")
  getEG(gN, "hgu95av2")
  getPMID(gN, "hgu95av2")
}
\keyword{manip}
