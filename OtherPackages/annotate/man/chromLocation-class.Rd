\name{chromLocation-class}
\docType{class}
\alias{chromLocation-class}
\alias{dataSource}
\alias{nChrom}
\alias{chromNames}
\alias{chromLocs}
\alias{chromLengths}
\alias{probesToChrom}
\alias{geneSymbols}
\alias{chromInfo}
\alias{organism,chromLocation-method}
\alias{dataSource,chromLocation-method}
\alias{nChrom,chromLocation-method}
\alias{chromNames,chromLocation-method}
\alias{chromLocs,chromLocation-method}
\alias{chromLengths,chromLocation-method}
\alias{probesToChrom,chromLocation-method}
\alias{geneSymbols,chromLocation-method}
\alias{chromInfo,chromLocation-method}
\alias{show,chromLocation-method}
\title{Class chromLocation, a class for describing genes and their
  chromosome mappings.}
\description{
  This class provides chromosomal information provided by a
  Bioconductor metadata package.  By creating the object once for a
  particular package, it can be used in a variety of locations without
  the need to recomputed values repeatedly. 
}
\section{Creating Objects}{
\code{  new('chromLocation',}
\code{    organism     = ...., # Object of class character}\cr
\code{    dataSource    = ...., # Object of class character}\cr
\code{    chromLocs    = ...., # Object of class list}\cr
\code{    probesToChrom  = ...., # Object of class ANY}\cr
\code{    chromInfo    = ...., # Object of class numeric}\cr
\code{    geneSymbols  = ...., # Object of class ANY}\cr
\code{  )}}
\section{Slots}{
  \describe{
    \item{\code{organism}:}{Object of class "character".  The organism
      that these genes correspond to.}
    \item{\code{dataSource}:}{Object of class "character".  The source of
    the gene data.}
    \item{\code{chromLocs}:}{Object of class "list".  A list which
      provides specific location information for every gene.}
    \item{\code{probesToChrom}:}{An object with an environment-like API
      which will translate a probe identifier to chromosome it belongs
      to.}
    \item{\code{chromInfo}:}{A numerical vector representing each
      chromosome, where the names are the names of the chromosomes and
      the values are their lengths}
    \item{\code{geneSymbols}:}{An environment or an object with
      environment-like API that maps a probe ID to
      the appropriate gene symbol}
  }
}
\section{Methods}{
  \describe{
    \item{chromLengths}{(chromLocation): Gets the lengths of the
      chromosome for this organism}
    \item{chromLocs}{(chromLocation): Gets the 'chromLocs' attribute.}
    \item{chromNames}{(chromLocation): Gets the name of the chromosomes
      for this organism}
    \item{dataSource}{(chromLocation): Gets the 'dataSource' attribute.}
    \item{probesToChrom}{(chromLocation): Gets the 'probesToChrom' attribute.}
    \item{nChrom}{(chromLocation): gets the number of chromosomes this
      organism has}
    \item{organism}{(chromLocation): gets the 'organism' attribute.}
    \item{chromInfo}{Gets the 'chromInfo' attribute.}
    \item{geneSymbols}{Gets the 'geneSymbols' attribute.}
  }
}
\seealso{\code{\link{buildChromLocation}}}
\examples{
  library("hgu95av2.db")

  z <- buildChromLocation("hgu95av2")
  
  ## find the number of chromosomes
  nChrom(z)

  ## Find the names of the chromosomes
  chromNames(z)

  ## get the organism this object refers to
  organism(z)

  ## get the lengths of the chromosomes in this object
  chromLengths(z)
}
\keyword{classes}
