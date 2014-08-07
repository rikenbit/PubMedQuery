\name{homoData-class}
\docType{class}
\alias{homoData-class}
\alias{homoData}
\alias{homoLL}
\alias{homoOrg}
\alias{homoType}
\alias{homoURL}
\alias{homoACC}
\alias{homoHGID}
%\alias{show}
\alias{homoPS}
\alias{orgNameNCode}
\alias{homoPS,homoData-method}
\alias{homoLL,homoData-method}
\alias{homoOrg,homoData-method}
\alias{homoType,homoData-method}
\alias{homoURL,homoData-method}
\alias{homoACC,homoData-method}
\alias{homoHGID,homoData-method}
\alias{show,homoData-method}
\title{Class "homoData"}
\description{A class to present data for HomologGene data of a matching
  sequence} 
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("homoData", ...)}. 
}
\section{Slots}{
  \describe{
    \item{\code{homoOrg}:}{Object of class \code{"character"} the
      scientific name of the organism of interest}
    \item{\code{homoLL}:}{Object of class \code{"numeric"} the LocusLink
      id of the gene of interest}
    \item{\code{homoType}:}{Object of class \code{"character"} the type of
      similarity. Valid values include B - a recipiprocal best best
      between 3 or more organisms, b - a reciprocal best match, and c -
      a curated homology relationship} 
    \item{\code{homoPS}:}{Object of class \code{"numeric"} percent
      similarity value}
    \item{\code{homoURL}:}{Object of class \code{"character"} the URL for
      curated homology relationship}
    \item{\code{homoACC}:}{Object of class \code{"character"} the
      accession number}
    \item{\code{homoHGID}:}{Object of class \code{"numeric"} the
      internal HomologGeneID}
  }
}
\section{Methods}{
  \describe{
    \item{homoPS}{\code{signature(object = "homoData")}: the get function for
      slot \code{homoPS}}
    \item{homoLL}{\code{signature(object = "homoData")}: the get function
      for slot \code{homoLL}}
    \item{homoOrg}{\code{signature(object = "homoData")}: the get function
      for slot \code{homoOrg}}
    \item{homoType}{\code{signature(object = "homoData")}: the get function
      for slot \code{homoType}}
    \item{homoURL}{\code{signature(object = "homoData")}: the get function
      for slot \code{homoURL}}
    \item{homoACC}{\code{signature(object = "homoData")}: the get function
      for slot \code{homoACC}}
    \item{homoHGID}{\code{signature(object = "homoHGID")}: the get
      function for slot \code{homoHGID}}
  }
}
\references{\url{ftp://ftp.ncbi.nih.gov/pub/HomoloGene/README}}
\author{Jianhua Zhang}

\examples{
    new("homoData", homoPS = 82.3, homoLL = 2324853, homoOrg = "Homo sapins",
homoType = "B", homoURL = "", homoHGID = 12345)
}
\keyword{classes} 
