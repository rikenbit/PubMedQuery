\name{pubMedAbst-class}
\docType{class}
\alias{pubMedAbst-class}
\alias{pubMedAbst}
\alias{authors}
\alias{abstText}
\alias{articleTitle}
\alias{journal}
\alias{pubDate}
\alias{pmid}
\alias{pubMedAbst,pubMedAbst-method}
\alias{authors,pubMedAbst-method}
\alias{abstText,pubMedAbst-method}
\alias{articleTitle,pubMedAbst-method}
\alias{journal,pubMedAbst-method}
\alias{pubDate,pubMedAbst-method}
\alias{pmid,pubMedAbst-method}
\alias{show,pubMedAbst-method}  
\title{Class pubMedAbst, a class to handle PubMed abstracts, and methods
for processing them.}
\description{  This is a class representation for PubMed abstracts. }
\section{Creating Objects}{
  \code{ new('pubMedAbst',}\cr
  \code{  authors      = ...., # Object of class vector}\cr
  \code{  pmid         = ...., # Object of class character}\cr
  \code{  abstText     = ...., # Object of class character}\cr
  \code{  articleTitle = ...., # object of class character}\cr
  \code{  journal      = ...., # Object of class character}\cr
  \code{  pubDate      = ...., # Object of class character}\cr
  \code{  )}}
\section{Slots}{
  \describe{
    \item{\code{pmid}:}{Object of class \code{"character"} The PubMed ID
      for this paper. }
    \item{\code{authors}:}{Object of class \code{"vector"} The authors
      of the paper. }
    \item{\code{abstText}:}{Object of class \code{"character"} The
      contained text of the abstract.  }
    \item{\code{articleTitle}:}{Object of class \code{"character"} The
      title of the article the abstract pertains to. }
    \item{\code{journal}:}{Object of class \code{"character"} The journal
      the article was published in. }
    \item{\code{pubDate}:}{Object of class \code{"character"} The date the
    journal was published. }
  }
}

\section{Methods}{
  \describe{
    \item{pmid}{\code{signature(object = "pmid")}: An accessor function
      for \code{pmid}}
    \item{abstText}{\code{signature(object = "pubMedAbst")}: An accessor
    function for \code{abstText}}
    \item{articleTitle}{\code{signature(object = "pubMedAbst")}: An accessor
      function for \code{articleTitle} }
    \item{authors}{\code{signature(object = "pubMedAbst")}: An accessor
      function for \code{authors} }
    \item{journal}{\code{signature(object = "pubMedAbst")}: An accessor
      function for \code{journal} }
    \item{pubDate}{\code{signature(object = "pubMedAbst")}: An accessor
      function for \code{pubDate}}
  }
}
\author{ Jeff Gentry }

\seealso{\code{\link{pubmed}}, \code{\link{genbank}}}
\examples{
   x <- pubmed("9695952","8325638","8422497")
   a <- xmlRoot(x)
   numAbst <- length(xmlChildren(a))
   absts <- list()
   for (i in 1:numAbst) {
      absts[[i]] <- buildPubMedAbst(a[[i]])
   }
}
\keyword{classes}
