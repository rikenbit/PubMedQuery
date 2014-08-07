\name{MeSHHyperGResult-class}
\docType{class}
\alias{MeSHHyperGResult-class}

\alias{meshIds}
\alias{meshIds,MeSHHyperGResult-method}

\alias{meshTerms}
\alias{meshTerms,MeSHHyperGResult-method}

\alias{meshCategory}
\alias{meshCategory,MeSHHyperGResult-method}

\alias{meshAnnotation}
\alias{meshAnnotation,MeSHHyperGResult-method}

\alias{meshDatabase}
\alias{meshDatabase,MeSHHyperGResult-method}

\alias{pvalues,MeSHHyperGResult-method}

\alias{show,MeSHHyperGResult-method}

\alias{summary,MeSHHyperGResult-method}

\title{Class "MeSHHyperGResult"}

\description{	
  This class represents the results of a test for overrepresentation
  of MeSH terms among genes in a selected gene set based upon the
  Hypergeometric distribution.

  For details on extracting information from this object, please read
  the documentation in the \link[meshr]{MeSHHyperGParams-class}.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MeSHHyperGResult", ...)}.
}


\section{Slots}{
	 \describe{
    \item{\code{meshCategory}:}{Object of class \code{"character"} representing the category of MeSH terms tested.}
    \item{\code{meshAnnotation}:}{Object of class \code{"character"}.  The name of the annotation data used in the analysis.}
    \item{\code{meshDatabase}:}{Object of class \code{"character"}.  The name of the database used in the analysis.}
    \item{\code{meshIds}:}{Object of class \code{"character"}.  The
      character vector of the MeSH IDs identified  as significant
      from the hypergeometric test.}
    \item{\code{meshTerms}:}{Object of class \code{"character"}.  The
      character vector of the MeSH terms identified  as significant
      from the hypergeometric test.}
    \item{\code{pvalues}:}{Object of class \code{"numeric"}.
      Associated p-values of significantly enriched MeSH terms.}
  }
}


\section{Methods}{
  \describe{
    \item{meshCategory}{\code{signature(r = "MeSHHyperGResult")}: 
      Returns the MeSH category used in the analysis.}

    \item{meshAnnotation}{\code{signature(r = "MeSHHyperGResult")}: 
      Returns the name of the annotation data used in the analysis.}
    
    \item{meshDatabase}{\code{signature(r = "MeSHHyperGResult")}: 
      Returns the name of the database used in the analysis.}

    \item{meshIds}{\code{signature(r = "MeSHHyperGResult")}: 
      Returns the character vector of the MeSH IDs identified  as significant
      in the analysis. }

    \item{meshTerms}{\code{signature(r = "MeSHHyperGResult")}:
      Returns the character vector of the MeSH terms identified  as significant
      in the analysis. }
    
    \item{pvalues}{\code{signature(r = "MeSHHyperGResult")}: 
      Returns the associated p-values of significantly enriched MeSH terms.}
    
  \item{summary}{\code{signature(r = "MeSHHyperGResult")}: Returns a
      \code{data.frame} summarizing the test result.  Optional
      arguments \code{pvalue} and \code{categorySize} allow
      specification of maximum p-value and minimum categorySize,
      respectively.  Optional argument \code{htmlLinks} is a logical
      value indicating whether to add HTML links (useful in
      conjunction with xtables print method with \code{type} set to
      \code{"html"}).}

  \item{show}{\code{signature(object = "MeSHHyperGResult")}:
      Return a short description of the result.}
  }
}

\author{
Gota Morota, Koki Tsuyuzaki, Takeru Nakazato, Itoshi Nikaido

Maintainer: Koki Tsuyuzaki <k.t.the-answer@hotmail.co.jp>
}


\seealso{
  \code{\link{meshr-package}},  \code{\link{MeSHHyperGParams-class}},
  \code{\link{meshHyperGTest}}
}


\keyword{classes}
