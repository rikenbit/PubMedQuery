\name{MeSHHyperGParams-class}
\docType{class}
\alias{MeSHHyperGParams-class}

\alias{initialize,MeSHHyperGParams-method}

\alias{geneIds,MeSHHyperGParams-method}
\alias{geneIds<-,MeSHHyperGParams,logical-method}
\alias{geneIds<-,MeSHHyperGParams,ANY-method}

\alias{universeGeneIds}
\alias{universeGeneIds<-}
\alias{universeGeneIds,MeSHHyperGParams-method}
\alias{universeGeneIds<-,MeSHHyperGParams-method}
\alias{universeGeneIds<-,MeSHHyperGParams,logical-method}
\alias{universeGeneIds<-,MeSHHyperGParams,ANY-method}

\alias{annotation,MeSHHyperGParams-method}
\alias{annotation<-,MeSHHyperGParams,character-method}

\alias{pvalueCutoff,MeSHHyperGParams-method}
\alias{pvalueCutoff<-,MeSHHyperGParams-method}

\alias{pAdjust}
\alias{pAdjust,MeSHHyperGParams-method}

\alias{makeValidParams,MeSHHyperGParams-method}

\title{Class "MeSHHyperGParams"}

\description{	
  A parameter class for representing all parameters needed for
  running the 'meshHyperGTest' method with one of the MeSH categories
  ("Anatomy", "Organisms", "Diseases", "Chemicals and Drugs", "Analytical, Diagnostic and Therapeutic Techniques and Equipment", "Psychiatry and Psychology", "Phenomena and Processes", "Disciplines and Occupations", "Anthropology, Education, Sociology and Social Phenomena", "Technology and Food and Beverages", "Humanities", "Information Science", "Persons", "Health Care", "Publication Type", "Geographical Locations").
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MeSHHyperGParams", ...)}.
}

\section{Slots}{
  \describe{
    \item{\code{geneIds}:}{Object of class \code{"ANY"}: A vector of
      gene identifiers.  Numeric and character vectors are probably the
      only things that make sense.  These are the gene ids for the
      selected gene set.}
    \item{\code{universeGeneIds}:}{Object of class \code{"ANY"}: A
      vector of gene ids in the same format as \code{geneIds} defining a
      subset of the gene ids on the chip that will be used as the
      universe for the hypergeometric calculation.}
    \item{\code{annotation}:}{A string giving the name of the
      gene-MeSH annotation package like org.MeSH.XXX.db.}
    \item{\code{category}:}{A string giving the name of the
      MeSH category like A, B, C, D, ...and so on.}
    \item{\code{database}:}{A string giving the name of the
      MeSH database like gendoo, gene2pubmed, ...and so on.}
    \item{\code{pvalueCutoff}:}{A numeric values between zero and one
      used as a p-value or FDR cutoff for hypergeometric test
      depending on \code{pAdjust}. The default is set to 0.05. }
    \item{\code{pAdjust}:}{A string which can be one of the
      Benjamini-Hochberg procedure (a.k.a. q-value) ("BH"), Q-value
      ("QV"), empirical Bayes method ("lFDR"), and unadjusted p-value
      ("none") for multiple testing correction. }
  }
}

\section{Methods}{
  \describe{

    \item{\code{geneIds(p)}, \code{geneIds(p) <- value}}{Accessor
      methods for the geneIds.}

    \item{\code{universeGeneIds(p)}, \code{universeGeneIds(p) <- value}}{Accessor methods for the geneIds.}
    
    \item{\code{annotation(p)}, \code{annotation(p) <- value}}{Accessor methods for the gene-MeSH annotation data.}

    \item{\code{pAdjust(p)}}{An accessor method for the choice of a method for multiple testing correction. }
  }
}

\author{
Gota Morota, Koki Tsuyuzaki, Takeru Nakazato, Itoshi Nikaido

Maintainer: Koki Tsuyuzaki <k.t.the-answer@hotmail.co.jp>
}

\seealso{
  \code{\link{meshr-package}},  \code{\link{MeSHHyperGResult-class}},
  \code{\link{meshHyperGTest}}, \code{\link{category}}, \code{\link{database}}
}

\keyword{classes}
