\name{formatDAVIDResult}
\alias{formatDAVIDResult}
\alias{formatAnnotationReport}
\alias{formatGeneReport}
\alias{formatGeneReportFull}
\alias{formatList}
\alias{formatGene2Gene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Format the character table returned by DAVID. }
\description{
These functions attempt to format the character table \code{result} returned by DAVID.  
}
\usage{
formatDAVIDResult(result, verbose=FALSE)
formatAnnotationReport(result)
formatGeneReport(result)
formatGeneReportFull(result)
formatList(result)
formatGene2Gene(result)
}
\arguments{
  \item{result}{ Character table returned by DAVID. }
  \item{verbose}{ If TRUE, print tool. Warn if tool=="geneReportFull" that the result will be returned invisibly due to its size. }
}
\details{
\code{formatDAVIDResult} switches out to one of \code{formatGeneReport},  \code{formatGeneReportFull}, \code{formatGene2Gene}, or \code{formatList}, depending on the \code{tool} argument of \code{DAVIDQuery()}  used to specify what query report to do.  The \code{tool} argument is passed as an attribute attached to \code{result}.

WARNINGS:
Not all values of \code{tool} have an associated format.

These format utilities are not guaranteed to work correctly for all combinations of inputs into \code{DAVIDQuery()}, or to continue to work correctly if or when the DAVID API changes.  If results appear incorrect, one can use the option \code{DAVIDQuery(formatIt=FALSE)} to see the unformatted output, and/or paste \code{DAVIDQuery(details=TRUE)[firstURL]} into a browser. 

In the case of formatGene2Gene, the \code{gene} column of the \code{details} component might not always contain a single identifier. 
}
\value{
For \code{tool=="geneAnnotationReport"}, a list, one component for each element in the \code{ids} arg. Each component has subcomponents 
  \item{Gene Name }{Self-explanatory.}
  \item{Species }{Self-explanatory.}
  \item{<id> }{The identifier(s) in the query. The name is whatever the id type was.}
  \item{<other items> }{Items produced for the input, specified by the \code{annot} arg of the query.   }

For \code{tool=="geneReport"} or  \code{tool=="list"}, a character matrix with column names scraped from DAVIDQueryResult, usually:
  \item{<gene name> }{Using the same ID type as the \code{type} argument of \code{DAVIDQuery()}.}
  \item{Gene Name }{Self-explanatory.}
  \item{Species }{Self-explanatory.}
In addition, for \code{tool=="geneReport"}, the first line of the returned output is saved as an attribute before discarding it.
 
For \code{tool=="geneReportFull"}, a list, one component for each element in the \code{ids} arg. Each component has subcomponents 
  \item{Gene Name }{Self-explanatory.}
  \item{Species }{Self-explanatory.}
  \item{<other items> }{The union of items produced for the input identifiers (generically called "genes" in DAVID).  (The set of attributes is not fixed.) }

For \code{tool=="formatGene2Gene"}, a list with one component for each Functional Group. Each component has components 
  \item{median }{See DAVID documentation.}
  \item{geo }{See DAVID documentation.}
  \item{diagram }{An attempt to parse the fourth column of the Functional Group line of the input.  See DAVID documentation and consult the DAVID team.}
  \item{details }{A data frame with columns 
    \describe{
	\item{gene}{"gene" as identified in the \code{ids} arg to \code{DAVIDQuery}}
	\item{geneName}{gene name}
	\item{url}{The URL for the Gene Report page at NIAID.}
    }
  }
As of this writing, the tool choices corresponding to most Functional Annotation tools cannot be handled by this package.
}
\keyword{ database }
