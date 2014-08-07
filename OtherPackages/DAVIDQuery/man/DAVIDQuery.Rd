\name{DAVIDQuery}
\alias{DAVIDQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ DAVIDQuery }
\description{
  Launch a query against DAVID, the Database for Annotation, Visualization and Integrated Discovery.  Return the results into an R object.
}
\usage{
DAVIDQuery(ids = "O00161,O75396", type = "UNIPROT_ACCESSION", annot, tool, URLlengthLimit = 2048,
details = TRUE, verbose = FALSE, writeHTML = FALSE, testMe = FALSE, graphicMenu = FALSE,  formatIt = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{ IDs for desired objects, as a character vector or as a single string with ids separated by ",".  To be passed to the DAVID website, the format has to be the latter.}
  \item{type}{ Type of input ids. If missing, a menu is contructed from the R object DAVIDTypeChoices.  See \url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html#input_list} for up-to-date choices.  }
  \item{annot}{ Type of annotation requested. If missing, a menu is contructed from the R object DAVIDAnnotChoices.  See \url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html#input_list} for up-to-date choices.  }
  \item{tool}{ Type of gene tool to use. If missing, a menu is contructed from the R object DAVIDToolChoices.  See \url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html#input_list} for up-to-date choices.  As of this writing, the tool choices corresponding to the Functional Annotation tools cannot be handled by this package.
}
  \item{URLlengthLimit}{ Published maximum length of the constructed URL. }
  \item{details}{ If TRUE (default), a list of intermediate results is returned;  otherwise, just the final query result. }
  \item{verbose}{ If TRUE (default is FALSE), more debugging information is printed. }
  \item{writeHTML}{ If TRUE (default is FALSE), write the received intermediate HTML to files. }
  \item{testMe}{ If TRUE (default is FALSE), assign default valuse and run. }
  \item{graphicMenu}{ If TRUE (default is FALSE), use a GUI window for the pick menus. }
  \item{formatIt}{ If TRUE (default), try to interpret the returned character table and structure the result. If false, the unadorned character table returned by DAVID.}
}
\details{
The API described at \url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html} is used.  The return is screen-scraped, a new URL is formulated and transmitted, again the return is screen-scraped to find the name of the results file, and finally that file is retrieved into a string matrix.  

Obviously this approach is brittle, but it has survived the 2008 DAVID update.
A real API would be better, of course.

The return value DAVIDQueryResult is just a character matrix.  Its content structure depends on the choices of tool and annotation arguments, so there has been no attempt to manipulate it into, say, a data frame with nice column names.

}

\value{

If detail==FALSE, only DAVIDQueryResult is returned.
This a character matrix holding the results of the tab-delimited file returned by DAVID.

If detail==TRUE, a list with contents useful for trouble-shooting:
	\item{firstURL}{}
	\item{firstStageResult}{}
	\item{DAVIDaction}{}
	\item{secondURL}{}
	\item{secondStageResult}{}
	\item{hasSessionEnded}{}
	\item{downloadFileName}{}
	\item{downloadURL}{}
	\item{DAVIDQueryResult}{}
}
\references{ 
\url{http://david.abcc.ncifcrf.gov}
\url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html}
Article:    DAVID gene ID conversion tool.  
Da Wei Huang, Brad T Sherman, Robert Stephens, Michael W Baseler, H Clifford Lane, and Richard A Lempicki
\url{http://www.pubmedcentral.nih.gov/articlerender.fcgi?tool=pubmed&pubmedid=18841237}

}

\author{ Roger Day }

\seealso{ \code{\link{DAVIDQueryLoop}}, \code{\link{formatDAVIDResult}} }
\examples{
result = DAVIDQuery(testMe=TRUE)
print(names(result))
print(result$firstURL)
print(result$secondURL)
print(names(result$DAVIDQueryResult))
print(names(result$DAVIDQueryResult$O00161))
print(result$DAVIDQueryResult$O00161$GENE_SYMBOL)  ### Uses UNIPROT ID's for input.
}
\keyword{ database }
