\name{pmAbst2HTML}
\alias{pmAbst2HTML}
\title{HTML Generation for PubMed Abstracts}
\description{
  This function will take a \code{pubMedAbst} object, or a list of these
  objects and generate a web page that will list the titles of
  the abstracts and link to their full page on PubMed
}
\usage{
pmAbst2HTML(absts, filename, title, frames = FALSE, table.center = TRUE)
}
\arguments{
  \item{absts}{A list of \code{pubMedAbst} (or a single object)}
  \item{filename}{The output filename.  If \code{frames} is
    \code{FALSE}, this is the name of the single output file and
    defaults to \code{absts.html}.  Otherwise, this is taken to be the
    base of a set of filenames, and the default base is the empty
    string.  See \code{value} for more information on output files.}
  \item{title}{ Extra title information for your listing}
  \item{frames}{If \code{frames} is \code{TRUE}, the resulting page will
  use HTML frames, resulting in a more complex set of output pages.}
  \item{table.center}{If TRUE, will center the listing of abstracts}
}
\details{
  This function uses the \code{Entrez} functionality provided by NCBI to
  retrieve the abstract URL at the PubMed site.  It will then create a
  tabular webpage which will list the titles of the abstracts provided
  and have them link to the appropriate PubMed page.  If \code{frames}
  is \code{TRUE}, the table of links will be on the left hand side of
  the page and the right hand will link directly to the appropriate
  PubMed page.
}
\value{
  If \code{frames} is \code{FALSE}, a simple HTML file is created with
  the name specified by \code{filename}.

  If \code{frames} is \code{TRUE}, then there are four HTML files
  created, of the form \code{XXXtop.html}, \code{XXXside.html},
  \code{XXXmain.html} and \code{XXXindex.html}, where \code{XXX} is the
  string provided by \code{filename}.
}
\author{Jeff Gentry}

\seealso{\code{pubMedAbst}}
\examples{
        x <- pubmed("9695952","8325638","8422497")
        a <- xmlRoot(x)
        numAbst <- length(xmlChildren(a))
        absts <- list()
        for (i in 1:numAbst) {
           absts[[i]] <- buildPubMedAbst(a[[i]])
        }
        ## First try it w/o frames - using a temporary
        ## file for the output
        fname <- tempfile()
        pmAbst2HTML(absts,filename=fname)

        if (interactive())
          browseURL(paste("file://",fname,sep=""))

        ## Now try it w/ frames, using temporary files again.
        fnameBase <- tempfile()
        pmAbst2HTML(absts,filename=fnameBase, frames=TRUE)

        if (interactive())
          browseURL(paste("file://",fnameBase,"index.html",sep=""))

}
\keyword{utilities}
