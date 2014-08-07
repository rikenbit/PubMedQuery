\name{genbank}
\alias{genbank}
\title{A function to open the browser to Genbank with the selected gene. }
\description{
  Given a vector of Genbank accession numbers or NCBI UIDs, the user can
  either have a browser display a URL showing a Genbank query for those
  identifiers, or a XMLdoc object with the same data.  
}
\usage{
genbank(...,disp=c("data","browser"), type=c("accession","uid"),
        pmaddress=.efetch("gene", disp, type))
}
\arguments{
  \item{...}{Vectorized set of Genbank accession numbers or NCBI UIDs}
  \item{disp}{Either "Data" or "Browser" (default is data).  Data
    returns a XMLDoc, while Browser will display information in the
    user's browser.}
  \item{type}{Denotes whether the arguments are accession numbers or
    UIDS.  Defaults to accession values.}
  \item{pmaddress}{Specific path to the pubmed efetch engine from the
    NCBI website.}

}
\details{
  A simple function to retrieve Genbank data given a specific ID, either
  through XML or through a web browser.  This function will accept
  either Genbank accession numbers or NCBI UIDs (defined as a Pubmed
  ID or a Medline ID) - although the types must not be mixed in a single
  call. 

  WARNING:  The powers that be at NCBI have been known to ban the IP
  addresses of users who abuse their servers (currently defined as less
  then 2 seconds between queries).  Do NOT put this function in a tight
  loop or you may find your access revoked.
}
\value{
  If the option "data" is used, an object of type XMLDoc is returned,
  unless there was an error with the query in which case an object of
  type try-error is returned.

  If the option "browser" is used, nothing is returned.
}

\author{R. Gentleman }

\seealso{\code{\link{pubmed}}, \code{\link[XML]{xmlTreeParse}}}

\examples{
   ## Use UIDs to get data in both browser & data forms

   if ( interactive() ) {
      disp <- c("data","browser")
   } else {
      disp <- "data"
   }

   for (dp in disp)
     genbank("12345","9997",disp=dp,type="uid")

   ## Use accession numbers to retrieve browser info
   if ( interactive() )
       genbank("U03397","AF030427",disp="browser")
}
\keyword{interface }
