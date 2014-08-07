\name{pubmed}
\alias{pubmed}

\title{A function to open the browser to Pubmed with the selected gene. }
\description{
  Given a vector of Pubmed identifiers or accession numbers, the user
  can either have a browser display a URL showing a Pubmed query for
  those identifiers, or a XMLdoc object with the same data.
}
\usage{
pubmed(...,disp=c("data","browser"), type=c("uid","accession"),
       pmaddress=.efetch("PubMed", disp, type))
}
\arguments{
  \item{...}{Vectorized set of Pubmed ID's}
  \item{disp}{Either "Data" or "Browser" (default is data).  Data
    returns a XMLDoc, while Browser will display information in the
    user's browser.}
  \item{type}{Denotes whether the arguments are accession numbers or
    UIDS.  Defaults to uids.}
  \item{pmaddress}{Specific path to the pubmed efetch engine from the
    NCBI website.}
}
\details{
  A simple function to retrieve Pubmed data given a specific ID, either
  through XML or through a web browser.  This function will accept
  either pubmed accession numbers or NCBI UIDs (defined as a Pubmed
  ID or a Medline ID) - although the types must not be mixed in a single
  call. 

  WARNING:  The powers that be at NCBI have been known to ban the IP
  addresses of users who abuse their servers (currently defined as
  less then 2 seconds between queries).  Do NOT put this function in
  a tight loop or you may find your access revoked.
}
\value{
  If the option "data" is used, an object of type XMLDoc is returned,
  unless there was an error with the query in which case an object of
  type try-error is returned.

  If the option "browser" is used, nothing is returned.
}

\author{R. Gentleman }

\seealso{\code{\link{genbank}}, \code{\link[XML]{xmlTreeParse}}}

\examples{
   if( interactive() )
    opts <- c("data","browser") else
    opts <- "data"
   for (dp in opts)
     pubmed("11780146","11886385","11884611",disp=dp)
}
\keyword{ interface }
