\name{readGEOAnn}
\alias{readGEOAnn}
\alias{readIDNAcc}
\alias{getGPLNames}
\alias{getSAGEFileInfo}
\alias{getSAGEGPL}
\alias{readUrl}
\title{Function to extract data from the GEO web site}
\description{
  Data files that are available at GEO web site are identified by GEO
  accession numbers. Given the url for the CGI script at GEO and
  a GEO accession number, the functions extract data from the web site
  and returns a matrix containing the data.
}
\usage{
readGEOAnn(GEOAccNum, url = "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?")
readIDNAcc(GEOAccNum, url = "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?")
getGPLNames(url ="http://www.ncbi.nlm.nih.gov/geo/query/browse.cgi?") 
getSAGEFileInfo(url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/browse.cgi?view=platforms&prtype=SAGE&dtype=SAGE")
getSAGEGPL(organism = "Homo sapiens", enzyme = c("NlaIII", "Sau3A"))
readUrl(url)
}
\arguments{
  \item{url}{\code{url} the url for the CGI script at GEO}
  \item{GEOAccNum}{\code{GEOAccNum} a character string for the GEO
    accession number of a desired file (e. g. GPL97)}
  \item{organism}{\code{organism} a character string for the name of the
    organism of interests}
  \item{enzyme}{\code{enzyme} a character string that can be eighter
    NlaII or Sau3A for the enzyme used to create SAGE tags}
}
\details{
  \code{url} is the CGI script that processes user's
  request. \code{\link{readGEOAnn}} invokes the CGI by passing a GEO 
  accession number and then processes the data file obtained.

  \code{\link{readIDNAcc}} calls \code{\link{readGEOAnn}} to read the
  data and the extracts the columns for probe ids and accession numbers.
  The \code{GEOAccNum} has to be the id for an Affymetrix chip.

  \code{\link{getGPLNames}} parses the html file that lists GEO
  accession numbers and descriptions of the array represented by the
  corresponding GEO accession numbers.  
}
\value{
  Both \code{\link{readGEOAnn}} and \code{\link{readIDNAcc}} return a
  matrix.

  \code{\link{getGPLNames}} returns a named vector of the names of
  commercial arrays. The names of the vector are the corresponding GEO
  accession number.
}
\references{\url{www.ncbi.nlm.nih.gov/geo}}
\author{Jianhua Zhang}

\examples{
# Get array names and GEO accession numbers
#geoAccNums <- getGPLNames()
# Read the annotation data file for HG-U133A which is GPL96 based on
# examining geoAccNums 
#temp <- readGEOAnn(GEOAccNum = "GPL96")
#temp2 <- readIDNAcc(GEOAccNum = "GPL96")
}
\keyword{manip}

