\name{requestIDConversion}
\alias{requestIDConversion}
\title{Request a conversion for a previously uploaded ID List...}
\usage{requestIDConversion(url, curl, type="AFFYMETRIX_3PRIME_IVT_ID",
    verbose=TRUE)}
\description{Request a conversion for a previously uploaded ID List}
\value{A character string containing the source for a conversion results html page
including the url to the conversion results text file.}
\keyword{internal}
\author{Roger Day, Alex Lisovich}
\arguments{\item{url}{The url to post a request to.}
\item{curl}{The RCurl handle.}
\item{type}{The DAVID ID Identifier to convert to.}
\item{verbose}{If TRUE enables diagnostic messages.}}
\examples{\dontrun{
myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle,verbose = TRUE);
url1="http://david.abcc.ncifcrf.gov/tools.jsp";
idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
res2<-uploadIDList(url1,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);
url2<-"http://david.abcc.ncifcrf.gov/conversion2.jsp";
res3<-requestIDConversion(url=url2,curl=myCurlHandle,type="AFFYMETRIX_3PRIME_IVT_ID",verbose=TRUE);
writeChar(res3, "thirdStageResult.html")
}}
