\name{uploadIDList}
\alias{uploadIDList}
\title{Upload an ID list to the DAVID query system...}
\usage{uploadIDList(url, curl, idList, type="UNIPROT_ACCESSION", verbose=TRUE)}
\description{Upload an ID list to the DAVID query system}
\value{A character string containing the source for a list upload html page.}
\keyword{internal}
\author{Roger Day, Alex Lisovich}
\arguments{\item{url}{The url to post a request to.}
\item{curl}{The RCurl handle.}
\item{idList}{The list of IDs to upload.}
\item{type}{The DAVID Identifier to convert from.}
\item{sessionID}{The RCurl session ID to be submitted as part of ID list uploading process.
If NULL the session ID is retrieved from curl handle.}
\item{verbose}{If TRUE, enables diagnostic messages.}}
\examples{\dontrun{ 
myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle);
url="http://david.abcc.ncifcrf.gov/summary.jsp";
idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
res2<-uploadIDList(url,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);
}}
