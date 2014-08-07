\name{convertIDList}
\alias{convertIDList}
\title{Retrieves conversions from one DAVID ID type in the input ID list to a different...}
\usage{convertIDList(idList, curl, fromType, toType, urlBase=DAVIDURLBase,
    testMe=FALSE, annotChoices=getIdConversionChoices(), details=FALSE,
    graphicMenu=FALSE, writeHTML=FALSE, verbose=FALSE)}
\description{Retrieves conversions from one DAVID ID type in the input ID list to a different  
DAVID ID type. The mappings are returned in the form of a set of unique pairs.}
\details{Due to the recent redesign of the DAVID online query system, it is no longer possible to use the DAVID API 
(as described in the documentation for \code{DAVIDQuery}) to perform gene ID conversion. For this reason, the gene ID
conversion is implemented as a separate function programmatically reproducing the Gene ID Conversion tool workflow as follows.
First, the list of IDs to be converted from the given ID type is submitted to the DAVID tools service using the HTTP message post.
Second, the DAVID check 'at least 80 percent of samples should be mapped' turned off by accessing the hidden URL "submitAnyway.jsp"
This ensures that the input ID list can contain any percentage of correct IDs and still be mapped properly.
Third, the request for ID convertion is sent by posting the HTTP message to the DAVID conversion service.
The resulting page is scrapped, the URL of the convertion result file is obtained and the file is retrieved. 
As the conversion results file is a well formatted table represented by a tab delimited .txt file,
no further formatting of the DAVIDQueryResult is needed.}
\value{A DAVIDQuery result. The informative part is a data frame consisting of columns 'From', 'To', 'Species' and 'Gene.Name'.
The 'From' column contains ID list submitted for conversion, the 'To' column contains the conversion results, 
while 'Species' and 'Gene.Name contain species and gene description, correspondingly.
The IDs for which conversion is not found (ambiguous IDs in DAVID terms) are not returned. 
The missing IDs can be found comparing the input ID set and the unique ID set in the 'From' column.
If no conversion at all is found the function returns NULL.}
\seealso{{\code{\link{DAVIDQuery}}, \code{\link{getIdConversionChoices}}}}
\author{Roger Day, Alex Lisovich}
\arguments{\item{idList}{The character vector of IDs to be converted.}
\item{curl}{RCurl handle.}
\item{fromType}{The type of input IDs. If NULL (default), determined through the popup menu.}
\item{toType}{The type to convert to. If NULL (default), determined through the popup menu.}
\item{urlBase}{The DAVID main page url. Default is DAVIDURLBase.}
\item{testMe}{If TRUE, assign default values and run. Dafault is FALSE.}
\item{annotChoices}{All available 'From' and 'To' ID conversion types.}
\item{details}{If TRUE, a list of intermediate results is returned; otherwise, just the final query result. Default is TRUE.}
\item{graphicMenu}{If TRUE, use a GUI window for the pick menus. Default is FALSE.}
\item{writeHTML}{If TRUE writes the conversion result html page into the 'conversionResult.html' file. Default is FALSE.}
\item{verbose}{If TRUE enables diagnostic messages.}}
\examples{\dontrun{
idList=c("P04264", "P13645");
data<-convertIDList(idList,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",writeHTML=TRUE,verbose=TRUE);
}}
