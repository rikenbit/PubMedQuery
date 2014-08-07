\name{getAnnotationChoices}
\alias{getAnnotationChoices}
\title{Retrieve all possible annotation values used in the annotation report tool...}
\usage{getAnnotationChoices(urlBase=DAVIDURLBase, curl=RCurl::getCurlHandle(),
    verbose=TRUE)}
\description{Retrieve all possible annotation values used in the annotation report tool}
\details{When the getAnnotationChoices gets called the first time within the R session, 
it retrieves the set of annotation values from the DAVID web services, stores them
within the DAVIDAnnotChoices data structure and then reuses it in subsequent calls.}
\value{the list  of possible annotation tags, i.e. GOTERM_MF_4, GOTERM_MF_5, BLOCKS_ID etc. 
used with the annotationReport tool.}
\seealso{{\code{\link{getIdConversionChoices}}, \code{\link{getAffyChipTypes}}, \code{\link{convertIDList}}, \code{\link{DAVIDQuery}}}}
\author{Roger Day, Alex Lisovich}
\arguments{\item{urlBase}{the DAVID main page url. Default is DAVIDURLBase.}
\item{curl}{RCurl handle. Default is getCurlHandle()}
\item{verbose}{if TRUE enables diagnostic messages}}
\examples{\dontrun{
#retrieve annotation values
annotChoices<-getAnnotationChoices();
#display choice dialog
item<-menu(graphics = TRUE, title = "Select Identifier",  annotChoices$from[,"name"]);
#retrieve identifier for subsequent conversion
ident<-annotChoices$from[item,"value"];
}}
