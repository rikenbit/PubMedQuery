\name{getAffyChipTypes}
\alias{getAffyChipTypes}
\title{Retrieve all Affymetrix array type available from DAVID database.}
\usage{getAffyChipTypes(urlBase=DAVIDURLBase, curl=RCurl::getCurlHandle(),
    verbose=TRUE)}
\description{Retrieve all Affymetrix array type available from DAVID database.}
\details{When the getAffyChipTypes gets called the first time within the R session, 
it retrieves the set of annotation values from the DAVID web services, stores them
within the DAVIDAffyChipChoices data structure and then reuses it in subsequent calls.}
\value{data frame containing (name,value) pair columns
The first column is a decorated Affymetrix array name, i.e. the name which appears in the corresponding 
drop-down list on the DAVID web site.
The second column is an actual DAVID URL address of the file containing the Affymetrix probeset IDs}
\seealso{{\code{\link{getAnnotationChoices}}, \code{\link{getIdConversionChoices}}, \code{\link{getAffyProbesetList}}, \code{\link{DAVIDQuery}}}}
\author{Roger Day, Alex Lisovich}
\arguments{\item{urlBase}{the DAVID main page url. Default is DAVIDURLBase.}
\item{curl}{RCurl handle. Default is getCurlHandle()}
\item{verbose}{if TRUE enables diagnostic messages}}
\examples{\dontrun{
#retrieve the set of all possible Affymetrix array types 
chipTypes<-getAffyChipTypes();
#display choice dialog
item<-menu(graphics = TRUE, title = "Select Array Type",  chipTypes[,"name"]);
#retrieve array type for subsequent usage
ident<-chipTypes[item,"value"];
}}
