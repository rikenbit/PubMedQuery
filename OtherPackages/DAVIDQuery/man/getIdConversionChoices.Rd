\name{getIdConversionChoices}
\alias{getIdConversionChoices}
\title{Retrieve all possible values defining the type of the submitted ID list as well as...}
\usage{getIdConversionChoices(urlBase=DAVIDURLBase,
    curl=RCurl::getCurlHandle(), verbose=TRUE)}
\description{Retrieve all possible values defining the type of the submitted ID list as well as
the type of conversion when using the ID conversion tool}
\details{When the getIdConversionChoicesgets called the first time within the R session, 
it retrieves the set of annotation values from the DAVID web services, stores them
within the DAVIDTypeChoices data structure and then reuses it in subsequent calls.}
\value{the list containing two data frames, 'to' and 'from', the first representing set of possible identifiers 
used when submitting the ID list, and the second representing set of possible identifiers the ID list can be converted to}
\seealso{{\code{\link{getAnnotationChoices}}, \code{\link{getAffyChipTypes}}, \code{\link{convertIDList}}, \code{\link{DAVIDQuery}}}}
\author{Roger Day, Alex Lisovich}
\arguments{\item{urlBase}{the DAVID main page url. Default is DAVIDURLBase.}
\item{curl}{RCurl handle. Default is getCurlHandle()}
\item{verbose}{if TRUE enables diagnostic messages}}
\examples{\dontrun{
#retrieve the ID set for conversion 
idChoices<-getIdConversionChoices();
#display choice dialog
item<-menu(graphics = TRUE, title = "Select Identifier",  idChoices$from[,"name"]);
#retrieve identifier for subsequent conversion
ident<-idChoices$from[item,"value"];
}}
