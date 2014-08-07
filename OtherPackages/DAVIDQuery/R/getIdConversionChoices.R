
#' Retrieve all possible values defining the type of the submitted ID list as well as
#' the type of conversion when using the ID conversion tool
#'
#' When the getIdConversionChoicesgets called the first time within the R session, 
#' it retrieves the set of annotation values from the DAVID web services, stores them
#' within the DAVIDTypeChoices data structure and then reuses it in subsequent calls.
#'
#' @name getIdConversionChoices
#' @param urlBase the DAVID main page url. Default is DAVIDURLBase.
#' @param curl RCurl handle. Default is getCurlHandle()
#' @param verbose if TRUE enables diagnostic messages
#' @return the list containing two data frames, 'to' and 'from', the first representing set of possible identifiers 
#' used when submitting the ID list, and the second representing set of possible identifiers the ID list can be converted to
#' @examples
#' \dontrun{
#' #retrieve the ID set for conversion 
#' idChoices<-getIdConversionChoices();
#' #display choice dialog
#' item<-menu(graphics = TRUE, title = "Select Identifier",  idChoices$from[,"name"]);
#' #retrieve identifier for subsequent conversion
#' ident<-idChoices$from[item,"value"];
#' }
#' @seealso {\code{\link{getAnnotationChoices}}, \code{\link{getAffyChipTypes}}, \code{\link{convertIDList}}, \code{\link{DAVIDQuery}}}
#' @export getAnnotationChoices
#' @author Roger Day, Alex Lisovich


getIdConversionChoices<-function(urlBase=DAVIDURLBase,curl=RCurl::getCurlHandle(),verbose=TRUE){
	if (is.null(DAVIDQuery::DAVIDTypeChoices)){

		if (verbose)
			cat("Retrieving ID conversion choices...\n");

		url=paste(urlBase,"conversion.jsp",sep="/");
		page<-RCurl::getURL(url,curl=curl);
	
		res<-list(from=getHTMLChoices(page,"name=\"Identifier\" >"),to=getHTMLChoices(page,"name=\"convertTo\""));

		#change DAVIDAnnotChoices in the DAVIDQuery namespace
		assignInNamespace("DAVIDTypeChoices",res,"DAVIDQuery");
	}
	return(DAVIDQuery::DAVIDTypeChoices);
}

