#' Retrieve all Affymetrix array type available from DAVID database.
#'
#' When the getAffyChipTypes gets called the first time within the R session, 
#' it retrieves the set of annotation values from the DAVID web services, stores them
#' within the DAVIDAffyChipChoices data structure and then reuses it in subsequent calls.
#'
#' @name getAffyChipTypes
#' @param urlBase the DAVID main page url. Default is DAVIDURLBase.
#' @param curl RCurl handle. Default is getCurlHandle()
#' @param verbose if TRUE enables diagnostic messages
#' @return data frame containing (name,value) pair columns
#' The first column is a decorated Affymetrix array name, i.e. the name which appears in the corresponding 
#' drop-down list on the DAVID web site.
#' The second column is an actual DAVID URL address of the file containing the Affymetrix probeset IDs
#' @examples
#' \dontrun{
#' #retrieve the set of all possible Affymetrix array types 
#' chipTypes<-getAffyChipTypes();
#' #display choice dialog
#' item<-menu(graphics = TRUE, title = "Select Array Type",  chipTypes[,"name"]);
#' #retrieve array type for subsequent usage
#' ident<-chipTypes[item,"value"];
#' }
#' @seealso {\code{\link{getAnnotationChoices}}, \code{\link{getIdConversionChoices}}, \code{\link{getAffyProbesetList}}, \code{\link{DAVIDQuery}}}
#' @export getAffyChipTypes
#' @author Roger Day, Alex Lisovich


getAffyChipTypes<-function(urlBase=DAVIDURLBase,curl=RCurl::getCurlHandle(),verbose=TRUE){
	if (is.null(DAVIDQuery::DAVIDAffyChipChoices)){
		if (verbose)
			cat("Retrieving Affymetrix array types...\n");

		url=paste(urlBase,"ease/update/EASE_Files.jsp",sep="");
		page<-RCurl::getURL(url,curl=curl);

		chipTypes<-getHTMLChoices(page,"Select an Affymetrix Population file");
		names<-chipTypes[,"name"];
		names<-gsub(".txt","",names,fixed=TRUE);
		names<-gsub(" Array","",names,fixed=TRUE)
		chipTypes[,"name"]<-names;

		#change DAVIDAffyChipChoicesin the DAVIDQuery namespace
		assignInNamespace("DAVIDAffyChipChoices",chipTypes,"DAVIDQuery");
	}
	return(DAVIDQuery::DAVIDAffyChipChoices);	
}

