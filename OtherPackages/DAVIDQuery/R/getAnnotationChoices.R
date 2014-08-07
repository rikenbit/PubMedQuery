
#' Retrieve all possible annotation values used in the annotation report tool
#'
#' When the getAnnotationChoices gets called the first time within the R session, 
#' it retrieves the set of annotation values from the DAVID web services, stores them
#' within the DAVIDAnnotChoices data structure and then reuses it in subsequent calls.
#'
#' @name getAnnotationChoices
#' @param urlBase the DAVID main page url. Default is DAVIDURLBase.
#' @param curl RCurl handle. Default is getCurlHandle()
#' @param verbose if TRUE enables diagnostic messages
#' @return the list  of possible annotation tags, i.e. GOTERM_MF_4, GOTERM_MF_5, BLOCKS_ID etc. 
#' used with the annotationReport tool.
#' @examples
#' \dontrun{
#' #retrieve annotation values
#' annotChoices<-getAnnotationChoices();
#' #display choice dialog
#' item<-menu(graphics = TRUE, title = "Select Identifier",  annotChoices$from[,"name"]);
#' #retrieve identifier for subsequent conversion
#' ident<-annotChoices$from[item,"value"];
#' }
#' @seealso {\code{\link{getIdConversionChoices}}, \code{\link{getAffyChipTypes}}, \code{\link{convertIDList}}, \code{\link{DAVIDQuery}}}
#' @export getAnnotationReportChoices
#' @author Roger Day, Alex Lisovich


getAnnotationChoices<-function(urlBase=DAVIDURLBase,curl=RCurl::getCurlHandle(),verbose=TRUE){
	if (is.null(DAVIDQuery::DAVIDAnnotChoices)){
		if (verbose)
			cat("Retrieving annotation report choices...\n");

		url=paste(urlBase,"content.jsp?file=DAVID_API.html",sep="");
		page<-RCurl::getURL(url,curl=curl);

		selection<-findInBrackets(page, "\"annot\" tag:","</html>")[1];
		selection<-removeHTMLComments(selection);

		choices<-findInBrackets(selection,">","</td>");
		empty_inds<- grep("\n",choices,fixed=TRUE)
		res<-choices[!(c(1:length(choices)) %in% empty_inds)];

		#get rid of "Category" item
		res<-res[-1];

		#change DAVIDAnnotChoices in the DAVIDQuery namespace
		assignInNamespace("DAVIDAnnotChoices",res,"DAVIDQuery");
	}
	return(DAVIDQuery::DAVIDAnnotChoices);
}
