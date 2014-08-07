
#' Retrieves conversions from one DAVID ID type in the input ID list to a different  
#' DAVID ID type. The mappings are returned in the form of a set of unique pairs.  
#'
#' Due to the recent redesign of the DAVID online query system, it is no longer possible to use the DAVID API 
#' (as described in the documentation for \code{DAVIDQuery}) to perform gene ID conversion. For this reason, the gene ID
#' conversion is implemented as a separate function programmatically reproducing the Gene ID Conversion tool workflow as follows.
#' First, the list of IDs to be converted from the given ID type is submitted to the DAVID tools service using the HTTP message post.
#' Second, the DAVID check 'at least 80 percent of samples should be mapped' turned off by accessing the hidden URL "submitAnyway.jsp"
#' This ensures that the input ID list can contain any percentage of correct IDs and still be mapped properly.
#' Third, the request for ID convertion is sent by posting the HTTP message to the DAVID conversion service.
#' The resulting page is scrapped, the URL of the convertion result file is obtained and the file is retrieved. 
#' As the conversion results file is a well formatted table represented by a tab delimited .txt file,
#' no further formatting of the DAVIDQueryResult is needed.
#'
#' @name convertIDList
#' @param idList The character vector of IDs to be converted.
#' @param curl RCurl handle.
#' @param fromType The type of input IDs. If NULL (default), determined through the popup menu.
#' @param toType The type to convert to. If NULL (default), determined through the popup menu.
#' @param urlBase The DAVID main page url. Default is DAVIDURLBase.
#' @param testMe If TRUE, assign default values and run. Dafault is FALSE.
#' @param annotChoices All available 'From' and 'To' ID conversion types.
#' @param details If TRUE, a list of intermediate results is returned; otherwise, just the final query result. Default is TRUE.
#' @param graphicMenu If TRUE, use a GUI window for the pick menus. Default is FALSE.
#' @param writeHTML If TRUE writes the conversion result html page into the 'conversionResult.html' file. Default is FALSE.
#' @param verbose If TRUE enables diagnostic messages.
#' @return A DAVIDQuery result. The informative part is a data frame consisting of columns 'From', 'To', 'Species' and 'Gene.Name'.
#' The 'From' column contains ID list submitted for conversion, the 'To' column contains the conversion results, 
#' while 'Species' and 'Gene.Name contain species and gene description, correspondingly.
#' The IDs for which conversion is not found (ambiguous IDs in DAVID terms) are not returned. 
#' The missing IDs can be found comparing the input ID set and the unique ID set in the 'From' column.
#' If no conversion at all is found the function returns NULL.
#' @examples
#' \dontrun{
#' idList=c("P04264", "P13645");
#' data<-convertIDList(idList,fromType="UNIPROT_ACCESSION",toType="AFFYMETRIX_3PRIME_IVT_ID",writeHTML=TRUE,verbose=TRUE);
#' }
#' @seealso {\code{\link{DAVIDQuery}}, \code{\link{getIdConversionChoices}}}
#' @export convertIDList
#' @author Roger Day, Alex Lisovich

convertIDList<-function(idList,curl=NULL,fromType=NULL, toType=NULL, urlBase=DAVIDURLBase, 
						testMe = FALSE, annotChoices=getIdConversionChoices(), details = FALSE, 
						graphicMenu = FALSE, writeHTML=FALSE, verbose=FALSE){

	if (is.null(curl)){
		curl<-RCurl::getCurlHandle(cookiefile = "DAVIDCookiefile.txt")
		res<-RCurl::getURL(urlBase, curl = curl);
	}

	if (testMe) {
		idList=c("P04264", "P13645");
		fromType="UNIPROT_ACCESSION";
		toType="AFFYMETRIX_3PRIME_IVT_ID"
	}

	if(is.null(annotChoices))
		annotChoices<-getIdConversionChoices(urlBase,verbose=verbose);

	if (is.null(fromType)){
		item<-menu(graphics = graphicMenu, title = "Select Identifier",  annotChoices$from[,"name"]);
		fromType<-annotChoices$from[item,"value"];
	}

	if (is.null(toType)){
		item<-menu(graphics = graphicMenu, title = "Convert To Identifier",  annotChoices$to[,"name"]);
		toType<-annotChoices$to[item,"value"];		
	}

	if (!(fromType %in% annotChoices$from[,"value"])){
		warning("Invalid source Identifier type");
		return(NULL);
	}

	if (!(toType %in% annotChoices$to[,"value"])){
		warning("Invalid target Identifier type");
		return(NULL);
	}


	#upload ID list
	url1=paste(urlBase,"tools.jsp",sep="");
	res1<-uploadIDList(url1,curl,idList,type=fromType, verbose=verbose);
	if (writeHTML)
		writeChar(res1, "firstStageResult.html");

	#turn off the internal DAVID check 'at least 80% samples should be mapped'
	submitAnyway<-RCurl::getURL(paste(urlBase,"submitAnyway.jsp",sep="/"),curl=curl);

	#submit conversion request
	url2<-paste(urlBase,"conversion2.jsp",sep="");
	res2<-requestIDConversion(url2,curl,type=toType,verbose=verbose);
	if (writeHTML)
		writeChar(res2, "secondStageResult.html");


	#retrieve download file name
	downloadFileName<-findInBrackets(res2,"href=\"",".txt",includeRight=TRUE);

	if (nchar(downloadFileName)>0){
		downloadURL<-paste(urlBase,downloadFileName,sep="");
	
		#retrieve unique pairs
		if (verbose){
			cat("Retrieving conversion file:",downloadURL,"\n\n");
		}

		DAVIDQueryResult<-read.delim(downloadURL, header = TRUE, stringsAsFactors = FALSE)
	}else {
		DAVIDQueryResult<-"No result were found";
		class(DAVIDQueryResult) <- "try-error"
	}

	attr(DAVIDQueryResult, "ids") <- idList;
	attr(DAVIDQueryResult, "tool") <- "geneIdConversion";
	attr(DAVIDQueryResult, "annot") <- toType;
	attr(DAVIDQueryResult, "type") <- fromType;

    	if (details) {
		return(list(ids = idList, firstURL = url1, firstStageResult = res1, 
            	DAVIDaction = "conversion.jsp", secondURL = url2, 
            	secondStageResult = res2, hasSessionEnded = FALSE, 
            	downloadFileName = downloadFileName, downloadURL = downloadURL, 
            	DAVIDQueryResult = DAVIDQueryResult))
	} else {
		return(DAVIDQueryResult);
	}
}


