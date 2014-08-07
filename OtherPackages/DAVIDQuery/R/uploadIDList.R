
#' Upload an ID list to the DAVID query system
#'
#' @name uploadIDList
#' @param url The url to post a request to.
#' @param curl The RCurl handle.
#' @param idList The list of IDs to upload.
#' @param type The DAVID Identifier to convert from.
#' @param sessionID The RCurl session ID to be submitted as part of ID list uploading process.
#' If NULL the session ID is retrieved from curl handle.
#' @param verbose If TRUE, enables diagnostic messages.
#' @return A character string containing the source for a list upload html page.
#' @examples
#' \dontrun{ 
#' myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
#' res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle);
#' url="http://david.abcc.ncifcrf.gov/summary.jsp";
#' idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
#' res2<-uploadIDList(url,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);
#' }
#' @keywords internal
#' @author Roger Day, Alex Lisovich


uploadIDList<-function(url,curl,idList,type="UNIPROT_ACCESSION",  verbose=TRUE){

	if (verbose) {
		cat("URL=",url,":\n");
		cat("Uploading the ID list for",type,"\n\n");
	}

	res<-RCurl::postForm(url,
        	.params=list(
		idType=type,
		uploadType="list",
		multiList="false",
		Mode="paste",
		useIndex="null",
		usePopIndex="null",
		demoIndex="null",
		ids=paste(idList,collapse=" "),
		removeIndex="null",
		renameIndex="null",
		renamePopIndex="null",
		newName="null",
		combineIndex="null",
		selectedSpecies="null",
		SESSIONID="",
		uploadHTML="",	
		managerHTML="",	
		sublist="",	
		rowids="",	
		convertedListName="null",
		convertedPopName="null",
		fileBrowser="",	
		Identifier=type,
		rbUploadType="list"),curl=curl
	);


	return(res);		
}


