#' Request a conversion for a previously uploaded ID List
#' 
#' @name requestIDConversion
#' @param url The url to post a request to.
#' @param curl The RCurl handle.
#' @param type The DAVID ID Identifier to convert to.
#' @param verbose If TRUE enables diagnostic messages.
#' @return A character string containing the source for a conversion results html page
#' including the url to the conversion results text file.

#' @examples
#' \dontrun{
#' myCurlHandle<-RCurl::getCurlHandle(cookiefile="DAVIDCookies.txt");
#' res1<-RCurl::getURL("http://david.abcc.ncifcrf.gov", curl = myCurlHandle,verbose = TRUE);
#' url1="http://david.abcc.ncifcrf.gov/tools.jsp";
#' idList=c("P04264", "P13645", "P35908", "P08729", "P08727", "P25705", "P06576", "Q2KHP4", "Q14764", "P14625");
#' res2<-uploadIDList(url1,myCurlHandle,idList,type="UNIPROT_ACCESSION",verbose=TRUE);
#' url2<-"http://david.abcc.ncifcrf.gov/conversion2.jsp";
#' res3<-requestIDConversion(url=url2,curl=myCurlHandle,type="AFFYMETRIX_3PRIME_IVT_ID",verbose=TRUE);
#' writeChar(res3, "thirdStageResult.html")
#' }
#' @keywords internal
#' @author Roger Day, Alex Lisovich 

requestIDConversion<-function(url,curl,type="AFFYMETRIX_3PRIME_IVT_ID",verbose=TRUE){
	if (verbose) {
		cat("URL=",url,":\n");
		cat("Request conversion to",type,"\n\n");
	}

	res<-RCurl::getForm(url,
		.params=c(
		status="showResult",
		uploadType="",
		convertTo=type,
		Submit="Submit to Conversion Tool"),curl=curl
	);
	return(res);
}

