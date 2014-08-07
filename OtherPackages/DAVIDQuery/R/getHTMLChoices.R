#' Get (name,value) pairs from the HTML page selection box
#'
#' @name getHTMLChoices
#' @param page character string representing HTML page
#' @param header character string uniquely identifying selection box
#' @return data frame containing (name,value) pair columns.
#' The first column is a decorated ID name, i.e. the name which appears in the corresponding drop-down list on the DAVID web site.
#' The second column is actual value used in a conversion request corresponding to a particular decorated name.
#' @keywords internal
#' @author Roger Day, Alex Lisovich

getHTMLChoices<-function(page,header) {

	selection<-findInBrackets(page, header,"</select>")[1];

	selection<-removeHTMLComments(selection);

	choices<-findInBrackets(selection,"<option value","</option>");

	choices<-gsub("= ","",choices,fixed=TRUE);
	choices<-gsub(" =","",choices,fixed=TRUE);
	choices<-gsub("=","",choices,fixed=TRUE);
	choices<-gsub("\"","",choices,fixed=TRUE);
	choices<-gsub("'","",choices,fixed=TRUE);
	choices<-gsub("\r\n","",choices,fixed=TRUE);

	inds<-c(1:length(choices))*2;
	split<-unlist(strsplit(choices,split=">"));
	res<-cbind(split[inds],split[inds-1]);
	colnames(res)<-c("name","value");
	return(as.data.frame(res,stringsAsFactors=FALSE));
}

