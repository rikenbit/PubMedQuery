#' Remove HTML comments from character string representing the HTML page
#'
#' @name removeHTMLComments
#' @param string character string representing HTML page
#' @return character string with HTML comments ('<!--...-->') removed
#' @keywords internal
#' @author Roger Day, Alex Lisovich

removeHTMLComments<-function(string){
	split<-strsplit(string,"<!--")[[1]];
	res<-split[1];
	if (length(split)>1){
		for (i in 2:length(split)){
			res<-c(res,strsplit(split[i],split="-->")[[1]][2]);
		}
	}
	res<-paste(res,collapse="");
	return(res);
}

