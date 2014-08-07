#' Extract the substring(s) between the left and right brackets
#' where left is the last occurence of the left bracket
#' before the first occurence of the right bracket
#'
#' @name findInBrackets
#' @param string character string to extract substring from
#' @param left left bracket (character string)
#' @param right right bracket (character string)
#' @param includeLeft if TRUE include left bracket into the result string. Default is FALSE
#' @param includeRight if TRUE include right bracket into the result string. Default is FALSE
#' @return substring between the left and right brackets. If not found, returns empty string.
#' @examples
#' \dontrun{
#' # obtain an HTML page represented by a character string conversionPage
#' # then extract the link to the data file with extension 'txt':
#' fileName<-findInBrackets(conversionPage,"href=\"",".txt");
#' }
#' @keywords internal
#' @author Roger Day, Alex Lisovich


findInBrackets<-function(string,left,right,includeLeft=FALSE,includeRight=FALSE){
	pos1<-gregexpr(right,string)[[1]];
	pos2<-gregexpr(left,string)[[1]];
	if (pos1[1]<1)
		return(NULL);
	res<-NULL;
	for (pos in pos1) {
		if (pos2[1]>0){
			start<-pos2[pos2<pos];
			if (length(start)>0){
				start<-start[length(start)]+(!includeLeft)*(nchar(left));
				end<-pos+includeRight*nchar(right)-1;
				res<-c(res,as.character(substr(string,start,end)));
			}
		}
	}
	if(is.null(res))
		res<-"";

	return(res);
}

