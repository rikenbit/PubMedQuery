formatDAVIDResult <- function(result, verbose=FALSE) {
	### we always use read.delim(...header=TRUE) but formatting expects the first row tobe the column names
	### in order to make formatting work we add the top row
	result<-rbind(colnames(result),result);

	tool <- attr(result,"tool") 
	if(verbose) 
		cat("formatDAVIDResult: tool=", tool, 
			ifelse(tool=="geneReportFull", 
			" (invisible return)", ""), 
		"\n")
	### formatting depends on which is done
	if(tool=="geneReportFull") {
		returnval <- try(formatGeneReportFull(result))
	} else if(tool=="geneReport") {
		returnval <- try(formatGeneReport(result))
	} else if(tool=="list") {
		returnval <- try(formatList(result))
	} else if(tool=="gene2gene") {
		returnval <- try(formatGene2Gene(result))
	} else if(tool=="annotationReport") {
		returnval <- try(formatAnnotationReport(result))
	} else
		returnval <- result  ### Unformatted for now.
	if(class(returnval) == "try-error")
		returnval <- result
	for(attname in c("annot", "ids", "tool", "type"))
		attr(returnval, attname) <- attr(result, attname)
	returnval
}
