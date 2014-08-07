formatGeneReport = function(result){

	attr(result, "firstLine") = result[1, ]
	result <- formatList(result)
	attr(result, "geneReportMessage") <- "CAUTION: The Gene Report is not just the rbind of results for the individual targets!"
	result
}


#original code
#formatGeneReport = function(result){
#
#	attr(result, "firstLine") = result[1, ]
#	result <- result [-1, ]
#	result <- formatList(result)
#	attr(result, "geneReportMessage") <- "CAUTION: The Gene Report is not just the rbind of results for the individual targets!"
#	result
#}
