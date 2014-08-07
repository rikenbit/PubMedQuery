
formatGene2Gene<- function(result){

	groupRows <- grep("Group", result[ , 1])

	res<-list();
	for (ind in 1:length(groupRows)) {
		theseRows <- groupRows[ind]: ifelse(ind == length(groupRows), dim(result)[1], groupRows[ind+1]-1);
		chunk <- result[theseRows, ];
		group<-chunk[1,1];
		name_value<-chunk[1,2];
		name=substr(name_value,1,nchar("Enrichment Score"));
		value=as.numeric(substr(name_value,nchar("Enrichment Score: ")+1,nchar(name_value)));
		details<-data.frame(chunk[-c(1:2),]);
		colnames(details)<-chunk[2,];
		res[[group]]<-list(type=name,value=value,details=details);
	}
	invisible(res)
}

#original code
#formatGene2Gene <- function(result){
#	groupRows <- grep("Functional", result[ , 1])
#	processDiagram <- function(diagramString){
#		lapply(
#		strsplit(
#			strsplit(diagramString, split="\\$")[[1]]
#			, split=";|:"),
#			function(svec) {
#				preSemi <- strsplit(svec[1], split=",")[[1]]
#				preSemi <- preSemi[preSemi != ""]
#				list(preSemi=preSemi,
#					preColon=svec[2],
#					postColon=as.numeric(svec[3]))
#			}
#		)
#	}	
#	returnValue <- lapply(1:length(groupRows),
#		function(ind){
#			theseRows <- groupRows[ind]:
#				ifelse(ind == length(groupRows), 
#						dim(result)[1],
#						groupRows[ind+1]-1)
#			#cat("formatGene2Gene: ind=", ind, " theseRows=", theseRows[1], ":", rev(theseRows)[1])
#			chunk <- result[theseRows, ]
#			list(
#				median=as.numeric(substring(
#					chunk[1, 2], nchar("Median: ")+1)),
#				geo=as.numeric(substring(
#					chunk[1, 3], nchar("Geo: ")+1)),
#				diagram=processDiagram(chunk[1,4]),
#				details=data.frame(
#					gene=chunk[-1, 1],
#					geneName=chunk[-1, 2],
#					url=chunk[-1, 3]
#					)
#			)	
#		}
#	)  ### end of "lapply" function.
#	invisible(returnValue)
#}
#

