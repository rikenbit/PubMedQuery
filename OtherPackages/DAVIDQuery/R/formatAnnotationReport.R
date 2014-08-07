formatAnnotationReport <- function(result){
	ids <- strsplit(attr(result,"ids") , split=",")[[1]]
	annot <- attr(result,"annot") 
	type <- attr(result,"type") 
	idRows <- match(ids, result[,1])
	theFeatures <- as.vector(unlist(result[1, , drop=T]))
	nFeatures <- length(theFeatures)
	temp1 = t(result[ -1,  ])  ## remove header row
	rownames(temp1) = theFeatures
	colnames(temp1) = ids
	sapply(simplify=F, ids, 
		function(id)
			sapply(simplify=F, theFeatures, function(feat) {
					s = temp1[ feat, id]
					#print(feat)
					#print(s)
					#ifelse(feat == "Gene Name",  s,  strsplit(s, ", ")[[1]] )
					###  a bug in ifelse() ???
					if(feat == "Gene Name") return (s) else return( strsplit(s, ", ")[[1]]) 
				
				}
			))
}
