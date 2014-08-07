formatList <- function(result){
	names(result) <- result[1, ]
	result <- result [-1, ]
	#try(dimnames(result)[[1]] <- result[,1])
	#dimnames(result)[[1]] <- 1:nrow(result)
	result
}
