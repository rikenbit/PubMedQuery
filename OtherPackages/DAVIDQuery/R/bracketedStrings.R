bracketedStrings <-
function(s, before, after, verbose=FALSE, addNames=FALSE, drop.na=TRUE, warn.if.gt.1=TRUE) {
	if(length(s) > 1) {
		result <- lapply(s, bracketedStrings, 
			before=before, after=after, verbose=FALSE)
		if(addNames) names(result) = s
		return(result)
	}
	starts <- (valStrings <- gregexpr(before, s)[[1]]) + attr(valStrings, "match.length")
	ends <- regexpr(after, (vStrings <- substring(s, starts,1e6)) ) - 2
	result <- substring(s, starts, starts+ends)
	if(verbose)
		cat(paste("=>",starts, starts+ends, result, sep=":", collapse="\n"))
	result <- result[result != ""]
	if(drop.na) result <- result[!is.na(result)]
	result <- result[starts>=0 & ends>=0]
	if((length(result) > 1) & (warn.if.gt.1))
		warning("More than one substring found.")
	return(result)
}

