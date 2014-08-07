`DAVIDQueryLoop` <-
function(
	idList=unlist(strsplit( strsplit(
"P31946 P62258 P29360 P42655 Q63631
P01892 O19619 P06338 P10313 P30444 P30445 P30446 P30514
", " ")[[1]], "\n")),
		idLimit=100,
		sleepSeconds=10, 
		hitsPerDayLimit=200,
		verbose=FALSE,
		testMe=FALSE,
		type,
		annot,
		tool,
		graphicMenu=FALSE,
		formatEach=FALSE,
		formatAll=FALSE,
...
) {	
	if(testMe) {
		type <- "UNIPROT_ACCESSION" 
		annot <- NULL 
		tool <- "geneReportFull" 
		tool <- "geneReport" 
		verbose <- TRUE
		idLimit <- 8
		sleepSeconds <- 10 
	} else {
		if(missing(type)) {
			type <- DAVIDTypeChoices[menu(DAVIDTypeChoices,
				graphics=graphicMenu, title="Choose an ID TYPE")]
		}
		if(missing(annot)) {
			annot <- DAVIDAnnotChoices[menu(DAVIDAnnotChoices,
				graphics=graphicMenu, title="Choose an Annotation")]
			if(length(annot) == 0) annot <- NULL
		}
		if(missing(tool)) {
			tool <- DAVIDToolChoices[menu(DAVIDToolChoices,
				graphics=graphicMenu, title="Choose a TOOL")]
		}
	}
	### To check for revised limits, see http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html .
	idLimit <- min(idLimit, 400)
	idsRemaining <- idList
	loopCounter <- 0
	while((length(idsRemaining) > 0) & (loopCounter < hitsPerDayLimit)) {
		## TODO: improve management of hitsPerDayLimit.
		numberToSend <- min(idLimit, length(idsRemaining))
		theIDstring <- paste(idsRemaining[1:numberToSend], collapse=",")
		if(verbose) cat("DAVIDQueryLoop (", loopCounter, "): sending ", theIDstring, "\n")
		thisReply <- try(DAVIDQuery(ids=theIDstring, details=FALSE, type=type, annot=annot, tool=tool, formatIt=formatEach, ...))
		ids.actually.done <- strsplit(attr(thisReply, "ids"), split=",")[[1]]
		numberProcessed <- length(ids.actually.done)
		if(numberProcessed < length(ids.actually.done)) {
			if(verbose)	warning("IDs truncated due to URL length limit")
		}
#		print(ids.actually.done)
#		try(row.names(thisReply) <- NULL)
#		cat("row names:\n",  row.names(thisReply), "\n")
		loopCounter <- loopCounter + 1
		###   geneReport formatted works with rbind
		tryErrors <- list()
		if(loopCounter == 1){
			#if(is.list(thisReply)) DAVIDQueryResult <- list(thisReply)
			#else 
				DAVIDQueryResult <- thisReply
		} else {
			if(class(thisReply) == "try-error")
				tryErrors = c(tryErrors, thisReply)
			if(is.data.frame(thisReply)) 
				DAVIDQueryResult <- rbind(DAVIDQueryResult, thisReply[-1,])  #remove extra header row
			else DAVIDQueryResult <- c(DAVIDQueryResult, thisReply)
			timeElapsed <- difftime(Sys.time(), the.time, units="secs")
			timeToSleep <- max(0, sleepSeconds - timeElapsed + 1)
			if(verbose) cat("DAVIDQueryLoop: Sleeping for ", timeToSleep, " seconds ...\n")
			Sys.sleep(timeToSleep)
		}
		the.time <- Sys.time()
		idsRemaining <- idsRemaining[-(1:numberToSend)]
		if(verbose) cat("DAVIDQueryLoop:  Finished call #", loopCounter, ".\n")
	}
	attr(DAVIDQueryResult, "ids") <- idList
	attr(DAVIDQueryResult, "tool") <- tool
	attr(DAVIDQueryResult, "annot") <- annot
	attr(DAVIDQueryResult, "type") <- type
	attr(DAVIDQueryResult, "tryErrors") <- tryErrors
	
	if(loopCounter == hitsPerDayLimit)
		warning("DAVIDQueryLoop:  loopCounter exceeded hitsPerDayLimit")
	if(formatAll) {
		formatReturn <- try( return(invisible(formatDAVIDResult(DAVIDQueryResult))))
		if(class(formatReturn) == "try-error")
			attr(DAVIDQueryResult, "format error") <- formatReturn 
	}
 	invisible(DAVIDQueryResult)
}
