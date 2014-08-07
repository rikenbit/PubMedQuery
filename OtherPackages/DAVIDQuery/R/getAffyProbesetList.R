
getAffyProbesetList<-function (chipname = NULL, menu = TRUE, verbose = FALSE) 
{
    chipChoices<-getAffyChipTypes(DAVIDURLBase,verbose=verbose);

    if (menu==TRUE || is.null(chipname)) {
	item<-max(1,menu(graphics = TRUE, title = "Choose an array name",	chipChoices[,"name"]));
	chipname<-chipChoices[item,"name"];
    }

    arrayLink<-chipChoices[chipChoices[,"name"]==chipname,"value"];
   
    if (verbose) 
        cat("chipname=", chipname, "\n");

    theURL <- paste(DAVIDURLBase, "ease/update/", arrayLink, sep = "");

    if (verbose) 
        cat("theURL=", theURL, "\n");

    theURL<-gsub(" ","%20",theURL,fixed=TRUE);

    result <- strsplit(RCurl::getURL(theURL), split = "\n")[[1]];
    attr(result, "chipType") <- chipname;

    return(result);
}
