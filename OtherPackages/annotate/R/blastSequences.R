## Try five times and give error if all attempts fail.
.tryParseResult <- function(url){
 for (i in 1:6) {
     result <- tryCatch({
         xmlTreeParse(url, useInternalNodes=TRUE,
                      error = xmlErrorCumulator(immediate=FALSE))
     }, error=function(err) NULL)
     if (!is.null(result)) return(result)
     Sys.sleep(30)
 }
 stop("no results after 5 attempts; please try again later")
}

## Using the REST-ish API described at
## http://www.ncbi.nlm.nih.gov/blast/Doc/node2.html
blastSequences <- function(x,database="nr",
                           hitListSize="10",
                           filter="L",
                           expect="10",
                           program="blastn"){
   ## TODO: lots of argument checking and testing.  Also,
   ## depending on which program string is used we need to make the correct
   ## kind of object at the end (so blastn means DNAMultipleAlignment, and
   ## blastp means AAMultipleAlignment etc.

   ## So:
   ## 1) get online values these parameters can be
   ## 2) document those
   ## 3) restrict their vals in the code here.
   ## 4) for program, use this to determine what object is returned.
   
   ## assemble the query
   baseUrl <- "http://www.ncbi.nlm.nih.gov/blast/Blast.cgi"
   query <- paste("QUERY=",as.character(x),"&DATABASE=",database,
                  "&HITLIST_SIZE=",hitListSize,"&FILTER=",filter,
                  "&EXPECT=",expect,"&PROGRAM=",program, sep="")
   url0 <- sprintf("%s?%s&CMD=Put", baseUrl, query)
   results <- tempfile()
   ## wait 5 seconds before request, to be polite, as requested
   ## by the documentation (if we are going to make several requests).
   ## Sys.sleep(10)
   require(XML)
   post <- htmlTreeParse(url0, useInternalNodes=TRUE)
   
   x <- post[['string(//comment()[contains(., "QBlastInfoBegin")])']]
   rid <- sub(".*RID = ([[:alnum:]]+).*", "\\1", x)
   ## start by taking however long NCBI thinks it will take and multiply by 20
   ## sadly, I did NOT make that number up.  :(
   rtoe <- as.integer(sub(".*RTOE = ([[:digit:]]+).*", "\\1", x)) * 20
   
   url1 <- sprintf("%s?RID=%s&FORMAT_TYPE=XML&CMD=Get", baseUrl, rid)
   ## wait RTOE seconds
   message("Waiting for NCBI to process the request")
   Sys.sleep(rtoe)   
   result <- .tryParseResult(url1)
   qseq <- xpathApply(result, "//Hsp_qseq", xmlValue)
   hseq <- xpathApply(result, "//Hsp_hseq", xmlValue)

   ## Instead lets put it into a DNAStringSet and make a MultipleSeqAlignment
   ## out of it.
   require(Biostrings)
   res <- list()
   for(i in seq_len(length(qseq))){
     res[i] <- DNAMultipleAlignment( c(hseq[[i]],qseq[[i]]),
                                     rowmask=as(IRanges(), "NormalIRanges"),
                                     colmask=as(IRanges(), "NormalIRanges"))
   }
   res 
}

## took 11.5 minutes to do a blast...  (ugh)
