#Copyright 2001 R.Gentleman, all rights reserved
#functions to look up particular genes at different sites
# Modifications to htmlpage and getQuery4XX functions added
# 7-12-04 by J. MacDonald

UniGeneQuery <- function(query, UGaddress="UniGene/",
                         type="CID") {
    if (missing(query))
        stop("No query, cannot proceed!")

    ##they are of the form HH.xxxx, where HH specifies the species
    q1 <- strsplit(query, "\\.")
    if( length(q1[[1]]) == 2 ) {
        id <- sapply(q1, function(x) x[2])
        species <- sapply(q1, function(x) x[1])
    }

    ncbiURL <- .getNcbiURL()
    ## Build up the query URL

    query <- paste(ncbiURL, UGaddress,
    "clust.cgi?ORG=",species,"&", type, "=",id, sep="")

    return(query)
}

entrezGeneByID <- function(query) {
    
    if (missing(query))
        stop("No query, cannot proceed!")

    ncbiURL <- .getNcbiURL()
    ## Build up the query URL

    query <- paste(ncbiURL, "/sites/entrez?db=gene&cmd=search&term=",query, sep="")

    return(query)
}

entrezGeneQuery <- function(query) {

    if (missing(query))
        stop("No query, cannot proceed!")

    ncbiURL <- .getNcbiURL()
    ## Build up the query URL

    str = ""
    ##reduce the set of parameters so that they are all one concatenated thing
    for(i in seq_len(length(query))){
        if(i==1){str=query[i]}else{
        str = paste(str,"%20",query[i],sep="")
      }
    }
        
    query <- paste(ncbiURL, "/sites/entrez?db=gene&cmd=search&term=",str, sep="")

    return(query)
}

pmidQuery <- function(query) {
    if (missing(query))
        stop("No query, cannot proceed!")

    query <- paste(query,collapse="%2c")
    ncbiURL <- .getNcbiURL()

    query <- paste(ncbiURL,"/entrez/query.fcgi?cmd=Retrieve&db=PubMed&",
                 "list_uids=",query,"&dopt=Abstract&tool=bioconductor",sep="")

    return(query)
}

genbank <- function(..., disp=c("data","browser"),
                    type=c("accession", "uid"),
                    pmaddress=.efetch("gene",disp,type)) {
    params <- list(...)
    params <- unlist(params)
    disp <- match.arg(disp)
    type <- match.arg(type)

    if (length(params) == 0) {
        stop("No Gene ID, cannot proceed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse="%2c")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type,db="genbank")

   if (is.null(args)) {
        print(paste("No XML records available for accession number",err))
        return(NULL)
    }

    id <- .getIdTag(disp,type)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")

    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        return(.handleXML(query))
    }
    else {
        browseURL(query)
    }
}

## bad query string:
## query = "http://www.ncbi.nih.gov/entrez/utils/pmfetch.fcgi?report=xml&mode=text&tool=bioconductor&db=Nucleotide&id=571320,4103966"

pubmed  <- function(..., disp=c("data","browser"),
                    type=c("uid","accession"),
                    pmaddress=.efetch("PubMed",disp,type)) {
    params <- list(...)
    params <- unlist(params)

    disp <- match.arg(disp)
    type <- match.arg(type)

    if (length(params) == 0) {
        stop("No PMID, cannot proceed")
    }

    ncbiURL <- .getNcbiURL()

    ## Build up the query URL
    args <- paste(params,collapse="%2c")
    ## See if we need to transform accession based arguments
    err <- args
    args <- .transformAccession(args, disp, type,"pubmed")

    if (is.null(args)) {
        print(paste("No XML records available for accession number",err))
        return(NULL)
    }

    id <- .getIdTag(disp,type)

    query <- paste(ncbiURL, pmaddress, id, args, sep="")


    ## Determine if we are displaying this data in a browser or
    ## returning an XMLDocument object
    if (disp == "data") {
        return(.handleXML(query))
    }
    else {
        browseURL(query)
    }
}


accessionToUID <- function(...,db=c("genbank","pubmed")) {
    #require(XML)
    ## Passed an accession #, returns a pubmed UID
    accNum <- list(...)
    accNum <- unlist(accNum)
    accNum <- paste(accNum,collapse="+OR+")

    db <- match.arg(db)

    ## Certain functions will be passing in a single string of comma
    ## deliminated Accession #s.  Change the commas to "+OR+"
    accNum <- gsub("\\,","+OR+",accNum)

    if (db == "genbank") {
        db <- "gene"
    }
    else {
        db <- "PubMed"
    }
    query <- paste(.getNcbiURL(), "entrez/eutils/esearch.fcgi?db=", db,
                   "&tool=bioconductor&term=",accNum,sep="")

    ## parse using XML package
    doc <- xmlParse(query)
    res <- xpathApply(doc=doc, path="/eSearchResult/IdList/Id",
                      fun=xmlValue)
    
    retVal <- unlist(res)
    if (length(retVal)==0){retVal <- NULL} else {
	retVal <- paste(retVal, collapse=",")
    }

    return(retVal)
}


.handleXML <- function(query,handlers=NULL) {
    ## In the case of an error retrieving proper XML output,
    ## will return NA to the calling function
    require(XML) || stop("Sorry, you need the XML package!")
    ## Make sure that XML version is what we require
    ## !!! Need to make this automatic, hardcode version in for now
    xmlVers <- packageDescription("XML",fields="Version")
    reqXmlVers <- "0.92-2"
    if (compareVersion(xmlVers,reqXmlVers) < 0)
        stop(paste("Installed XML version is ",xmlVers,
                   " while this functionality requires ", reqXmlVers,
                   ":  Please update your XML package.",sep=""))

    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages = TRUE))

    ## get the XML file contents from URL, and remove extra
    ## text strings before <xml...
    query <- paste(scan(query, what="", sep="\n"), "\n", collapse="\n")
    query <- sub("^[^<]*<(.*)", "<\\1",query)

    retVal <- NULL
    xml <- try(xmlTreeParse(query,asText=TRUE,handlers=NULL,asTree=TRUE))
    options(show.error.messages = TRUE)

    if (inherits(xml,"try-error") == TRUE) {
        return(NA)
    }

    return(xml)
}

.getNcbiURL <- function() {
    ## Returns the URL for NCBI, which should be located in Annotate's
    ## option set
    BioCOpt <- getOption("BioC")

    if (!is.null(BioCOpt)) {
        ncbiURL <- BioCOpt$annotate$urls$ncbi
    }

    if (!exists("ncbiURL")) {
        ncbiURL <- "http://eutils.ncbi.nlm.nih.gov"
        ## old one: "http://www.ncbi.nih.gov/"
    }

    return(ncbiURL)
}

.getIdTag <- function(disp=c("data","browser"),
                      type=c("uid","accession")) {
    disp <- match.arg(disp)
    type <- match.arg(type)

    if (disp == "data") {
        return("&id=")
    }
    else {
        if (type == "uid") {
            return("&list_uids=")
        }
        else {
            return("&term=")
        }
    }
}

## TODO: retire this method by replacing it with .efetch (NCBI is no longer supporting URLs of this ilk)
## .pmfetch <- function(db="PubMed", disp=c("data","browser"),
##                      type=c("uid","accession")) {
##     ## Returns the base query string for the pmfetch engine @ pubmed

##     disp <- match.arg(disp)
##     type <- match.arg(type)

##     if (disp == "data") {
##         base <-
##     "entrez/utils/pmfetch.fcgi?report=xml&mode=text&tool=bioconductor&db="
##     }
##     else {
##         base1 <- "entrez/query.fcgi?tool=bioconductor&cmd="
##         if (type == "uid") {
##             base2 <- "Retrieve&db="
##         }
##         else {
##             base2 <- "Search&db="
##         }
##         base <- paste(base1,base2,sep="")
##     }
##     return(paste(base,db,sep=""))
## }


## Needed to replace the aging (and obsoleted by NCBI) pmfetch...
.efetch <- function(db="PubMed", disp=c("data","browser"),
                     type=c("uid","accession")) {

    ## Returns the base query string for the efetch engine

    disp <- match.arg(disp)
    type <- match.arg(type)

    if (disp == "data") {
        base <-
    "entrez/eutils/efetch.fcgi?tool=bioconductor&rettype=xml&retmode=text&db="
    }
    else {
        base1 <- "entrez/query.fcgi?tool=bioconductor&cmd="
        if (type == "uid") {
            base2 <- "Retrieve&db="
        }
        else {
            base2 <- "Search&db="
        }
        base <- paste(base1,base2,sep="")
    }
    return(paste(base,db,sep=""))
}


.transformAccession <- function(args, disp, type, db) {
    ## Used to change accession ID arguments to query functions
    ## into UIDs if necessary.  Returns NULL if there aren't any left.
    if ((disp == "data")&&(type=="accession")) {
        args <- accessionToUID(args,db=db)
    }

    return(args)
}

genelocator <- function(x) {
    .Defunct("none", package="annotate", msg = "is no longer supported")
}


pmAbst2HTML <- function(absts, filename, title, frames = FALSE,
                      table.center=TRUE) {
    ## Currently just a very naive implementation of a pmid2html type
    ## of thing.  Intended to be temporary just while I'm testing some
    ## of this stuff.

    if (!is.list(absts)) {
        if (is(absts,"pubMedAbst"))
            absts <- list(absts)
        else
            stop("'absts' parameter does not seem to be valid.")
    }

    ## Assign a default filename.  If we're using frames, then
    ## 'filename' is really just the base filename, so make it empty
    if (missing(filename))
        if (frames)
            fileName <- ""
        else
            filename <- "absts.html"

    if (missing(title))
        title <- "BioConductor Abstract List"

    nrows = length(absts)
    pmids <- unlist(lapply(absts,pmid))
    dates <- unlist(lapply(absts,pubDate))
    queries <- unlist(lapply(absts,
                             function(x){pm <- pmid(x);out<-pmidQuery(pm);out}))
    titles <- unlist(lapply(absts, articleTitle))
    ## If we're using frames, need to point the anchors to
    ## the main frame, otherwise not.
    anchors <- makeAnchor(queries, titles, toMain=frames)

    topText <- paste("<html>\n<head>\n<title>", title, "</title>",
                     "\n</head>\n<body bgcolor=#708090>\n",
                     "<H1 ALIGN=CENTER>", title, "</H1>\n",
                     "</body></title>", sep="")
    head <- c("Article Title", "Publication Date")
    headOut <- paste("<TH>", head, "</TH>", collapse="\n")

    if (frames) {
        top <- new("HTMLPage", fileName=paste(filename,"Top.html",sep=""),
                   pageText= topText)
        tableHeader <- paste("<TR>",headOut,"</TR>", sep="\n")
        sideText <- paste("<TABLE BORDER=1>", tableHeader, sep="\n")

        tds <- paste("<TD>",anchors,"</TD><TD>",dates,"</TD>",sep="",
                     collapse="\n</TR>\n<TR>\n")
        tds <- paste("<TR>",tds,"</TR>")
        sideText <- paste(sideText, tds)
        if (table.center)
            sideText <- paste("<CENTER>",sideText,"</CENTER>", sep="\n")
        sideText <- paste("<html>", "<head>",
                          "<title>BioConductor Abstract List</title>",
                          "</head>","<body bgcolor=#708090>",
                          sideText, "</body>", "</html>", sep="\n")
        side <- new("HTMLPage",
                    fileName=paste(filename,"Side.html",sep=""),
                    pageText=sideText)

        metaText <- paste("<meta HTTP-EQUIV=\"REFRESH\" CONTENT=\"1;",
                          queries[1],"\">",sep="")
        mainText <- paste("<html>", "<head>",
                          "<title>BioConductor Abstract List</title>",
                          "</head>","<body bgcolor=#708090>",
                          metaText,
                          "</body>","</html>", sep="\n")

        main <- new("HTMLPage",
                    fileName=paste(filename,"Main.html",sep=""),
                    pageText=mainText)

        page <- new("FramedHTMLPage", topPage=top, sidePage=side, mainPage=main,
                    fileName=paste(filename,"index.html",sep=""),
                    pageTitle=title)
        toFile(page)
    }
    else {
        outfile <- file(filename,"w")
        cat(topText, file = outfile)
        if( table.center )
            cat("<CENTER> \n", file=outfile)

        cat("<TABLE BORDER=1>", file = outfile, sep = "\n")
        cat("<TR>",headOut,"</TR>", file=outfile, sep="\n")

        tds <- paste("<TD>",anchors,"</TD><TD>",dates,"</TD>",sep="")
        for (td in tds)
            cat("<TR>", td, "</TR>", file=outfile,sep="\n")

        cat("</TABLE>",file=outfile)
        if( table.center )
            cat("</CENTER> \n", file=outfile)
        cat("</body>", "</html>", sep = "\n", file = outfile)
        close(outfile)
    }
    invisible(NULL)
}

htmlpage <- function (genelist, filename, title, othernames, table.head,
                      table.center=TRUE, repository = list("en"), ...){
    if(!is.list(repository))
        stop("The repository argument must be a list!", call. = FALSE)
    chklen <- function(x){
        if(is.data.frame(x) || is.matrix(x)) dim(x)[1]
        else length(x)
    }
    getRows <- function(x){
        paste("<P>", x, "</P>", collapse="", sep="")
    }

    if(is.data.frame(genelist))
        len.vec <- chklen(genelist)
    else
        if(is.list(genelist))
            len.vec <- sapply(genelist, chklen)
        else
            stop("The 'genelist' should be either a data.frame or a list",
                 call.=FALSE)
    if(!missing(othernames)) {
       if(is.data.frame(othernames))
        len.vec <- c(len.vec, chklen(othernames))
       else if( is.list(othernames))
            len.vec <- c(len.vec, sapply(othernames, chklen))
        else
            stop("The 'othernames' should be either a data.frame or a list",
                 call.=FALSE)
    }
    if(any(len.vec != len.vec[1]))
        stop(paste("Some items in either", genelist, "or", othernames,
                   "have mis-matched lengths.\nPlease check this",
                   "discrepancy and re-run.\n"), .call=FALSE)

    out <- mapply(getCells, genelist, repository, ..., SIMPLIFY=TRUE)

    if (!missing(othernames)) {
        if(is.data.frame(othernames))
            out <- data.frame(out, othernames)
        else
            if (is.list(othernames)) {
                ## if othernames is a list, we have to ensure we handle
                ## the contents of the list correctly
                ## e.g., cbind()ing a factor will coerce things incorrectly
                ## here we just put everything in another list that we can
                ## then coerce to a data.frame
                others <- vector("list", length(othernames))
                for(i in seq(along=othernames)){
                    if(is.data.frame(othernames[[i]]))
                        others[[i]] <-  othernames[[i]]
                    else
                        if(is.list(othernames[[i]])){
                            ## if othernames[[i]] is a list, the assumption
                            ## here is that we want a multi-line table entry
                            ## in the HTML page
                            others[[i]] <- sapply(othernames[[i]],
                                                  getRows)
                        }else{
                            others[[i]] <- othernames[[i]]
                        }
                }
                out <- data.frame(out, as.data.frame(others))
            }
    }

    colnames(out) <- table.head
    out <- xtable(out, caption=if(!missing(title)) title, ...)
    print(out, type="html", file=filename, caption.placement="top",
          include.rownames=FALSE, sanitize.text.function=function(x) x,
          ...)
}

getCells <-  function(ids, repository = "ug", ...){
  # This function allows us to insert multiple links in each cell by
  # building up the HTML more incrementally. Passing a list of character
  # vectors will result in multiple links per cell. Otherwise we get one link per cell.

  if(is.list(ids)){
    out <- vector()
    temp <- lapply(ids, getQueryLink, repository=repository, ...)
    for(i in seq(along = ids)){
      if(temp[i] != "&nbsp;")
        out[i] <- paste("<P><A HREF=\"", temp[[i]], "\">",
                        ids[[i]], "</A></P>", sep = "", collapse="")
      else
        out[i] <- temp[i]
    }
  }else{
    temp <- getQueryLink(ids, repository, ...)
    blanks <- temp == "&nbsp;"
    out <- paste(" <A HREF=\"", temp, "\">",
                 ids, "</A>", sep = "")
    out[blanks] <- "&nbsp;"
  }
  return(out)
}

## getQueryLink <-function (ids, repository = "ug", ...){
##   switch(tolower(repository), ug = return(getQuery4UG(ids)),
##          gb = return(getQuery4GB(ids)), sp = return(getQuery4SP(ids)),
##          omim = return(getQuery4OMIM(ids)), fb = return(getQuery4FB(ids)),
##          en = return(getQuery4EN(ids)), tr = return(getQuery4TR(ids)),
##          go = return(getQuery4GO(ids)), ens = return(getQuery4ENSEMBL(ids, ...)),
##          random = return(getQuery4Random(ids)), stop("Unknown repository name"))
## }

## Code from Martin Morgan that allows end user to add arbitrary
## repository

## the interface: set, get, clear
setRepository <- function(repository, FUN, ..., verbose=TRUE) 
{
    ## checs on repository, FUN, then...
    if (verbose && exists(repository, .repositories))
        warning("replacing repository '", repository, "'")
    .repositories[[repository]] <- FUN
}

getRepositories <- function()
{
    ls(.repositories)
}

clearRepository <- function(repository, verbose=TRUE)
{
    if (!(length(repository) == 1 && is.character(repository)))
        stop("argument 'repository' must be character(1)")
    ## check repository, then
    if (exists(repository, .repositories))
        rm(list=repository, envir=.repositories)
    else if (verbose)
        warning("undefined repository '", repository, "'")
}

## this should be backward compatible
getQueryLink <- function (ids, repository = "ug", ...) 
{
    if (!exists(repository, .repositories))
        stop("unknown repository '", repository, "'")
    .repositories[[repository]](ids, ...)
}


getTDRows <- function (ids, repository = "ug", ...){
  # Modification of Jianhua's original code to allow for multiple links per cell.
  out <- paste("<TD>", getCells(ids, repository), "</TD>", sep="")
  return(out)
}

getQuery4GO <- function(ids, ...) {
##GO IDs
  blanks <- ids == "&nbsp;"
  AMIGO_URL <- "http://amigo.geneontology.org/cgi-bin/amigo/term_details?term="
  out <- paste(AMIGO_URL, ids, sep = "")
  out[blanks] = "&nbsp;"
  return(out)
}

getQuery4Affy <- function (ids, ...){
  # Affy IDs are all over the map, so there is no good way to catch any garbage input.
  # Here we have to rely on the end user to filter out garbage by passing an empty cell.
  blanks <- ids == "&nbsp;"
  out <- paste("https://www.affymetrix.com/LinkServlet?&probeset=",
               ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4UG <- function (ids, ...){
  # Slight modification of Jianhua's original code, replacing error message with
  # empty cells in the table.
  if(is.factor(ids))
    ugs <- strsplit(as.character(ids), "\\.")
  else
    ugs <- strsplit(ids, "\\.")
  badUG <- function(x) if (length(x) != 2 || nchar(x[1]) < 2)
    return(TRUE)
  else return(FALSE)
  bIDs <- sapply(ugs, badUG)
  temp <- vector()
  for( i in seq(along=ids)){
    if(!bIDs[i])
    temp[i] <- paste("http://www.ncbi.nlm.nih.gov/UniGene/clust.cgi?ORG=",
                     ugs[[i]][1], "&CID=", ugs[[i]][2], sep = "")
    else
    temp[i] <- "&nbsp;"
  }
  return(temp)
}

getQuery4LL <- function (ids, ...) {
  .Defunct(msg="The 'll' repository argument is deprecated. Please use 'en'\n.")
}

getQuery4EN <- function (ids, ...){
  ## Here we rely on Entrez Gene IDs being all numeric to filter out garbage
  ## that will result in busted links.
  if(is.factor(ids)){
    options(warn = -1)
    ids <- as.numeric(as.character(ids))
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.character(ids)){
    options(warn = -1)
    ids <- as.numeric(ids)
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.numeric(ids))
    blanks <- is.na(ids)
  out <- paste("http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=Graphics&list_uids=",
               ids, sep = "")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4TR <- function(ids, ...){
    ## No automatic garbage checking. The ath1121501 has accnum values of 'multiple'
    ## that we can convert to blanks however.
    blanks <- ids == "&nbsp;" || ids == "multiple"
    out <- paste("http://www.arabidopsis.org/servlets/Search?type=general&search_action=detail&method=1&name=", ids,
                 "&sub_type=gene", sep="")
    out[blanks] <- "&nbsp;"
    return(out)
}


getQuery4GB <- function (ids, ...){
  # GenBank ids can be either GB or RefSeq, so there is no good way to filter garbage.
  # Again we rely on end user to pass blanks.
  blanks <- ids == "&nbsp;"
  out <- paste("http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=Nucleotide&cmd=search&term=",
               ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}



getQuery4SP <- function(ids, ...){
  ## SwissProt ids are not consistent enough to do any sort of garbage checking
  ## so here we rely on a blank being passed by the end user.
  blanks <- ids == "&nbsp;"
  ## http://www.uniprot.org/uniprot?query=1&AC=P21108
  out <- paste("http://www.uniprot.org/uniprot/", ids, sep="")
  out[blanks] <- "&nbsp;"
  return(out)
}

getQuery4OMIM <- function(ids, ...){
  # Conversion here relies on the assumption that OMIM ids are all numeric
  # so any non-numeric entry must be some sort of garbage that will result in
  # a broken link.
  if(is.factor(ids)){
    options(warn = -1)
    ids <- as.numeric(as.character(ids))
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.character(ids)){
    options(warn = -1)
    ids <- as.numeric(ids)
    options(warn = 0)
    blanks <- is.na(ids)
  }
  if(is.numeric(ids))
    blanks <- is.na(ids)

  out <- paste("http://www.omim.org/entry/", ids, sep="")
  if(!is.null(blanks))
    out[blanks] <- "&nbsp;"

  return(out)

}

getQuery4FB <- function (ids, ...){
  ## Function to build links to flybase for drosophila arrays
  ## Here I rely on the flybase number starting with FBgn
  ## The end user can also pass an empty cell identifier
  if(is.factor(ids))
    fbs <- strsplit(as.character(ids), "FBgn")
  else
    fbs <- strsplit(ids, "FBgn")
  badFB <- function(x) if(length(x) != 2 || nchar(x[1]) != 0)
    return(TRUE) else return(FALSE)
  bIDS <- sapply(fbs, badFB)
  out <- paste("http://flybase.bio.indiana.edu/.bin/fbidq.html?",
                    ids, sep = "")
  out[bIDS] <- "&nbsp;"
  return(out)
}

getQuery4ENSEMBL <- function(ids, ...){
    ## function to build links to Ensembl
    ## Ensembl IDs can start with ENSG, ENSE, ENSP or ENST at the very least

    ids[is.na(ids)] <- "&nbsp;"
    
    if(is.factor(ids))
        enids <- strsplit(as.character(ids), "ENS")
    else
        enids <- strsplit(ids, "ENS")
    badENS <- function(x) if(length(x) !=2 || nchar(x[1]) != 0)
        return(TRUE) else return(FALSE)
    bIDS <- sapply(enids, badENS)
    ##FIXME: should we do some error checking on the species?
    ## it should be e.g., Homo_sapiens
    if(!is.null(list(...)$species))
        species <- list(...)$species
    else
        stop("To make links for Ensembl, you need to pass a 'species' argument.",
             call. = FALSE)
    out <- paste("http://www.ensembl.org/", species, "/Gene/Summary?g=",
                 ids, sep = "")
    out[bIDS] <- "&nbsp;"
    
    out
}

