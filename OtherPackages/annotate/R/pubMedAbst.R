    ## Define the class structure of the pubMedAbst object
    setGeneric("pubMedAbst", function(object)
               standardGeneric("pubMedAbst"))

    setClass("pubMedAbst",
             representation(pmid="character", authors="vector", abstText="character",
             articleTitle="character", journal="character",
             pubDate="character"))

    setMethod("show", "pubMedAbst", function(object) {
      s <- c("An object of class 'pubMedAbst':",
          paste("Title:  ", articleTitle(object)),
          paste("PMID:   ", pmid(object)),
          paste("Authors:", paste(authors(object), collapse=", ")),
          paste("Journal:", journal(object)), 
          paste("Date:   ", pubDate(object)))
      cat(strwrap(s, exdent=5), sep="\n")
    })

    ## Define generics
    if (is.null(getGeneric("authors")))
        setGeneric("authors", function(object)
                   standardGeneric("authors"))

    if (is.null(getGeneric("abstText")))
        setGeneric("abstText", function(object)
                   standardGeneric("abstText"))

    if (is.null(getGeneric("articleTitle")))
        setGeneric("articleTitle", function(object)
                   standardGeneric("articleTitle"))

    if (is.null(getGeneric("journal")))
        setGeneric("journal", function(object)
                   standardGeneric("journal"))

    if (is.null(getGeneric("pubDate")))
        setGeneric("pubDate", function(object)
                   standardGeneric("pubDate"))

    if (is.null(getGeneric("pmid")))
        setGeneric("pmid", function(object)
                   standardGeneric("pmid"))

## Methods
    setMethod("authors", "pubMedAbst", function(object)
              object@authors)
    setMethod("abstText", "pubMedAbst", function(object)
              object@abstText)
    setMethod("articleTitle", "pubMedAbst", function(object)
              object@articleTitle)
    setMethod("journal", "pubMedAbst", function(object)
              object@journal)
    setMethod("pubDate", "pubMedAbst", function(object)
              object@pubDate)
    setMethod("pmid", "pubMedAbst", function(object)
              object@pmid)

buildPubMedAbst <- function(xml) {
    ## Passed in a XML tree detailing a single article
    ## will parse the XML and create a new class

    xmlMedline <- xml["MedlineCitation"][[1]]
    xmlArticle <- xmlMedline["Article"]

    ## Disable error messages, and wrap potential error causers
    ## w/ trys
    options(show.error.messages = FALSE)
    on.exit(options(show.error.messages=TRUE))

    ## Get the PMID
    pmid <- xmlMedline["PMID"][[1]]
    pmid <- try(as.character(xmlChildren(pmid)$text)[6])
    if (inherits(pmid,"try-error") == TRUE) {
        pmid <- "No PMID Provided"
    }

    ## Retrieve Article Title
    articleTitle <- xmlArticle[[1]][["ArticleTitle"]]
    articleTitle <-
    try(as.character(xmlChildren(articleTitle)$text)[6])
    if (inherits(articleTitle,"try-error") == TRUE) {
        articleTitle <- "No Title Provided"
    }

    ## Retrieve the abstract
    abstText <- xmlArticle[[1]]["Abstract"][[1]]["AbstractText"]
    abstText <- try(as.character(xmlChildren(abstText[[1]])$text)[6])
   if (inherits(abstText,"try-error") == TRUE) {
       abstText <- "No Abstract Provided"
   }

    ## Retrieve the date - get the year/month separately and then
    ## join them at the end.  If no month or year provided, subst
    ## "MontH" and "Year" respectively
    pubDateBase <-
        xmlArticle[[1]]["Journal"][[1]]["JournalIssue"][[1]]["PubDate"]
    pubDateMonth <- pubDateBase[[1]]["Month"]
    pubDateMonth <-
        try(as.character(xmlChildren(pubDateMonth[[1]])$text)[6])
    if (inherits(pubDateMonth,"try-error") == TRUE) {
        pubDateMonth <- "Month"
    }
    pubDateYear <- pubDateBase[[1]]["Year"]
    pubDateYear <-
        try(as.character(xmlChildren(pubDateYear[[1]])$text)[6])
    if (inherits(pubDateYear, "try-error") == TRUE) {
        pubDateYear <- "Year"
    }
    ## Join up the date information
    pubDate <- paste(pubDateMonth,pubDateYear)

    ## Get the journal this was published in
    journal <-
        xml["MedlineCitation"][[1]]["MedlineJournalInfo"][[1]]["MedlineTA"]
    journal <- try(as.character(xmlChildren(journal[[1]])$text)[6])
    if (inherits(journal,"try-error") == TRUE) {
        journal <- "No Journal Provided"
    }

    ## Build up a vector of author names, created by assembling the
    ## pieces of each author's name.
    authorList <- xmlArticle[[1]]["AuthorList"]
    authors <- vector()
    numAuthors <- try(length(xmlChildren(authorList[[1]])))
    if (inherits(numAuthors,"try-error") == TRUE) {
        authors[1] <- "No Author Information Provided"
    }
    else {
        for (i in 1:numAuthors) {
            curAuthor <- authorList[[1]][i]
            last <-
                try(as.character(xmlChildren(curAuthor[[1]]["LastName"][[1]])$text)[6])
            if (inherits(last,"try-error") == TRUE) {
                last <- "LastName"
            }

            initial <-
                try(as.character(xmlChildren(curAuthor[[1]]["Initials"][[1]])$text)[6])
            if (inherits(initial,"try-error") == TRUE) {
                initial <- "M"
            }

            authors[i] <- paste(initial,last)
        }
    }

    ## Restore error messages
    options(show.error.messages=TRUE)

    newPMA <- new("pubMedAbst", articleTitle=articleTitle,
                  abstText=abstText, pubDate=pubDate,authors=authors,
                  journal=journal, pmid=pmid)

    return(newPMA)
}

pm.getabst <- function(geneids, basename) {
    pmids <- getPMID(geneids, basename)
    numids <- length(geneids)
    rval <- vector("list", length=numids)
    names(rval) <- geneids
    for(i in 1:numids) {
        pm <- pmids[[i]]
        if( length(pm)==1 && is.na(pm) )
            rval[[i]] <- NA
        else {
            absts <- pubmed(pm)
            a <- xmlRoot(absts)
            numAbst <- length(xmlChildren(a))
            absts <- vector("list", length=numAbst)
            for (j in 1:numAbst)
                absts[[j]] <- buildPubMedAbst(a[[j]])
            rval[[i]] <- absts
        }
    }
    rval
}

pm.abstGrep <- function(pattern, absts, ...)
{
    nabsts <- length(absts)
    rval <- rep(FALSE, nabsts)
    for(i in 1:nabsts) {
        atxt <- abstText(absts[[i]])
        ans <- grep(pattern, atxt, ...)
        if( length(ans) && ans==1 )
            rval[i] <- TRUE
    }
    rval
}

pm.titles <- function (absts) {
     numa <- length(absts)
     rval <- vector("list", length=numa)
     for(j in 1:numa)
         rval[[j]] <- sapply(absts[[j]], function(x) articleTitle(x))
     rval
}

