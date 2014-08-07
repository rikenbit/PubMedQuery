
# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readIDNAcc <- function(GEOAccNum, url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?"){
    temp <- readGEOAnn(GEOAccNum, url)
    return(temp[,c("ID", "GB_ACC")])
}

getSAGEGPL <- function(organism = "Homo sapiens",
                       enzyme = c("NlaIII", "Sau3A")){

    enzyme <- match.arg(enzyme)

    SAGEFiles <- getSAGEFileInfo()
    return(SAGEFiles[SAGEFiles[,2] == organism & SAGEFiles[,3] ==
                       enzyme, 1])
}

getSAGEFileInfo <- function(url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/browse.cgi?view=platforms&prtype=SAGE&dtype=SAGE"){
    temp <- readUrl(url)
    # Get the GPL number, organism, and enzyme type
    temp <- matrix(temp[grep("<TD", temp)], ncol = 8,
                   byrow = TRUE)[,c(1, 5, 6)]
    temp[,1] <- gsub(".*>(GPL.*)</a>", "\\1", temp[,1])
    temp[,2] <- gsub(".*>(.*)</a>", "\\1", temp[,2])
    temp[,3] <- gsub(".*>(.*):.*</TD>", "\\1", temp[,3])

    return(temp)
}


# Query the GEO database. url is the common CGI scrip at GEO
# and GEOAccNum is the GEO accession number representing a file in the
# database
readGEOAnn <- function(GEOAccNum, url =
                       "http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?"){

    temp <- readUrl(paste(url, "acc=", GEOAccNum,
                   "&view=data&form=text&targ=self", sep = ""))
    # Remove the header lines that come with the file
    temp <- temp[grep("\t", temp)]
    # Add NAs to lines with no value for the last column
    temp <- strsplit(gsub("\t$", "\tNA", temp), "\t")
    # Convert to a matrix
    temp <- t(sapply(temp, unlist))
    # The first row is for column name. Remove it.
    colnames(temp) <- temp[1,]
    return(temp[-1,])
}

# Read from GEO and map GEO accession numbers to array names.
getGPLNames <- function(url =
                        "http://www.ncbi.nlm.nih.gov/geo/query/browse.cgi?"){
    temp <- readUrl(paste(url,
                      "view=platforms&prtype=nucleotide&dtype=commercial",
                      sep = ""))

    temp <- temp[grep("<TD", temp)]
    temp <- matrix(temp, ncol = 8, byrow = TRUE)

    chipNames <- gsub(".*>(.*)</TD>$", "\\1", temp[,6])
    names(chipNames) <- gsub(".*>(.*)</a>$", "\\1", temp[,1])

    return(chipNames)
}

readUrl <- function(url){
    options(show.error.messages = FALSE)
    con <- try(url(url, open = "r"))
    options(show.error.messages = TRUE)
    if(inherits(con, "try-error")){
        stop(paste("Can't connect to url", url))
    }
    temp <- readLines(con)
    close(con)
    return(temp)
}
