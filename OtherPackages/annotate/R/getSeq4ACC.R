getGI <- function(accNum){
    # Get the gi based on the Accession number
    gi <- readLines(paste("http://www.ncbi.nlm.nih.gov/entrez/",
                          "query.fcgi?db=Nucleotide&cmd=search&term=",
                          accNum, sep = ""))
    gi <- gsub(paste(".*gi\\|([0-9]+)\\|[a-zA-Z0-9]+\\|", accNum,
                     ".*", sep = ""), "\\1",
               gi[grep(paste("gi\\|.*\\|[a-zA-Z0-9]+\\|", accNum, ".*",
                                    sep = ""), gi)])
    if(length(gi) == 0){
        stop(paste("Can't obtain a gi number for", accNum))
    }else{
        return(gi)
    }
}


getSEQ <- function(gi){

    ## Old stuff left just in case NCBI changes things back on us (10/15/10)
    ## seq <- readLines(paste("http://www.ncbi.nlm.nih.gov/entrez/batchseq.cgi?",
    ##              "cmd=&txt=on&save=&cfm=&list_uids=", gi, "&",
    ##              "db=nucleotide&extrafeat=16&term=&view=fasta&",
    ##              "dispmax=20&SendTo=t&__from=&__to=&__strand=", sep = ""))

    seq <- readLines(paste("http://www.ncbi.nlm.nih.gov/entrez/eutils/",
                 "efetch.fcgi?db=nucleotide&rettype=fasta&id=",gi,
                  sep = ""))

    if(length(seq) == 0){
        stop("Failed to extract the sequence")
    }else{
        return(paste(seq[2:length(seq)], sep = "", collapse = ""))
    }
}

## better use reverseComplement from Biostrings

#revBase <- function(x){
#  tot <- which(x == "A")
#  tog <- which(x == "C")
#  toa <- which(x == "T")
#  toc <- which(x == "G")
#  x[tot] <- "T"
#  x[toa] <- "A"
#  x[toc] <- "C"
#  x[tog] <- "G"
#  x
# }

#revString <- function(x)
#   sapply(lapply(lapply(strsplit(x, NULL), rev), revBase), paste, collapse="")

