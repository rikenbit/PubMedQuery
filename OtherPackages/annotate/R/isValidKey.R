##Helper function for schema checking:
.defineBaseSelectSQL <- function(schema){  
  ##schema <- dbmeta(conn, "DBSCHEMA")
  ##centralID <- dbmeta(conn, "CENTRALID")
    if(schema == "YEAST_DB"){
        sql <- "select distinct systematic_name from sgd where systematic_name != 'NA';"
    }else if(length(grep("CHIP_DB$", schema))==1 ){  #All chip packages have a probes table with probe_ids
        sql <- "select distinct probe_id from probes;"
    }else if(length(grep("_DB$", schema))==1 && length(grep("CHIP_DB$", schema))==0){
        sql <- "select distinct gene_id from genes;"
    }else{
        stop("Unidentified database schema.  Cannot find central table.  May need to add schema options to isValidKey().")
    }    
    return(sql)
}

##Given a list of IDs and a package, are these IDs valid primary IDs for this package?
isValidKey <- function(ids, pkg){
    ##argument checking
    if(!is.character(ids)) stop("'ids' must be a character vector of IDs that you wish to validate")    
    ##access the DB, get the primary IDs, and then test if they are in your list of ids
    require(paste(pkg, ".db",sep=""),character.only = TRUE)
    conn <- do.call(paste(pkg, "_dbconn", sep=""), list())    
    schema <- dbmeta(conn, "DBSCHEMA")
    sql <- .defineBaseSelectSQL(schema)
    res <- dbGetQuery(conn, sql)
    res <- as.vector(res[,1])#slice to grab result which will always be a single column (based on the sql queries)
    return(ids %in% res)
}

##Given a package, what are all the unique valid primary IDs for this package?
allValidKeys <- function(pkg){
    ##access the DB and get all the primary IDs, (unique constraint already on the field being sought)
    require(paste(pkg, ".db",sep=""),character.only = TRUE)
    conn <- do.call(paste(pkg, "_dbconn", sep=""), list())
    schema <- dbmeta(conn, "DBSCHEMA")
    sql <- .defineBaseSelectSQL(schema)    
    res <- dbGetQuery(conn, sql)
    res <- as.vector(res[,1])#slice to grab result which will always be a single column (based on the sql queries)
    return(res)
}


##Given a list of gene symbols, return the primary ID (or probe if its a chip package) that should be used.
##If there was a symbol or ID in the original list that we don't have a better ID for, keep the original symbol...
##Because of the many to one nature of probes to genes, it will NOT be possible to support CHIP packages with this function.
updateSymbolsToValidKeys = function(symbols, pkg) {
    #argument checking
    if(!is.character(symbols)) stop("'symbols' must be a character vector of gene symbols that you wish to translate to the primary ID of the package")
    require(paste(pkg, ".db",sep=""),character.only = TRUE)

    ##Check the schema
    conn <- do.call(paste(pkg, "_dbconn", sep=""), list())
    schema <- dbmeta(conn, "DBSCHEMA")

    ##'pkg' cannot be a chip package.
    if(length(grep("CHIP_DB$", schema))>=1){
        stop("Because of the many to many relationship that can exist between probes and IDs, this function can only work with the organism level packages which can ensure that there is only one most valid ID per gene symbol.")
    }    
    
    ##Do the right thing depending on what type of package this is.
    if(length(grep("^YEAST", schema))>=1){
        ##if its yeast...
        rr1 = mappedRkeys(eval(parse(text=paste(pkg, "ALIAS", sep=""))))
        r2 = revmap(eval(parse(text=paste(pkg, "ALIAS", sep=""))))
    }else if(length(grep("^ARABIDOPSIS", schema))>=1){  
        stop("Sorry, but the Arabidopsis packages do not have alias information at this time.")
    }else if(length(grep("^MALARIA", schema))>=1){#MALARIA packages are not entrez gene based
        r2 = eval(parse(text=paste(pkg, "ALIAS2ORF", sep="")))
        rr1 = mappedRkeys(revmap(eval(parse(text=paste(pkg, "ALIAS2ORF", sep="")))))        
    }else{  #so far everything other than yeast and malaria should have reversed alias map and eg base
        ##so if its something other than yeast we need to do this...
        r2 = eval(parse(text=paste(pkg, "ALIAS2EG", sep="")))
        rr1 = mappedRkeys(revmap(eval(parse(text=paste(pkg, "ALIAS2EG", sep="")))))        
    }

    mA = match(symbols, rr1)
    wh = rr1[mA[!is.na(mA)]]
    
    mB = unlist(mget(wh, r2))  
    symbols[match(names(mB), symbols)] = mB
    return(symbols)
}



## ##TEST examples:
## fu <- c("15S_rRNA_2","21S_rRNA_4","15S_rRNA")
## isValidKey(fu, "org.Sc.sgd")
## updateSymbolsToValidKeys(fu, "org.Sc.sgd")

## sna <- c("1769325_at","altSymbol")
## isValidKey(sna, "yeast2")

## bar <- c("MAPK11","P38B","FLJ45465", "altSymbol")
## isValidKey(bar, "org.Hs.eg")
## updateSymbolsToValidKeys(bar, "org.Hs.eg")

## foo <- c("1396.pre-tRNA-Met-1", "1396.t00553", "altSymbol")
## updateSymbolsToValidKeys(foo, "org.Pf.plasmo")
## isValidKey(foo, "org.Pf.plasmo")
