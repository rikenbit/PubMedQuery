getPMInfo <- function(x) { 
#
# getMLInfo: get medline-related info from a pubmed xml DOM tree
# works with result of Bioconductor annotate::pubmed function
#
# tagVals: utility function for grabbing vector of
# tag values from any DOM tree
#
tagVals <- function(x,tag) { 
 tagNames <- function() {
  store <- character(0)
  add <- function(x) {
   if(inherits(x, "XMLNode") & xmlName(x) == tag) {
     store <<- c(store, xmlValue(x))
    }
   x
   }
  return(list(add=add, tagVals = function() {return(store)}))
  }
 h <- tagNames()
 xmlDOMApply(x, h$add) 
 h$tagVals()
}
#
# here's the main body of getMLInfo.  the function 'arts' creates
# a closure for collecting data on articles in the document returned
# by the pubmed function.  the 'add' element of the closure
# adds information to various local vectors and lists as xmlDOMApply
# walks through the tree.
#
 if (!inherits(x, "XMLDocument")) stop("only applies to XMLDocument")
 arts <- function() {
  pmarts <- list()
  pmart <- list()
  jinfo <- character(0)
  alist <- character(0)
  chemlist <- character(0)
  cura <- character(0)
  cur <- 1
  add <- function(x) {
   if(inherits(x, "XMLNode") & xmlName(x) == "ArticleTitle") {
     pmart[["title"]] <<- xmlValue(x)
    }
   if(inherits(x, "XMLNode") & xmlName(x) == "MedlineTA") {
     pmart[["MedlineTA"]] <<- xmlValue(x)
    }
   if(inherits(x, "XMLNode") & xmlName(x) == "AbstractText") {
     pmart[["abstract"]] <<- xmlValue(x)
    }
   if(inherits(x, "XMLNode") & xmlName(x) == "PubmedArticle") {
     id <- xmlValue(getNodeSet(x, "/PubmedArticle/*/PMID")[[1L]])
     pmarts[[id]] <<- pmart
     pmart <<- list()
     cur  <<- cur+1
    }
#
# deal with journal info
# this is an ugly part because tags like Year or Volume can occur in
# different contexts.  Need to know something about the parent.
# but we don't want to assume too much about sequence of nodes
#
   if (inherits(x, "XMLNode") & xmlName(x) == "ISSN") {
     jinfo <<- c(jinfo,ISSN=xmlValue(x))
   }
   if (inherits(x, "XMLNode") & xmlName(x) == "JournalIssue") {
    jikids <- xmlChildren(x)
    for (i in seq_along(jikids))
     {
     if (xmlName(jikids[[i]]) == "Volume")
       jinfo <<- c(jinfo,vol=xmlValue(jikids[[i]]))
     else if (xmlName(jikids[[i]]) == "Issue")
       jinfo <<- c(jinfo,iss=xmlValue(jikids[[i]]))
     else if (xmlName(jikids[[i]]) == "PubDate")
       {
       Year <- tagVals(jikids[[i]],"Year")
       Month <- tagVals(jikids[[i]],"Month")
       Day <- tagVals(jikids[[i]],"Day")
       jinfo <<- c(jinfo,year=Year,month=Month,day=Day)
       }
     }
     pmart[["JrnlInfo"]] <<- jinfo
     jinfo <<- character(0)
   }
#
# deal with author info
#
   if (inherits(x, "XMLNode") & xmlName(x) =="AuthorList") {
     pmart[["authors"]] <<- alist
     alist <<- character(0)
   }
   if (inherits(x, "XMLNode") & xmlName(x) =="Author") {
     alist <<- c(alist,cura)
     cura <<- character(0)
   }
   if (inherits(x, "XMLNode") & xmlName(x) =="LastName") {
     cura <<- paste(cura,last=xmlValue(x),sep="") 
   }
#   if (inherits(x, "XMLNode") & xmlName(x) =="ForeName") {
#     cura <<- paste(cura,fore=xmlValue(x)) 
#   }
   if (inherits(x, "XMLNode") & xmlName(x) =="Initials") {
     cura <<- paste(cura,inits=xmlValue(x)) 
   }
#
# deal with substance info
#
   if (inherits(x, "XMLNode") & xmlName(x) =="ChemicalList") {
     pmart[["chemlist"]] <<- chemlist
     chemlist <<- character(0)
   }
   if (inherits(x, "XMLNode") & xmlName(x) =="NameOfSubstance") {
     chemlist <<- c(chemlist,xmlValue(x))
   }
   x
   }
  return(list(add=add, arts = function() {return(pmarts)}))
  }
 h <- arts()
 xmlDOMApply(xmlRoot(x), h$add) 
 h$arts()
}
