pmid2MIAME = function (pmid) 
{
#
# we find that some abstracts are text values of <Abstract>
# and some of <Abstract><AbstractText> ...
#
# need to think about validity checking here .. DTD?
#
    require("XML") || stop("need the XML package for this function")
    x = pubmed(pmid)
    rr = xmlRoot(x)
    top = xmlChildren(rr)
    pmart = top[["PubmedArticle"]]
    cit = xmlChildren(pmart)[["MedlineCitation"]]
    art = cit[["Article"]]
    cart = xmlChildren(art)
    title = xmlValue(cart[["ArticleTitle"]])
    abst = xmlValue(cart[["Abstract"]])
    if (is.null(abst)) {
	caa = xmlChildren(cart[["Abstract"]])
        abst = xmlValue(caa[["AbstractText"]])
    }
    if (is.null(abst)) abst = ""
    aff = xmlValue(cart[["Affiliation"]])
    an = cart[["AuthorList"]]
    last = xmlValue(xmlChildren(an[[1]])[["LastName"]])
    ini = xmlValue(xmlChildren(an[[1]])[["Initials"]])
    new("MIAME", name=paste(last,ini,collapse=", "), lab = aff, title = title, abstract = abst, pubMedIds = pmid)
}
