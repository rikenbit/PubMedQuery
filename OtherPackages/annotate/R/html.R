makeAnchor <- function(link, title, toMain=FALSE) {
    ## Takes a vector of links and a vector of titles -
    ## returns a vector of anchors.

    ## !! Should allow links to be URL objects as well as strings
    out <- paste("<A HREF=",link,sep="")
    if (toMain)
        out <- paste(out," target=\"main\"", sep="")
    out <- paste(out,">",title,"</A>",sep="")
    out
}


    ## A simple class to represent a basic "HTML Page", currently
    ## being naively stored as a block of text.
    setClass("HTMLPage", representation(fileName="character",
                                        pageText="character",
                                        pageTitle="character"))
    if (is.null(getGeneric("fileName")))
        setGeneric("fileName", function(object, ...)
                   standardGeneric("fileName"))
    setMethod("fileName", "HTMLPage", function(object, ...)
              object@fileName)

    if (is.null(getGeneric("pageText")))
        setGeneric("pageText", function(object, ...)
                   standardGeneric("pageText"))

    setMethod("pageText", "HTMLPage", function(object, ...)
              object@pageText)

    if (is.null(getGeneric("pageTitle")))
        setGeneric("pageTitle", function(object, ...)
                   standardGeneric("pageTitle"))
    setMethod("pageTitle", "HTMLPage", function(object, ...)
              object@pageTitle)

    setMethod("show","HTMLPage", function(object) print(pageText(object)))

    if (is.null(getGeneric("toFile")))
        setGeneric("toFile", function(object, ...)
                   standardGeneric("toFile"))
    setMethod("toFile", "HTMLPage", function(object, ...) {
        cat(pageText(object), file=fileName(object))
    })

    ## Defines a basic framed page.  We're using 3 frames, a top
    ## banner, a side navigation bar and a main page, much like the
    ## bioconductor website.  The object also has it's own HTML page
    ## associated with it via HTMLPage inheritance.
    setClass("FramedHTMLPage", representation(topPage="HTMLPage",
                                              sidePage="HTMLPage",
                                              mainPage="HTMLPage"),
             contains="HTMLPage")

    if (is.null(getGeneric("topPage")))
        setGeneric("topPage", function(object, ...)
                   standardGeneric("topPage"))
    setMethod("topPage", "FramedHTMLPage", function(object, ...)
              object@topPage)

    if (is.null(getGeneric("sidePage")))
        setGeneric("sidePage", function(object, ...)
                   standardGeneric("sidePage"))
    setMethod("sidePage", "FramedHTMLPage", function(object, ...)
              object@sidePage)

    if (is.null(getGeneric("mainPage")))
        setGeneric("mainPage", function(object, ...)
                   standardGeneric("mainPage"))
    setMethod("mainPage", "FramedHTMLPage", function(object, ...)
              object@mainPage)

    setMethod("toFile", "FramedHTMLPage", function(object, ...) {
        toFile(topPage(object))
        toFile(sidePage(object))
        toFile(mainPage(object))

        ## Is there a way to force a call to HTMLPage's 'toFile' here?
        cat(pageText(object), file=fileName(object))
    })

    setMethod("initialize", "FramedHTMLPage",
              function(.Object, topPage=new("HTMLPage"),
                       sidePage=new("HTMLPage"),
                       mainPage=new("HTMLPage"),
                       fileName=new("character"),
                       pageTitle=new("character")) {
                  .Object@pageTitle <- pageTitle
                  .Object@fileName <- fileName
                  .Object@topPage <- topPage
                  .Object@sidePage <- sidePage
                  .Object@mainPage <- mainPage
                  topName <- fileName(topPage(.Object))
                  sideName <- fileName(sidePage(.Object))
                  mainName <- fileName(mainPage(.Object))

                  out <- paste("<HTML>","<HEAD>",sep="\n")
                  t <- paste("<TITLE>",pageTitle(.Object),"</TITLE>")
                  out <- paste(out,t,"</HEAD>",
                               "<frameset rows=\"70,*\" border =\" 0\" frameborder=\" no\" framespacing =\" 0\">",
                               "  <frame name=\"banner\" scrolling=\"no\" noresize target=\"contents\" src=\"",topName,"\" marginwidth=\"0\" marginheight=\"0\">",
                               "  <frameset cols=\"250,*\">",
                               "    <frame name=\"contents\" target=\"main\" src=\"",sideName,"\" marginwidth=\"10\" marginheight=\"10\" scrolling=\"auto\" noresize>",
                               "    <frame name=\"main\" scrolling=\"auto\" noresize src=\"",mainName,"\" marginwidth =\" 10\" marginheight =\" 10\" target=\"_self\">",
                               "  </frameset>","  <noframes>","  <body>","",
                               "  <p>This page uses frames, but your browser doesn't support them.</p>",
                               "", "  </body>","  </noframes>",
                               "</frameset>","</html>",
                               sep="\n")
                  .Object@pageText <- out
                  .Object
              })
