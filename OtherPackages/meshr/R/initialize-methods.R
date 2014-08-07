##
## Initialize MeSHHyperGParams 
##

setMethod("initialize", "MeSHHyperGParams",
          function(.Object, ...)
{
    .Object <- callNextMethod()
    makeValidParams(.Object)
})

