.buildAnnotateOpts <- function() {
    if (is.null(getOption("BioC"))) {
        BioC <- list()
        class(BioC) <- "BioCOptions"
        options("BioC"=BioC)
    }

    Annotate <- list()
    class(Annotate) <- "BioCPkg"
    Annotate$urls <- list( ncbi = "http://www.ncbi.nlm.nih.gov/",
          data="http://www.bioconductor.org/datafiles/annotate/")

    BioC <- getOption("BioC")
    BioC$annotate <- Annotate
    options("BioC"=BioC)
}

.onLoad <- function(libname, pkgname) {
    .setDefaultRepositories()
    .buildAnnotateOpts()
    if(.Platform$OS.type == "windows" && interactive()
        && .Platform$GUI ==  "Rgui"){
        Biobase::addVigs2WinMenu("annotate")
    }
}

.repositories <- new.env(parent = emptyenv())

.setDefaultRepositories<- function() {
    setRepository("ug", getQuery4UG)
    setRepository("affy", getQuery4Affy)
    setRepository("gb", getQuery4GB)
    setRepository("sp", getQuery4SP)
    setRepository("omim", getQuery4OMIM)
    setRepository("fb", getQuery4FB)
    setRepository("en", getQuery4EN)
    setRepository("tr", getQuery4TR)
    setRepository("go", getQuery4GO)
    setRepository("ens", getQuery4ENSEMBL)
}
