serializeEnv <- function(env, fname) {
 if (!is.character(fname))
     stop("conn should be a character name of file for storage")

 if (is.character(env)) {
     cmd <- paste("envList <- as.list(", env, ")")
     eval(parse(text=cmd))
 }
 else if (is.environment(env))
     envList <- as.list(env)
 else
     stop("invalid 'env' argument")

 keys <- names(envList)

 outFile <- gzfile(fname)
 open(outFile, open="wb")

 cat("<?xml version=\"1.0\"?>\n",
     "<values xmlns:bt=\"http://www.bioconductor.org/RGDBM\">",
     file=outFile)
 for (i in seq(along=envList)) {
     cat("\n<entry>\n\t<key>\n\t\t<![CDATA[",
          keys[i], "]]>",
         "\n\t\t</key>\n\t\t<value>\n\t\t<![CDATA[",
         serialize(envList[[i]], NULL, ascii=TRUE),
         "]]>\n\t\t</value>\n\t</entry>", sep="", file=outFile, append=TRUE)
 }
 cat("\n</values>", file=outFile, append=TRUE)

 close(outFile)
}

serializeDataPkgEnvs <- function(pkgDir) {
    pkg <- basename(pkgDir)
    require(pkg, character.only=TRUE) || stop("data package ",
                 pkg, " not installed")

    cDir <- getwd()
    on.exit(setwd(cDir), add=TRUE)
    setwd(pkgDir)

    if (! file.exists("inst"))
        if (!dir.create("inst"))
            stop("Failed to create inst for ", pkgDir)
    if (! file.exists(file.path("inst", "gdbm")))
        if (!dir.create(file.path("inst", "gdbm")))
            stop("Failed to create inst/gdbm for ", pkgDir)
    setwd("inst/gdbm")

    dataSets <- ls(paste("package", pkg, sep=":"))
    if (length(dataSets) == 0)
        return(0)

    for (i in seq(along=dataSets)) {
        cmd <- paste("is.environment(", dataSets[i], ")")
        if (eval(parse(text=cmd))) {
            print(paste("Converting", dataSets[i]))
            serializeEnv(dataSets[i], paste(dataSets[i], ".xml.gz", sep=""))
        }
        else
            print(paste(dataSets[i], "is not an environment, skipping."))
    }
    NULL
}
