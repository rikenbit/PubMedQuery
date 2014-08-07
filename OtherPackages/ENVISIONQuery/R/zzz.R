.First.lib <- function(libname, pkgname) {
	### .First.lib is NOT RECOMMENDED for packages with namespace, but it works.
	data(ENVISIONServices);
}

.onAttach = function(libname, pkgname) {

getJavaVersion<-function(){
 	version=.jcall("java.lang.System","S","getProperty","java.version");
	return(version);
}


jpackage<-function (name, jars = "*", java.dir="java",morePaths = "", nativeLibrary = FALSE, 
    lib.loc = NULL) 
{
    if (!.jniInitialized) 
        .jinit()
    classes <- system.file(java.dir, package = name, lib.loc = lib.loc)
    if (nchar(classes)) {
        .jaddClassPath(classes)
        if (length(jars)) {
            if (length(jars) == 1 && jars == "*") {
                jars <- grep(".*\\.jar", list.files(classes, 
                  full.names = TRUE), TRUE, value = TRUE)
                if (length(jars)) 
                  .jaddClassPath(jars)
            }
            else .jaddClassPath(paste(classes, jars, sep = .Platform$file.sep))
        }
    }
    if (any(nchar(morePaths))) {
        cl <- as.character(morePaths)
        cl <- cl[nchar(cl) > 0]
        .jaddClassPath(cl)
    }
    if (is.logical(nativeLibrary)) {
        if (nativeLibrary) {
            libs <- "libs"
            if (nchar(.Platform$r_arch)) 
                lib <- file.path("libs", .Platform$r_arch)
            lib <- system.file(libs, paste(name, .Platform$dynlib.ext, 
                sep = ""), package = name, lib.loc = lib.loc)
            if (nchar(lib)) 
                .jaddLibrary(name, lib)
            else warning("Native library for `", name, "' could not be found.")
        }
    }
    else {
        .jaddLibrary(name, nativeLibrary)
    }
    invisible(TRUE)
}

	jpackage(pkgname,java.dir="jar");

	javaVersion<-getJavaVersion();

	if(javaVersion<"1.6"){
		msg<-paste("Your R is configured to use Java version ",javaVersion,".\n",
			"The ",pkgname, " package requires R to be configured with Java 1.6 or higher.\n",
			"Please install the correct Java version and/or reconfigure R and try again.\n",
			 sep="");
		#warning(msg,immediate. = TRUE);
		stop(msg);
	}
		

	desc <- packageDescription(pkgname)
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	cat("This is ", pkgname, " ", desc$Version, " ", desc$Date, "\n")
	return(invisible(NULL))
}

