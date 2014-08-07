.First.lib <- function(libname, pkgname) {
	### .First.lib is NOT RECOMMENDED for packages with namespace, but it works.
	data(DAVIDURLBase)
	data(DAVIDToolChoices)
	data(DAVIDTypeChoices)
	data(DAVIDAnnotChoices)
	data(DAVIDAffyChipChoices)
	data(idExampleList)
}
.onAttach = function(libname, pkgname) {
	desc <- packageDescription("DAVIDQuery")
	DQdate <-  desc$Date
	DQVersion =  desc$Version
	cat("This is DAVIDQuery Version ", DQVersion , " ", DQdate, "\n")
	return(invisible(NULL))
}
