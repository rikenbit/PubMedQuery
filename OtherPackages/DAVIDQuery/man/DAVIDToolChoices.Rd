\name{DAVIDToolChoices}
\alias{DAVIDToolChoices}
\alias{DAVIDTypeChoices}
\alias{DAVIDAnnotChoices}
\alias{DAVIDAffyChipChoices}
\docType{data}
\title{ Choices for the DAVID query  parameters }
\description{
	DAVIDToolChoices, DAVIDTypeChoices, DAVIDAnnotChoices and DAVIDAffyChipChoices are data structures used to construct pick menus, when the corresponding arguments to DAVIDQuery are not provided.
}
\details{
	The source of these lists can be found at
	\url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html#input_list}.
	The DAVIDToolChoices list is hardcoded within the package and includes an additional item representing the DAVID gene ID conversion tool.
	The DAVIDTypeChoices and DAVIDAnnotChoices lists are retrieved from DAVID web services at a run time so the possible future alterations and additions
	to these lists are likely to be handled automatically.
}
\source{
	\url{http://david.abcc.ncifcrf.gov/content.jsp?file=DAVID_API.html#input_list}
}
\seealso{ \code{\link{DAVIDQuery}}, \code{\link{getAnnotationChoices}}, \code{\link{getIdConversionChoices}}, \code{\link{getAffyChipTypes}}}
\keyword{ database }
