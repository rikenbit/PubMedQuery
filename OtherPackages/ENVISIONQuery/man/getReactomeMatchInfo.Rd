\name{getReactomeMatchInfo}
\alias{getReactomeMatchInfo}
\title{Retrieve Reactome pathway match info from the 'Entry' node of the Enfin xml document.}
\usage{getReactomeMatchInfo(entry, positiveResultIDs)}
\description{Retrieve Reactome pathway match info from the 'Entry' node of the Enfin xml document.}
\value{The array with at least three columns, the first containing the source ID set, the second containing
the pathway description and the third containing the link to Reactome web page for a given pathway. 
If 'enfin-reactome-add-coverage' option is set to 'true' then two extra columns
are added: the first for pathway coverage number, and the second for the total protein count per pathway.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{positiveResultIDs}{Set of 'ID' nodes of the Enfin xml document which contain the pair match information.}}
