\name{getPicrMatchInfo}
\alias{getPicrMatchInfo}
\title{Retrieve PICR match info from the 'Entry' node of the Enfin xml document.}
\usage{getPicrMatchInfo(entry, positiveResultIDs)}
\description{Retrieve PICR match info from the 'Entry' node of the Enfin xml document.}
\value{The array consisting of three columns, the first containing the source ID set,
and the second containing the ID set the source IDs are interacting with and the thirfd
containing IntAct interaction ID.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{positiveResultIDs}{Set of 'ID' nodes of the Enfin xml document which contain the pair match information.}}
