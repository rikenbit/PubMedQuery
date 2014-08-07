\name{getIdMatchInfo}
\alias{getIdMatchInfo}
\title{Retrieve the set of match pairs from the 'Entry' node of the Enfin xml document.}
\usage{getIdMatchInfo(entry, positiveResultIDs,
    participant.keyword="participant", attribute.keyword="attribute")}
\description{Retrieve the set of match pairs from the 'Entry' node of the Enfin xml document.}
\value{The array consisting of two columns, the first containing the source ID set,
and the second containing the match result}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{entry}{The 'Entry' node of the Enfin xml document.}
\item{positiveResultIDs}{Set of 'ID' nodes of the Enfin xml document which contain the pair match information.}
\item{participant.keyword}{The keyword identifying the match related information within the match top node.
Default is "participant".}
\item{attribute.keyword}{The keyword identifying the attribute related information within the pair match top node.
Default is "attribute".}}
