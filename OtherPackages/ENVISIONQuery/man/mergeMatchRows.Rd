\name{mergeMatchRows}
\alias{mergeMatchRows}
\title{Merge a set of rows with non-unique values in a given set of columns.}
\usage{mergeMatchRows(matchTable, matchInds)}
\description{Merge a set of rows with non-unique values in a given set of columns.}
\details{Collapse the rows that have the same values in for a given set of columns into a single row,
merging the rest of the values into the list of unique values separated by comma, one list per column.}
\value{Data frame conaining the unique set of values in matchInds column set.}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{matchTable}{Data frame on which merging is performed.}
\item{matchInds}{The column indexes which should contain the unique set of values after merging.}}
