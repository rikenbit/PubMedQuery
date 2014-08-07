\name{DAVIDQueryLoop}
\alias{DAVIDQueryLoop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Access DAVID multiple times. }
\description{
Make a query larger than DAVID allows in one go, by looping, respecting the limitations imposed by DAVID policies.
}
\usage{
DAVIDQueryLoop(
	idList = unlist(strsplit(strsplit("P31946 P62258 P29360 P42655 Q63631\nP01892 O19619 P06338 P10313 P30444 P30445 P30446 P30514\n",
		" ")[[1]], "\n")),
	idLimit = 100,
	sleepSeconds = 10,
	hitsPerDayLimit = 200,
	verbose = FALSE,
	testMe = FALSE,
	type,
	annot,
	tool,
	graphicMenu = FALSE,
	formatEach = FALSE,
	formatAll = FALSE,
	...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{idList}{ IDs of interest for query. }
	\item{idLimit}{ Published limit of number of ID's to process in one call.}
	\item{sleepSeconds}{ Published minimum time between iterations }
	\item{hitsPerDayLimit}{Published maximum URL calls to the API per day from one address.}
	\item{verbose}{Print out tracking information as the queries are sent. }
	\item{testMe}{ Runs DAVIDQueryLoop with arguments set as follows:  annot=NULL, tool="geneReportFull", type="UNIPROT\_ACCESSION", verbose=TRUE
	}
	\item{type}{See \code{\link{DAVIDQuery}}. }
	\item{annot}{See \code{\link{DAVIDQuery}}. }
	\item{tool}{See \code{\link{DAVIDQuery}}. }
	\item{graphicMenu}{See \code{\link{DAVIDQuery}}. }
	\item{formatEach}{Passed to \code{\link{DAVIDQuery}} as the \code{formatIt} argument. }
	\item{formatAll}{Assembled results are sent to  \code{\link{formatDAVIDResult}}. }
	\item{\dots}{ Other args to be passed to \code{\link{DAVIDQuery}}. }
}
\value{
The results of DAVIDQuery bound together with \code{\link{rbind}}.
Not printed (returned invisibly).
}
\note{
For some choice of the \code{tool} argument, the result returned may differ if \code{idLimit} is changed.  
}
\author{ Roger Day }
\seealso{ \code{\link{DAVIDQuery}} }
\keyword{ database }
