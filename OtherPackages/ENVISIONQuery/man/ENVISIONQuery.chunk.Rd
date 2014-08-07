\name{ENVISIONQuery.chunk}
\alias{ENVISIONQuery.chunk}
\title{Retrieves a set of unique matches for a given ID list.}
\usage{ENVISIONQuery.chunk(ids, utilityTool, service, tool, formatIt,
    options=list(), filter=list(), compact=TRUE, writeHTML=FALSE,
    nchunk=NA, verbose=FALSE, ...)}
\description{Retrieves a set of unique matches for a given ID list.}
\value{The ENVISIONQueryResult structure}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{ids}{Character vector representing the ID list.}
\item{utilityTool}{The tool used to convert the ID list into the input Enfin xml file.}
\item{service}{Service handler}
\item{tool}{The tool used to perform the conversion}
\item{formatIt}{If TRUE (default), try to interpret the returned character table and structure the result. 
If false, the character string representing the entire enfinXML file returned by ENVISION.}
\item{options}{The (optional)list each element of which represents the <name,value> pair used to apply
the additional constraints when sending a query to a particular service (maximum number of pathways
for Reactome service as an example)}
\item{filter}{The list where the name of each element represents an output data frame column
on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
containing a character vector defining the set of values on which the merging (intersection) for a given column
will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.}
\item{compact}{If TRUE and the formatted output (formatIt = TRUE)is represented by a data frame, 
collapses the rows with duplicated match sets but different attributes into a single row with unique match set
and an attribute list separated by comma for each attribute column. Default is TRUE.}
\item{writeHTML}{If TRUE, write the received intermediate HTML to file. Default is FALSE.}
\item{nchunk}{Chunk number. If not NA and writeHTML is TRUE the input and output xml file names are appended 
with chunk number. Default is NA.}
\item{verbose}{If TRUE, enables diagnostic messages. Default is FALSE.}
\item{...}{additional arguments.}}
