\name{formatIdMap}
\alias{formatIdMap}
\title{Convert the Enfin xml ID conversion results file into the pair match data frame...}
\usage{formatIdMap(xml, filter=list(), compact=TRUE, verbose=TRUE)}
\description{Convert the Enfin xml ID conversion results file into the pair match data frame}
\value{The data frame consisting of two groups of columns: 
the first containing the ID match set (i.e. 'Affymetrix GeneChip', 'Uniprot' etc.),
and the second containing the attributes for a particular match ('organism.species', 'Microarray.Platform' etc.).}
\keyword{internal}
\author{Alex Lisovich, Roger Day}
\arguments{\item{xml}{Character string representing an Enfin xml file}
\item{filter}{The list where the name of each element represents an output data frame column
on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
containing a character vector defining the set of values on which the merging (intersection) for a given column
will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.}
\item{compact}{If TRUE, collapses the rows with duplicated match sets but different attributes 
into a single row with unique match set and an attribute list separated by comma for each attribute column. Default is TRUE.}
\item{verbose}{if TRUE enables diagnostic messages. Default is FALSE.}}
