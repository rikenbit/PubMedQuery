\name{formatReactome}
\alias{formatReactome}
\title{Convert the EnXML PICR  service results file into the protein interaction data frame...}
\usage{formatReactome(xml, filter=list(), compact=TRUE, verbose=TRUE)}
\description{Convert the EnXML PICR  service results file into the protein interaction data frame}
\value{The data frame with at least three columns, containing the source ID set, 
the pathway description and the link to Reactome web page for a given pathway correspondingly. 
If 'enfin-reactome-add-coverage' option is set to 'true' then two extra columns
are added: for pathway coverage number, and  total protein count per pathway.}
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
