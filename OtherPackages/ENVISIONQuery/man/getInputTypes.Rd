\name{getInputTypes}
\alias{getInputTypes}
\title{Get the list of available input types for a given tool.}
\usage{getInputTypes(tool, inputType="menu")}
\description{Get the list of available input types for a given tool.}
\details{The input types could be subdivided into the following three groups:
1. The list of IDs (types 'Uniprot ID', 'Affymetrix ID', 'Enquant ID' and 'Protein ID').
2. The character string representing the enfinXML document ('Enfin XML') and
3. The name of the file containing the enfinXML document.
The types belonging to the groups 2 and 3 can be used to construct a pipeline of queries, 
where the output of the given query can be used as an input for the next one}
\value{If inputType is 'all', the character vector of all available input types. If inputType is 
valid input type, the inputType value returned, otherwise returns NULL.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{tool}{The tool handler}
\item{inputType}{Either 'menu' or a valid input type. Default is 'menu'.}}
\examples{#check available input type for a given tool

service<-getService("Reactome");
tool<-getTool(service,"FindPathAdv");
getInputTypes(tool);}
