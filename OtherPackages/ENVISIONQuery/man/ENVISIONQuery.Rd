\name{ENVISIONQuery}
\alias{ENVISIONQuery}
\title{Launch a query against Envision, a web online query system...}
\usage{ENVISIONQuery(ids=c("1553619_a_at", "1553497_at"), typeName="menu",
    serviceName="menu", toolName="menu", chunk=1000, details=TRUE,
    writeHTML=FALSE, testMe=FALSE,
    graphicMenu=getOption("menu.graphics"), formatIt=TRUE,
    options=list(), filter=list(), compact=TRUE, verbose=FALSE)}
\description{Launch a query against Envision, a web online query system
providing elaborated information for EnCORE services. 
Return the results into an R object.}
\value{The ENVISIONQueryResult structure or NULL if no results were found.}
\note{For a pipeline of services, only unformatted output supported at this time.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{ids}{Depending on input type, IDs for desired objects as a character vector a character string containing
the entire Enfin xml document or a name of an Enfin xml file.}
\item{typeName}{Type of input ids. If 'menu' (default), a menu  is constructed allowing to choose one of the available types.}
\item{serviceName}{The Envision service name or a character vector of service names.
In a latter case system compbines requests into pipeline using the output of current service request as an input for a next one.
If 'menu' (default), the menu is contructed allowing to choose one of the available services.}
\item{toolName}{The tool for a particular envision service. If 'menu' (default) and there is more than one tool available, 
a menu is contructed allowing to choose one of the available tools.}
\item{chunk}{The number of IDs retrieved during a single query session. In case the number of IDs exceeds the chunk size,
the whole ID set is retrieved during multiple query sessions overcoming the potential limitations on a single query session
ID set size. If NA, a single session is used. Default is 1000.}
\item{details}{If TRUE (default), a list of intermediate results is returned; otherwise, just the final query result.
Note: not implemented at this time}
\item{writeHTML}{If TRUE (default is FALSE), write the received intermediate HTML to files.}
\item{testMe}{If TRUE (default is FALSE), assign default values and run.}
\item{graphicMenu}{If TRUE (default is FALSE), use a GUI window for the pick menus.}
\item{formatIt}{If TRUE (default), try to interpret the returned character table and structure the result. 
If false, the character string representing the entire enfinXML file returned by ENVISION.
Note: formatting is implemented only for 'Probe2Uniprot' and 'ID conversion' services at this time.
If false, the character string representing the entire enfinXML file returned by ENVISION.}
\item{options}{The (optional)list each element of which represents the <name,value> pair used to apply
the additional constraints when sending a query to a particular service (maximum number of pathways
for Reactome service as an example)}
\item{filter}{The (optional)list where the name of each element represents the formatted output data frame column
on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the list element
containing a character vector defining the set of values on which the merging (intersection) for a given column
will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.}
\item{compact}{If TRUE and the formatted output (formatIt = TRUE)is represented by a data frame, 
collapses the rows with duplicated match sets but different attributes into a single row with unique match set
and an attribute list separated by comma for each attribute column. Default is TRUE.}
\item{verbose}{If TRUE (default is FALSE), more debugging information is printed.}}
\examples{#### basic ENVISIONQuery request

#convert the Affy probeset IDs to UniProt IDs
res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),
serviceName="ID Conversion",toolName="Affy2Uniprot",typeName="Affymetrix ID");
print(res);

#retrieve the pathways for given Uniprot ID(s)
res<-ENVISIONQuery(ids="P38398",serviceName="Reactome",typeName="Uniprot ID");
print(res[,-3]);

#retrieve protein-protein interactions
res<-ENVISIONQuery(ids="P38398",serviceName="Intact",typeName="Uniprot ID");
print(res[1:5,]);

#convert EnSembl IDs to Uniprot IDs
try({res<-ENVISIONQuery(ids=c("ENSP00000397145","ENSP00000269554"),serviceName="Picr",typeName="Protein ID");})
print(res);

#### ENVISIONQuery request using options and filters

#match Uniprot IDs to EnSembl and TrEMBL
options<-list("enfin-picr-search-database"=c("ENSEMBL_HUMAN","TREMBL"));
try({res<-ENVISIONQuery(ids="P38398",serviceName="Picr",options=options,typeName="Protein ID");})
print(res);

#retrieve the pathways for given Uniprot ID(s)sorting them by coverage
#and calcultating the total protein count
options<-list("enfin-reactome-add-coverage"="true",
"enfin-reactome-sort-by-coverage"="true");
res<-ENVISIONQuery(ids="P38398",serviceName="Reactome",options=options,typeName="Uniprot ID");
print(res[,-3]);

#convert the Affy probeset IDs to UniProt IDs restricting 
#output by micro array type and organism species
filter<-list(Microarray.Platform="affy_hg_u133_plus_2",
organism.species="Homo sapiens");
res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),filter=filter,
serviceName="ID Conversion",toolName="Affy2Uniprot",typeName="Affymetrix ID");
print(res);

#### pipeline of ENVISIONQuery requests. As of recent version, 
#### output formatting for a pipelined request is not supported yet.

#Intact-Reactome cascaded request for UniProt ID(s).   
IntactReactomeXML<-ENVISIONQuery(ids="P38398",
serviceName=c("Intact","Reactome"),typeName="Uniprot ID",verbose=TRUE);
#convert xml text into XMLDocument
#using XML package for further exploring
if(!is.null(IntactReactomeXML)){
xmlDoc<-xmlTreeParse(IntactReactomeXML,useInternalNodes = TRUE, asText=TRUE);
class(xmlDoc);
}

#### interactive ENVISIONQuery requests
\dontrun{
res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),filter=filter,
serviceName="menu",toolName="menu",typeName="menu");
print(res);
}}
