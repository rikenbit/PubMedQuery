### R code from vignette source 'ENVISIONQuery.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: chunk1
###################################################
library(ENVISIONQuery);
#check available services
getServiceNames();
#check available tools for a given service
service<-getService("ID Conversion");
getToolNames(service);
#check available input type for a given tool
service<-getService("Reactome");
tool<-getTool(service,"FindPathAdv");
getInputTypes(tool);
#getAvailable options for a given service
getServiceOptions("Picr");


###################################################
### code chunk number 2: chunk2
###################################################
#convert the Affy probeset IDs to Uniprot IDs
res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),
serviceName="ID Conversion",toolName="Affy2Uniprot",typeName="Affymetrix ID");
print(res);
#retrieve the pathways for given Uniprot ID(s)
res<-ENVISIONQuery(ids="P38398",serviceName="Reactome",typeName="Uniprot ID");
print(res[1:5,]);
#retrieve protein-protein interactions
res<-ENVISIONQuery(ids="P38398",serviceName="Intact",typeName="Uniprot ID");
print(res[1:5,]);
#convert EnSembl IDs to Uniprot IDs
res<-ENVISIONQuery(ids=c("ENSP00000397145","ENSP00000269554"),
serviceName="Picr",typeName="Protein ID");
print(res);


###################################################
### code chunk number 3: chunk3
###################################################
#match Uniprot IDs to EnSembl and TrEMBL
options<-list("enfin-picr-search-database"=c("ENSEMBL_HUMAN","TREMBL"));
res<-ENVISIONQuery(ids="P38398",options=options,
serviceName="Picr",typeName="Protein ID");
print(res);
#retrieve the pathways for given Uniprot ID(s)sorting them by coverage
#and calculating the total protein count
options<-list("enfin-reactome-add-coverage"="true",
"enfin-reactome-sort-by-coverage"="true");
res<-ENVISIONQuery(ids="P38398",options=options,
serviceName="Reactome",typeName="Uniprot ID");
print(res[1:5,]);
#convert the Affy probeset IDs to UniProt IDs restricting
#output by micro array type and organism species
filter<-list(Microarray.Platform="affy_hg_u133_plus_2",
organism.species="Homo sapiens");
res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),
filter=filter, serviceName="ID Conversion",
toolName="Affy2Uniprot",typeName="Affymetrix ID");
print(res);


###################################################
### code chunk number 4: chunk4
###################################################
#Intact-Reactome cascaded request for UniProt ID(s).
IntactReactomeXML<-ENVISIONQuery(ids="P38398",
serviceName=c("Intact","Reactome"),typeName="Uniprot ID",verbose=TRUE);
#convert xml text into XMLDocument
#using XML package for further exploring
if(!is.null(IntactReactomeXML)){
xmlDoc<-xmlTreeParse(IntactReactomeXML,useInternalNodes = TRUE, asText=TRUE);
class(xmlDoc);
}


###################################################
### code chunk number 5: chunk5 (eval = FALSE)
###################################################
## filter<-list(Microarray.Platform="affy_hg_u133_plus_2",
## organism.species="Homo sapiens");
## res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),
## filter=filter, serviceName="menu",toolName="menu",typeName="menu"); 
## print(res);


###################################################
### code chunk number 6: sessionInfo
###################################################
toLatex(sessionInfo())


