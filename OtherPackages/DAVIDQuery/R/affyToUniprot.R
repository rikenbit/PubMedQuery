
affyToUniprot<-function(ids = "88736_AT", ...){
	DAVIDQuery(ids=ids,type="AFFYMETRIX_3PRIME_IVT_ID",tool="geneIdConversion", annot="UNIPROT_ACCESSION",...);

}
