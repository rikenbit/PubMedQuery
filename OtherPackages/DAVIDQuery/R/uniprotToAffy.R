uniprotToAffy<-function(uid="O00161", ...){
	DAVIDQuery(ids=uid,type="UNIPROT_ACCESSION",tool="geneIdConversion", annot="AFFYMETRIX_3PRIME_IVT_ID",...);
}

