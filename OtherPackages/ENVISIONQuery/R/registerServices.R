


#' Register available Envision services
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day
 
registerServices<-function(){

	utility<-tryCatch(new(J("UtilityEnvisionClient")),error=function(err) NULL);
	intact<-tryCatch(new(J("IntactEnvisionClient")),error=function(err) NULL);
	picr<-tryCatch(new(J("PicrEnvisionClient")),error=function(err) NULL);
	probe2uniprot<-tryCatch(new(J("Probe2UniprotEnvisionClient")),error=function(err) NULL);
	affy2uniprot<-tryCatch(new(J("Affy2UniprotEnvisionClient")),error=function(err) NULL);
	reactome<-tryCatch(new(J("ReactomeEnvisionClient")),error=function(err) NULL);

	
	
	serviceRegistry<-list();
	if(!is.null(utility)){
		serviceRegistry[["Utility"]]=list(
			tools=list(
				"Uniprot ID"=list(client=utility$xmlFromUniprotList),
				"Affymetrix ID"=list(client=utility$xmlFromAffyList),
				"Enquant ID"=list(client=utility$xmlFromEnquantList),
				"Protein ID"=list(client=utility$xmlFromProteinList),
				"Enfin XML"=list(client=utility$xmlFromEnfinXML),
				"Enfine XML file"=list(client=utility$xmlFromEnfinFile)
			)
		)
	}
	if(!is.null(affy2uniprot)){
		affy2uniprot$enableLogger(FALSE);
		serviceRegistry[["ID Conversion"]]=list(
			service=affy2uniprot,
			tools=list(
				Affy2Uniprot=list(
					client=affy2uniprot$mapAffy2UniProt,
					input=c("Affymetrix ID","Enfin XML","Enfine XML file"),
					formatter=formatIdMap,
					merger=mergeFrameList
				),			
				Uniprot2Affy=list(
					client=affy2uniprot$mapUniProt2Affy,
					input=c("Uniprot ID","Enfin XML","Enfine XML file"),
					formatter=formatIdMap,
					merger=mergeFrameList
				)
			)						
		)
	}
	if(!is.null(intact)){
		intact$enableLogger(FALSE);
		serviceRegistry[["Intact"]]=list(
			service=intact,
			tools=list(
				findPartners=list(
					client=intact$findPartners,
					input=c("Uniprot ID","Enfin XML","Enfine XML file"),
					formatter=formatIntact,
					merger=mergeFrameList
				)
			)
		)
	}
	if(!is.null(picr)){
		picr$enableLogger(FALSE);
		serviceRegistry[["Picr"]]=list(
			service=picr,
			tools=list(
				#map2Uniprot=list(
				#	client=picr$map2Uniprot,
				#	input=c("Protein ID","Enfin XML","Enfine XML file"),
				#	formatter=formatPicr,
				#	merger=mergeFrameList
				#),			
				mapProteinsAdv=list(
					client=picr$mapProteinsAdv,
					input=c("Protein ID","Enfin XML","Enfine XML file"),
					formatter=formatPicr,
					merger=mergeFrameList
				)			
			)
		)
	}
	if(!is.null(reactome)){
		reactome$enableLogger(FALSE);
		serviceRegistry[["Reactome"]]=list(
			service=reactome,
			tools=list(
				#findPath=list(
				#	client=reactome$findPath,
				#	input=c("Uniprot ID","Enfin XML","Enfine XML file"),
				#	formatter=formatReactome,
				#	merger=mergeFrameList
				#),
				findPathAdv=list(
					client=reactome$findPathAdv,
					input=c("Uniprot ID","Enfin XML","Enfine XML file"),
					formatter=formatReactome,
					merger=mergeFrameList
				)
			)
		)
	}
	return(serviceRegistry);


	#serviceRegistry[["Probe2Uniprot"]]=list(
	#	client=probe2uniprot,
	#	formatter=formatIdMap,
	#	merger=mergeFrameList,
	#	tools=list(
	#		Full=probe2uniprot$doService
	#		#Compact=probe2uniprot$doServiceAdv
	#	)						
	#
	#)

}