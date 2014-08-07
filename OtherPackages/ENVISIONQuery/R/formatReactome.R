

#' Retrieve Reactome pathway match info from the 'Entry' node of the Enfin xml document.
#' 
#' @name getReactomeMatchInfo
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param positiveResultIDs Set of 'ID' nodes of the Enfin xml document which contain the pair match information.
#' @return The array with at least three columns, the first containing the source ID set, the second containing
#' the pathway description and the third containing the link to Reactome web page for a given pathway. 
#' If 'enfin-reactome-add-coverage' option is set to 'true' then two extra columns
#' are added: the first for pathway coverage number, and the second for the total protein count per pathway.
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

getReactomeMatchInfo<-function(entry,positiveResultIDs){
	if(length(positiveResultIDs)==0)
		return(NULL);

	source.keyword<-"sourceRef";
	ref.keyword<-"xrefs";	
	
	first<-TRUE;
	attrnames<-NULL;
	matchInfo<-NULL;

	children<-xmlChildren(entry);

	molecules<-children[names(children)=="molecule"];
	moleculeIDs<-unlist(lapply(molecules,function(molecules){xmlAttrs(molecules)["id"]}));

	sets<-children[names(children)=="set"];
	setIDs<-unlist(lapply(sets,function(sets){xmlAttrs(sets)["id"]}));
	resultInds<-setIDs %in% positiveResultIDs;
	resultNodes<-sets[resultInds];

	matchInfo<-array("",dim=c(length(resultNodes),3));
	colnames(matchInfo)<-c("UniProt ID","Pathway","URL");
	for (i in 1:length(resultNodes)){

		#get pathway description and DB url
		pathwayNode<-xmlChildren(resultNodes[[i]])["names"];
		pathway<-xmlValue(pathwayNode[[1]][[1]]);
		urlNode<-xmlChildren(resultNodes[[i]])["setType"];
		urlID<-xmlAttrs(urlNode[[1]])["id"];
		url<-paste("http://www.reactome.org/cgi-bin/eventbrowser?DB=gk_current&ID=",urlID,sep="");

		#get input protein ID
		sourceNode<-resultNodes[[i]]["participant"][[1]];
		sourceRefID<-xmlAttrs(sourceNode);
		sourceNode<-molecules[moleculeIDs==sourceRefID];
		sourceRefNode<-xmlChildren(sourceNode[[1]])[ref.keyword];
		sourceID<-xmlAttrs(sourceRefNode[[1]][[1]])["id"];

		paramNodes<-resultNodes[[i]]["parameter"];
		vals<-c(sourceID,pathway,url);
		if(length(paramNodes)>0){
			if(first){
				first<-FALSE;
				matchInfo<-cbind(matchInfo,"","");
				colnames(matchInfo)[c(4,5)]<-c("pathway.coverage","total.protein.count");
			}
			for (j in 1:length(paramNodes))
				vals<-c(vals,xmlAttrs(paramNodes[[j]])["factor"]);
		}

		matchInfo[i,]<-vals;
	}

	return(matchInfo);
}



#' Convert the EnXML PICR  service results file into the protein interaction data frame
#'
#' @name formatReactome
#' @param xml Character string representing an Enfin xml file
#' @param filter The list where the name of each element represents an output data frame column
#' on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
#' containing a character vector defining the set of values on which the merging (intersection) for a given column
#' will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
#' The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.
#' @param compact If TRUE, collapses the rows with duplicated match sets but different attributes 
#' into a single row with unique match set and an attribute list separated by comma for each attribute column. Default is TRUE.
#' @param verbose if TRUE enables diagnostic messages. Default is FALSE.
#' @return The data frame with at least three columns, containing the source ID set, 
#' the pathway description and the link to Reactome web page for a given pathway correspondingly. 
#' If 'enfin-reactome-add-coverage' option is set to 'true' then two extra columns
#' are added: for pathway coverage number, and  total protein count per pathway.
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

formatReactome<-function(xml,filter=list(),compact=TRUE,verbose=TRUE){
	if(verbose)
		cat("formatting output xml...\n");

	first<-TRUE;

	tree<-xmlTreeParse(xml,useInternalNodes = TRUE, asText=TRUE);
	doc<-xmlRoot(tree);

	entries<-xmlChildren(doc);
	entry<-entries[[1]];


	positiveResultIDs<-getPositiveResultSetIDs(entry);

	idMatchInfo<-getReactomeMatchInfo(entry,positiveResultIDs);

	if(is.null(idMatchInfo))
		return(NULL);


	rm(tree);
	gc();

	colnames(idMatchInfo)<-gsub(" ",".",colnames(idMatchInfo),fixed=TRUE);
	colnames(idMatchInfo)<-gsub("_",".",colnames(idMatchInfo),fixed=TRUE);

	names<-names(filter)[names(filter) %in% colnames(idMatchInfo)];
	if(length(filter)>0){
		for(name in names){
			idMatchInfo<-idMatchInfo[idMatchInfo[,name] %in% filter[[name]],];
		}	
	}


	return(idMatchInfo);
}


