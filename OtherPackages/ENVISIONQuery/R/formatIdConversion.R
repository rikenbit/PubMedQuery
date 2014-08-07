#' Retrieve the set of match pairs from the 'Entry' node of the Enfin xml document.
#' 
#' @name getIdMatchInfo
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param positiveResultIDs Set of 'ID' nodes of the Enfin xml document which contain the pair match information.
#' @param participant.keyword The keyword identifying the match related information within the match top node.
#' Default is "participant".
#' @param attribute.keyword The keyword identifying the attribute related information within the pair match top node.
#' Default is "attribute".
#' @return The array consisting of two columns, the first containing the source ID set,
#' and the second containing the match result
#' @keywords internal 
#' @author  Alex Lisovich, Roger Day

getIdMatchInfo<-function(entry,positiveResultIDs,participant.keyword="participant",attribute.keyword="attribute"){

	if(length(positiveResultIDs)==0)
		return(NULL);

	first<-TRUE;
	attrnames<-NULL;
	pairsInfo<-NULL;

	children<-xmlChildren(entry);

	inResults<-unlist(lapply(children,function(child){xmlAttrs(child)})) %in% positiveResultIDs;

	sets<-children[inResults];
	for (i in 1:length(sets)){
		children<-xmlChildren(sets[[i]]);
		idNodes<-children[names(children)==participant.keyword];

		if(length(idNodes)>0){
			idVals<-unlist(lapply(idNodes,function(idNode){xmlAttrs(idNode)}));
			attrNodes<-children[names(children)==attribute.keyword];
			if(length(attrNodes)>0){
				mergedVals<-cbind(
					attrnames=unlist(lapply(attrNodes,function(attrNode) {xmlAttrs(attrNode)["name"]})), #attrnames
					attrVals=unlist(lapply(attrNodes,function(attrNode){xmlValue(xmlChildren(attrNode)[[1]])})) #attrVals
				);
				#if(length(attrnames)!=length(unique(attrnames)))
					mergedVals<-mergeMatchRows(mergedVals,1);
				if(first){
					pairsInfo<-array("",dim=c(length(sets),length(idNodes)+ncol(mergedVals)));
					colnames(pairsInfo)[(length(idNodes)+1):ncol(pairsInfo)]<-mergedVals[,1];
					first=FALSE;
				}
				pairsInfo[i,]<-c(idVals,mergedVals[,2]);
			}
		}	
	}
	if (!is.null(pairsInfo))
		pairsInfo<-pairsInfo[pairsInfo[,1]!="",];
	return(pairsInfo);
}

#' Convert the Enfin xml ID conversion results file into the pair match data frame
#'
#' @name formatIdMap
#' @param xml Character string representing an Enfin xml file
#' @param filter The list where the name of each element represents an output data frame column
#' on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
#' containing a character vector defining the set of values on which the merging (intersection) for a given column
#' will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
#' The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.
#' @param compact If TRUE, collapses the rows with duplicated match sets but different attributes 
#' into a single row with unique match set and an attribute list separated by comma for each attribute column. Default is TRUE.
#' @param verbose if TRUE enables diagnostic messages. Default is FALSE.
#' @return The data frame consisting of two groups of columns: 
#' the first containing the ID match set (i.e. 'Affymetrix GeneChip', 'Uniprot' etc.),
#' and the second containing the attributes for a particular match ('organism.species', 'Microarray.Platform' etc.).
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

formatIdMap<-function(xml,filter=list(),compact=TRUE,verbose=TRUE){
	if(verbose)
		cat("formatting output xml...\n");

	first<-TRUE;

	tree<-xmlTreeParse(xml,useInternalNodes = TRUE, asText=TRUE);
	doc<-xmlRoot(tree);

	entries<-xmlChildren(doc);
	entry<-entries[[1]];


	positiveResultIDs<-getPositiveResultSetIDs(entry);
	idMatchInfo<-getIdMatchInfo(entry,positiveResultIDs);

	if(is.null(idMatchInfo))
		return(NULL);


	matchInds<-which(is.na(colnames(idMatchInfo)));
	idEntries<-getIdEntries(entry);
	if(first){
		first<-FALSE;
		children<-xmlChildren(entry);
		firstIDs<-idMatchInfo[1,matchInds];
		colnames(idMatchInfo)[matchInds]<-idEntries[firstIDs,"db"];
	}

	for(i in 1:nrow(idMatchInfo)){
		idMatchInfo[i,matchInds]<-idEntries[idMatchInfo[i,matchInds],1];
	}

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

	if(compact)
		idMatchInfo<-mergeMatchRows(idMatchInfo,matchInds);


	return(idMatchInfo);
}


