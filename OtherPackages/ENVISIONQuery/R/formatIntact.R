

#' Retrieve IntAct match info from the 'Entry' node of the Enfin xml document.
#' 
#' @name getIntactMatchInfo
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param positiveResultIDs Set of 'ID' nodes of the Enfin xml document which contain the pair match information.
#' @param participant.keyword The keyword identifying the match related information within the match top node.
#' Default is "participant".
#' @param primary.ref.keyword The keyword identifying the IntAct ID related information. Default is 'xrefs'.
#' @return The array consisting of three columns, the first containing the source ID set,
#' and the second containing the ID set the source IDs are interacting with and the thirfd containing IntAct interaction ID. 
#' @keywords internal 
#' @author  Alex Lisovich, Roger Day

getIntactMatchInfo<-function(entry,positiveResultIDs,participant.keyword="participant",primary.ref.keyword="xrefs"){

	if(length(positiveResultIDs)==0)
		return(NULL);

	first<-TRUE;
	attrnames<-NULL;
	matchInfo<-NULL;

	children<-xmlChildren(entry);

	inResults<-unlist(lapply(children,function(child){xmlAttrs(child)})) %in% positiveResultIDs;

	sets<-children[inResults];
	for (i in 1:length(sets)){
		children<-xmlChildren(sets[[i]]);
		primaryRefNode=children[primary.ref.keyword];
		idNodes<-children[names(children)==participant.keyword];
		

		if(length(idNodes)>0){
			idVals<-unlist(lapply(idNodes,function(idNode){xmlAttrs(idNode)}));
			ebID<-xmlAttrs(primaryRefNode[[1]][[1]])["id"];
			if(first){
				matchInfo<-array("",dim=c(length(sets),3));
				colnames(matchInfo)[3]<-"IntAct.ID";
				first=FALSE;
			}
			matchInfo[i,]<-c(idVals,ebID);
		}	
	}

	return(matchInfo);
}



#' Convert the EnXML IntAct service results file into the protein interaction data frame
#'
#' @name formatIntact
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

formatIntact<-function(xml,filter=list(),compact=TRUE,verbose=TRUE){
	if(verbose)
		cat("formatting output xml...\n");

	first<-TRUE;

	tree<-xmlTreeParse(xml,useInternalNodes = TRUE, asText=TRUE);
	doc<-xmlRoot(tree);

	entries<-xmlChildren(doc);
	entry<-entries[[1]];


	positiveResultIDs<-getPositiveResultSetIDs(entry);
	idMatchInfo<-getIntactMatchInfo(entry,positiveResultIDs);

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

	#modify name for column #2
	colnames(idMatchInfo)[2]<-paste(colnames(idMatchInfo)[2],"(Interacts with)");

	return(idMatchInfo);
}


