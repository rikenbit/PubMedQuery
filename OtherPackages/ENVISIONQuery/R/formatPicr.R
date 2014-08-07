

#' Retrieve PICR match info from the 'Entry' node of the Enfin xml document.
#' 
#' @name getPicrMatchInfo
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param positiveResultIDs Set of 'ID' nodes of the Enfin xml document which contain the pair match information.
#' @return The array consisting of three columns, the first containing the source ID set,
#' and the second containing the ID set the source IDs are interacting with and the thirfd
#' containing IntAct interaction ID.
#' @keywords internal 
#' @author  Alex Lisovich, Roger Day

getPicrMatchInfo<-function(entry,positiveResultIDs){

	if(length(positiveResultIDs)==0)
		return(NULL);

	source.keyword<-"sourceRef";
	ref.keyword<-"xrefs";


	first<-TRUE;
	attrnames<-NULL;
	matchInfo<-NULL;

	children<-xmlChildren(entry);

	#get molecules (contain ID info)
	molecules<-children[names(children)=="molecule"];
	moleculeIDs<-unlist(lapply(molecules,function(molecule){xmlAttrs(molecule)["id"]}));

	#get result nodes
	resultInds<-moleculeIDs %in% positiveResultIDs;
	
	resultNodes<-molecules[resultInds];

	#get input nodes
	inputIDs<-getInputIdEntries(entry);
	inputInds<-moleculeIDs %in% inputIDs;
	inputNodes<-molecules[inputInds];

	#create srcInfo vector
	srcInfo<-rep("",times=length(inputNodes));
	names(srcInfo)<-inputIDs;

	#fill in input info
	for (i in 1:length(inputNodes)){
		node<-inputNodes[[i]];
		refNode<-xmlChildren(node)[ref.keyword];
		srcInfo[i]<-xmlAttrs(refNode[[1]][[1]])["id"];
	}

	#create matchInfo table
	matchInfo<-array("",dim=c(length(resultNodes),1));
	colnames(matchInfo)<-"Protein ID";

	for (i in 1:length(resultNodes)){

		#get result ID and DB name
		set_children<-xmlChildren(resultNodes[[i]]);
		primaryRefNode=xmlChildren(resultNodes[[i]])[ref.keyword];
		resultID<-xmlAttrs(primaryRefNode[[1]][[1]])["id"];
		resultDB<-xmlAttrs(primaryRefNode[[1]][[1]])["db"];

		#get input ID handling case of mapping to itself (no source.keyword)
		sourceID<-xmlAttrs(resultNodes[[i]])[source.keyword];
		if(is.na(sourceID))
			sourceID<-xmlAttrs(resultNodes[[i]])["id"];

		#add DB column if necessary
		if(!(resultDB %in% colnames(matchInfo))){
			matchInfo<-cbind(matchInfo,"");
			colnames(matchInfo)[ncol(matchInfo)]<-resultDB;
		}

		matchInfo[i,1]<-srcInfo[sourceID];
		matchInfo[i,resultDB]<-resultID;
	}

	return(matchInfo);
}



#' Convert the EnXML PICR  service results file into the protein interaction data frame
#'
#' @name formatPicr
#' @param xml Character string representing an Enfin xml file
#' @param filter The list where the name of each element represents an output data frame column
#' on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
#' containing a character vector defining the set of values on which the merging (intersection) for a given column
#' will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
#' The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.
#' @param compact If TRUE, collapses the rows with duplicated match sets but different attributes 
#' into a single row with unique match set and an attribute list separated by comma for each attribute column. Default is TRUE.
#' @param verbose if TRUE enables diagnostic messages. Default is FALSE.
#' @return The data frame consisting of three columns, the first containing the source ID set,
#' and the second containing the ID set the source IDs are interacting with and the thirfd
#' containing IntAct interaction ID. 
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

formatPicr<-function(xml,filter=list(),compact=TRUE,verbose=TRUE){
	if(verbose)
		cat("formatting output xml...\n");

	first<-TRUE;

	tree<-xmlTreeParse(xml,useInternalNodes = TRUE, asText=TRUE);
	doc<-xmlRoot(tree);

	entries<-xmlChildren(doc);
	entry<-entries[[1]];


	positiveResultIDs<-getPositiveResultSetIDs(entry);

	idMatchInfo<-getPicrMatchInfo(entry,positiveResultIDs);

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

	if(compact && (ncol(idMatchInfo)==2))
		idMatchInfo<-mergeMatchRows(idMatchInfo,1);

	return(idMatchInfo);
}


