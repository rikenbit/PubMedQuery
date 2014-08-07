
#' Retrieve a set of all ID entries
#'
#' @name getIdEntries
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param nodeName The name identifying the node containing the ID info. Default is 'molecule'.
#' @param primaryAttr The name of attribute which is used as row names of the output data frame.
#' Default is 'id' standing for gene ID
#' @param dataAttrs The names of the attributes which values are to be retrieved. 
#' Default is c('id','db'), i.e. gene ID and data base source. 
#' @return Data frame with a single column containing all (input and output) IDs. 
#' with row names corresponding to the Enfin xml reference names.
#' @keywords internal
#' @author  Alex Lisovich, Roger Day
 
getIdEntries<-function(entry,nodeName="molecule",primaryAttr="id", dataAttrs=c("id","db")){
	children<-xmlChildren(entry);
	molecules<-children[names(children)==nodeName];

	res<-NULL;
	for (attrName in dataAttrs) {
		attrVals<-as.character(unlist(lapply(molecules,function(molecule){xmlAttrs(molecule[[1]][[1]])[attrName]})));
		res<-cbind(res,attrVals);
	}

	res<-data.frame(res,stringsAsFactors=FALSE);
	rownames(res)<-as.character(unlist(lapply(molecules,function(molecule){xmlAttrs(molecule)[primaryAttr]})));
	colnames(res)<-dataAttrs;

	return(res);	
}

#' Retrieve a set of input ID entries
#'
#' The retrieval method based on assumption than the input ID set info incapsulated within the very first
#' node with name 'set'.
#'
#' @name getInputIdEntries
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param setName The name ov candidate nodes potentially containing the input ID set info. Default is 'set'.
#' @return Character vector representing the set of input IDs.
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

getInputIdEntries<-function(entry,setName="set"){
	children<-xmlChildren(entry);
	set<-children[names(children)==setName][[1]];
	participants<-xmlChildren(set);
	inputEntries<-as.character(unlist(lapply(participants,function(participant){xmlAttrs(participant)})));
	return(inputEntries);
}

#' Retrieve a set of positive query result references from the 'Entry' node of the Enfin xml document.
#'
#' @name getPositiveResultSetIDs
#' @param entry The 'Entry' node of the Enfin xml document.
#' @param setName The name of the the nodes potentially containing the positive result set info. Default is 'set'.
#' @param keyword The name of a set node subnode potentially containing the positive result set info. Default is 'setType'.
#' @param term The attribute name to test the candidate node against. Default is 'positive result set'.
#' @return Character vector representing the references to the positive matching results node set.
#'
#' @usage getPositiveResultSetIDs(entry,setName="set",keyword="setType",term="positive result set")
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

getPositiveResultSetIDs<-function(entry,setName="set",keyword="setType",term="positive result set"){
	positive_results<-NULL;
	children<-xmlChildren(entry);
	sets<-children[names(children)==setName];
	for(i in length(sets):1){
		items<-xmlChildren(sets[[i]]);
		if (keyword %in% names(items)){
			attribute<-xmlAttrs(items[[keyword]])[1];			
			if (attribute==term){
				positive_results<-items;
				break;
			}
		}
	}
	if(is.null(positive_results))
		return(NULL);

	items<-items[names(items)=="participant"];
	setIDs<-as.character(unlist(lapply(items,function(item){xmlAttrs(item)[1]})));
	return(setIDs);
}


#' Merge a set of rows with non-unique values in a given set of columns.
#'
#' Collapse the rows that have the same values in for a given set of columns into a single row,
#' merging the rest of the values into the list of unique values separated by comma, one list per column.
#' 
#' @name mergeMatchRows
#' @param matchTable Data frame on which merging is performed.
#' @param matchInds The column indexes which should contain the unique set of values after merging.
#' @return Data frame conaining the unique set of values in matchInds column set.
#' 
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

mergeMatchRows<-function(matchTable,matchInds){
	pasteColumns<-function (x, sep = "") {
    		pastestring <- paste("list(", paste("x", "[", 1:dim(x)[1], 
        				",]", sep = "", collapse = ","), ")", sep = "");
    		return(do.call(paste, c(eval(parse(text = pastestring)), sep = sep)));
	}


	columns<-colnames(matchTable)[matchInds];

	splitIDs<-pasteColumns(t(matchTable[,matchInds]),sep="\r");

	f<-factor(splitIDs);
	
	res<-levels(f);
	splits<-unlist(strsplit(res,split="\r"));
	res<-matrix(splits,ncol=length(matchInds),byrow=TRUE);	
	

	
	##attrColumns<-colnames(matchTable)[!(colnames(matchTable) %in% columns)];
	attrColumns<-c(1:ncol(matchTable))[-matchInds];

	for(column in attrColumns){
		splits<-split(matchTable[,column],f);
		splits<-lapply(splits,unique);
		attrList<-lapply(splits,paste,collapse=",");
		attributes<-unlist(attrList);
		res<-cbind(res,attributes);
	}

	res<-as.data.frame(res,stringsAsFactors=FALSE);
	colnames(res)<-colnames(matchTable);

	return(res);

}


