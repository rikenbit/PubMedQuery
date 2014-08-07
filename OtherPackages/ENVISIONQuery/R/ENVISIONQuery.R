#' Get available services
#' 
#' @name getServices
#' @return The multi-level list of services and service attributes
#'
#' @examples
#' #get the handlers for all available services including utilities
#' services<-getServices();
#' names(services);
#'
#' @export getServices
#' @author  Alex Lisovich, Roger Day

getServices<-function(){
	if(is.null(ENVISIONServices))
		assignInNamespace("ENVISIONServices",registerServices(),"ENVISIONQuery");
	return(ENVISIONServices)
}


#' Get the names of available Envision services
#'
#' @name getServiceNames
#' @return Character vector representing the list of of available Envision service names.
#'
#' @examples
#'
#' #get the names of all available services
#' serviceNames<-getServiceNames();
#' print(serviceNames);
#'
#' @export getServiceNames
#' @author  Alex Lisovich, Roger Day

getServiceNames<-function(){
	return(names(getServices()));
}


#' Get the service handler using it's name
#'
#' @name getService
#' @param serviceName The name of Envision service which handler should be retrieved. If equal to "menu" (default)
#' menu is contructed allowing to choose one of the available services
#' @param graphicMenu If TRUE (default is FALSE), use a GUI window for the pick menus.
#' @return the Envision service handler.
#'
#' @examples
#'
#' #get an ID Conversion service handle
#'
#' service<-getService("ID Conversion");
#'
#' \dontrun{
#'
#' #get a handle to service selected interactively
#'
#' service<-getService("menu");
#' }
#'
#' @export getService
#' @author  Alex Lisovich, Roger Day

getService<-function(serviceName="menu",graphicMenu=getOption("menu.graphics")){
	serviceNames<-getServiceNames();
	if (serviceName=="menu"){
		item<-menu(graphics = graphicMenu, title = "Select Service", serviceNames[-1]);
		if(item==0)
			return(NULL);
		serviceName<-serviceNames[item+1];
	}

	if (!(serviceName %in% serviceNames)){
		warning("ENVISIONQuery: Invalid service name");
		return(NULL);
	}
	service<-getServices()[[serviceName]];
	attr(service,"serviceName")<-serviceName;

	return(service);
}

#' Get default service options.
#'
#' @name getServiceOptions
#' @param serviceName The name of Envision service which default options should be retrieved. 
#' @param graphicMenu If TRUE (default is FALSE), use a GUI window for the pick menus.
#' If equal to "menu" (default) menu is contructed allowing to choose one of the available services
#' @return The list where each item name and value corresponds to the particular option
#' name and default value correspondingly.
#'
#' @examples
#' #get options for 'Reactome' service
#' options<-getServiceOptions("Reactome");
#' print(options);
#'
#' \dontrun{
#' #get options for a service selected interactively
#' getServiceOptions("menu");
#' }
#'
#' @export getServiceOptions
#' @author  Alex Lisovich, Roger Day

getServiceOptions<-function(serviceName="menu",graphicMenu=getOption("menu.graphics")){
	service<-getService(serviceName,graphicMenu);
	if(is.null(service))
		return(NULL);

	s_options<-service$service$getDefaultOptions();
	
	options<-list();
	if(length(s_options)>1){
		for (i in seq(from=1,to=length(s_options),by=2))
			options[[s_options[i]]]<-c(options[[s_options[i]]],s_options[i+1]);
	}
	return(options);
}

#' Get the names of available tools for a given Envision service
#'
#' @name getToolNames
#' @param service The Envision service handler
#' @return Character vector representing the list of of available tool names for a given service.
#'
#' @examples
#'
#' #get the list of available tools for a given service
#'
#' service<-getService("ID Conversion");
#' toolNames<-getToolNames(service);
#' print(toolNames);
#'
#' @export getToolNames
#' @author  Alex Lisovich, Roger Day

getToolNames<-function(service){
	return(names(service$tools));
}


#' Get the tool handler of a given service using the tool name
#'
#' @name getTool
#' @param service Service handler
#' @param toolName The name of Envision service tool handler to be retrieved. If equal to "menu" (default)
#' and the number of tools is greater than 1 menu is contructed allowing to choose one of the available services.
#' @param selection.title The selection list title. Default is 'Select Tool'.
#' @param graphicMenu If TRUE (default is FALSE), use a GUI window for the pick menus.
#' @return the Envision service tool handler.
#' @usage getTool(service,toolName="menu",selection.title="Select Tool", graphicMenu=getOption("menu.graphics"))
#'
#' @examples
#' #get the tool handler for 'mapProteinsAdv' tool in 'Picr' service
#'
#' service<-getService("Picr");
#' getToolNames(service);
#' tool<-try({getTool(service,"FindPathAdv");
#' 		client<-getServiceClient(tool);
#' 		print(client);
#' })
#'
#' @export getTool
#' @author  Alex Lisovich, Roger Day

getTool<-function(service,toolName="menu",selection.title="Select Tool", graphicMenu=getOption("menu.graphics")){
	if(length(toolName)>1){
		toolNames<-toolName;
		toolName<-"menu";
	} else {
		toolNames<-getToolNames(service);
	}

	if (length(toolNames)>1) {
		if (toolName=="menu"){
			item<-menu(graphics = graphicMenu, title = selection.title, toolNames);
			if(item==0)
				return(NULL);
			toolName=toolNames[item];
		}
	}else{
		toolName<-toolNames[1];
	}

	if (!(toolName%in% toolNames)){
		warning("ENVISIONQuery: Invalid tool name");
		return(NULL);
	}

	tool<-service$tools[[toolName]];
	attr(tool,"toolName")<-toolName;
	return(tool);
}

#' Get the Web Service client finction handler for a given tool
#'
#' @name getServiceClient
#' @param tool The tool handler
#' @return the function which implements client functionality for a particular data retrieval service
#' 
#' @examples
#' #get Java Web Service client for 'FindPathAdv' tool of 'Reactome' service
#'
#' service<-getService("Reactome");
#' getToolNames(service);
#' try({tool<-getTool(service,"FindPathAdv");
#' 		client<-getServiceClient(tool);
#' 		print(client);
#' })
#' @export getServiceClient
#' @author  Alex Lisovich, Roger Day

getServiceClient<-function(tool){
	return(tool$client);
}

#' Get the list of available input types for a given tool.
#'
#' The input types could be subdivided into the following three groups:
#' 1. The list of IDs (types 'Uniprot ID', 'Affymetrix ID', 'Enquant ID' and 'Protein ID').
#' 2. The character string representing the enfinXML document ('Enfin XML') and
#' 3. The name of the file containing the enfinXML document.
#' The types belonging to the groups 2 and 3 can be used to construct a pipeline of queries, 
#' where the output of the given query can be used as an input for the next one
#'
#' @name getInputTypes
#' @param tool The tool handler
#' @param inputType Either 'menu' or a valid input type. Default is 'menu'.
#' @return If inputType is 'all', the character vector of all available input types. If inputType is 
#' valid input type, the inputType value returned, otherwise returns NULL.
#'
#' @examples
#'
#' #check available input type for a given tool
#'
#' service<-getService("Reactome");
#' try({tool<-getTool(service,"FindPathAdv");
#' 	print(getInputTypes(tool));
#' })
#'
#' @export getInputTypes
#' @author  Alex Lisovich, Roger Day

getInputTypes<-function(tool,inputType="menu"){
	if(inputType=="menu"){
		return(tool$input);
	} else if (inputType %in% tool$input){
		return(inputType);
	} else {
		warning("ENVISIONQuery: invalid input type");
		return(NULL);
	}
}


#' Get the formatting function for a given tool
#'
#' @name getFormatter
#' @param tool Tool handler
#' @return The formatting function for a given service.
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

getFormatter<-function(tool) {
	return(tool$formatter);
}

#' Get the merging function for a given tool.
#'
#' If possible, meerging function merges the list of formatted query result chunks into a single formatted object (typically a data frame).
#'
#' @name getMerger
#' @param tool Tool handler
#' @return The merging function for a given service.
#' 
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

getMerger<-function(tool) {
	return(tool$merger);
}


#' Convert options list into the character vector.
#'
#' Convert options list into the character vector suitable for submission
#' to Java Web Services subsystem.
#'
#' @name jgetOptions
#' @param service Service handler
#' @param options Options list
#' @return Character vector containing set of <option name,value> pairs
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

jgetOptions<-function(service,options){
	all_options<-getServiceOptions(attr(service,"serviceName"));
	option_names<-names(options) %in% names(all_options);
	options<-options[option_names];
	if(length(options)==0)
		return(character(0));

	res<-NULL;
	for (i in 1:length(options)){
		for (j in 1:length(options[[i]]))
			res<-c(res,c(names(options)[i],options[[i]][j]));
	}
	return(res);
}

#' Display the task progress message.
#'
#' Displays the percentage of task completeness accompanied by the custom message by repatedly updating the same output line.
#' @name progressMsg
#' @param msg Custom message to be displayed.
#' @param i Current task step.
#' @param total Total number of task steps.
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

progressMsg<-function(msg,i,total){
	msgFreq<-max(1,as.integer(total/100));
	if ((i %% msgFreq)==0){
		cat("                                                      \r");
		cat(msg,round(i*100/total),"%\r");
		flush.console();
	}
}

#' Convert the ID set into the list of ID subsets.
#'
#' Converts the ID set into the list of ID subsets according to the chunk size.
#' Used to subdivied a large query into the set of smaller queries to overcome a potential limitation
#' on a single query ID set size.
#'
#' @name toChunks
#' @param ids Character vector representing the input ID set.
#' @param chunk The maximium number of IDs retrieved during a single query session. Default is 1000.
#' @return A list of ID subsets according to the chunk size.
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

toChunks<-function(ids,chunk=1000){
	sz<-length(ids);
	if(sz>chunk){
		start<-seq(from=1,to=sz,by=chunk);
		end<-seq(from=chunk,to=sz,by=chunk);
		if(length(end)<length(start))
			end<-c(end,sz);
	} else {
		start<-1;
		end<-sz;
	}

	chunks<-list();
	for(i in 1:length(start))
		chunks[[i]]<-ids[start[i]:end[i]];
	return(chunks);
}


#' Merges the data frames representing the partial query results into a single data frame.
#'
#' @name mergeFrameList
#' @param resList List of data frames.
#' @param verbose if TRUE enables diagnostic messages. Default is FALSE.
#' @return A single data frame representing the merged query results.
#'
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

mergeFrameList<-function(resList,verbose=FALSE){
	if(length(resList)==0)
		return(NULL);

	rows<-0;
	columns<-NULL;
	for(i in 1:length(resList)){
		if(!is.null(resList[[i]])){
			rows<-rows+nrow(resList[[i]]);
			if(is.null(columns))
				columns<-colnames(resList[[i]]);
		}
	}
	res<-array("",dim=c(rows,ncol(resList[[1]])));
	colnames(res)<-columns;
	res<-as.data.frame(res,stringsAsFactors=FALSE);

	row<-0;
	for(i in 1:length(resList)){
		if(!is.null(resList[[i]])){
			rows<-nrow(resList[[i]]);
			res[(row+1):(row+rows),]<-resList[[i]];
			row<-row+rows;
		}
	}
	return(res);
}

#' Perform queries in chunks to comply with potential limitations 
#' on the query size for Envision Web query system.
#'
#' @name ENVISIONQuery.loop
#' @param ids Character vector representing the input ID set.
#' @param chunk maximum size of the portion of the input ID list to be used in a single query Fun call.
#' @param queryFun a functions submitting the query to the Envision Web query system.
#' @param verbose if TRUE enables diagnostic messages. Default is FALSE.
#' @param ... additional arguments to be passed to queryFun.
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

ENVISIONQuery.loop<-function(ids,chunk,queryFun,verbose,...) {
	
	if(is.na(chunk))
		chunk=length(ids);

	idList<-toChunks(ids,chunk);

	res<-list();
	for (i in 1:length(idList)){
		if(verbose){
			if(verbose>1)
				cat("processing chunk",i,"out of",length(idList),"\n")
			else
				progressMsg("retrieved:",(i-1),length(idList));
		}
		nchunk=ifelse(length(idList)>1,i,-1);
		res[[i]]<-queryFun(idList[[i]],nchunk=nchunk,verbose=max(0,(verbose-1)),...);

	}
	if(verbose){
		progressMsg("retrieved:",length(idList),length(idList));
		cat("\n");

	}

	return(res);
}

#' Retrieves a set of unique matches for a given ID list.
#' 
#' @name ENVISIONQuery.chunk
#' @param ids Character vector representing the ID list.
#' @param utilityTool The tool used to convert the ID list into the input Enfin xml file.
#' @param service Service handler
#' @param tool The tool used to perform the conversion
#' @param formatIt If TRUE (default), try to interpret the returned character table and structure the result. 
#' If false, the character string representing the entire enfinXML file returned by ENVISION.
#' @param options The (optional)list each element of which represents the <name,value> pair used to apply
#' the additional constraints when sending a query to a particular service (maximum number of pathways
#' for Reactome service as an example)
#' @param filter The list where the name of each element represents an output data frame column
#' on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the element itself
#' containing a character vector defining the set of values on which the merging (intersection) for a given column
#' will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
#' The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.
#' @param compact If TRUE and the formatted output (formatIt = TRUE)is represented by a data frame, 
#' collapses the rows with duplicated match sets but different attributes into a single row with unique match set
#' and an attribute list separated by comma for each attribute column. Default is TRUE.
#' @param writeHTML If TRUE, write the received intermediate HTML to file. Default is FALSE.
#' @param nchunk Chunk number. If not NA and writeHTML is TRUE the input and output xml file names are appended 
#' with chunk number. Default is NA.
#' @param verbose If TRUE, enables diagnostic messages. Default is FALSE.
#' @param ... additional arguments.
#' @return The ENVISIONQueryResult structure
#' @keywords internal
#' @author  Alex Lisovich, Roger Day

ENVISIONQuery.chunk<-function(ids,utilityTool,service,tool,
		formatIt,options=list(),filter=list(),compact=TRUE,
		writeHTML=FALSE,nchunk=NA,verbose=FALSE,...){

	if(attr(utilityTool,"toolName") %in% c("Enfin XML","Enfine XML file")){
		jids<-.jarray(as.character(ids));
	} else {	
		jids<-.jarray(ids);
	}

	if(verbose)
		cat("Preparing input data...\n");
	inpXML<-utilityTool$client(jids);

	if (writeHTML)
		writeChar(inpXML,paste("input",ifelse(is.na(nchunk),"",nchunk),".xml",sep=""));


	if(verbose)
		cat("Retrieving query data...\n");
	outXML<-tool$client(inpXML,jgetOptions(service,options));

	if (writeHTML)
		writeChar(outXML,paste(attr(service,"serviceName"),"_",attr(tool,"toolName"),"_output",nchunk,".xml",sep=""));

	if(length(grep("Error",outXML,fixed=TRUE))){
		msg=paste("\n Web Service ***",attr(service,"serviceName"),"*** temporarily unavailable\n",
			"If the problem persists, contact EnCORE team (encore-help@ebi.ac.uk) for technical support");
		stop(msg);
	}

	if (formatIt){
		formatter<-getFormatter(tool);
		if (!is.null(formatter)){
			res<-formatter(outXML,filter=filter,compact=compact,verbose=verbose,...);
			return(res);		
		}
	}
	return(outXML);
}

#' Launch a query against Envision, a web online query system
#' providing elaborated information for EnCORE services. 
#' Return the results into an R object.
#'
#' @name ENVISIONQuery
#' @param ids Depending on input type, IDs for desired objects as a character vector a character string containing
#' the entire Enfin xml document or a name of an Enfin xml file.
#' @param typeName Type of input ids. If 'menu' (default), a menu  is constructed allowing to choose one of the available types.
#' @param serviceName The Envision service name or a character vector of service names.
#' In a latter case system compbines requests into pipeline using the output of current service request as an input for a next one.
#' If 'menu' (default), the menu is contructed allowing to choose one of the available services. 
#' @param toolName The tool for a particular envision service. If 'menu' (default) and there is more than one tool available, 
#' a menu is contructed allowing to choose one of the available tools.
#' @param chunk The number of IDs retrieved during a single query session. In case the number of IDs exceeds the chunk size,
#' the whole ID set is retrieved during multiple query sessions overcoming the potential limitations on a single query session
#' ID set size. If NA, a single session is used. Default is 1000.
#' @param details If TRUE (default), a list of intermediate results is returned; otherwise, just the final query result.
#' Note: not implemented at this time
#' @param writeHTML If TRUE (default is FALSE), write the received intermediate HTML to files.
#' @param testMe If TRUE (default is FALSE), assign default values and run.
#' @param graphicMenu If TRUE (default is FALSE), use a GUI window for the pick menus.
#' @param formatIt If TRUE (default), try to interpret the returned character table and structure the result. 
#' If false, the character string representing the entire enfinXML file returned by ENVISION.
#' Note: formatting is implemented only for 'Probe2Uniprot' and 'ID conversion' services at this time.
#' If false, the character string representing the entire enfinXML file returned by ENVISION.
#' @param options The (optional)list each element of which represents the <name,value> pair used to apply
#' the additional constraints when sending a query to a particular service (maximum number of pathways
#' for Reactome service as an example)
#' @param filter The (optional)list where the name of each element represents the formatted output data frame column
#' on which filetering is to be performed ('organism.species', 'Microarray.platform' etc.) and the list element
#' containing a character vector defining the set of values on which the merging (intersection) for a given column
#' will be performed ('Homo sapiens' for 'organism.species', 'affy_hg_u133_plus_2' for 'Microarray.platform' etc.).
#' The filtering is performed if the list is not empty and the formatIt=TRUE. Default is an empty list.
#' @param compact If TRUE and the formatted output (formatIt = TRUE)is represented by a data frame, 
#' collapses the rows with duplicated match sets but different attributes into a single row with unique match set
#' and an attribute list separated by comma for each attribute column. Default is TRUE.
#' @param verbose If TRUE (default is FALSE), more debugging information is printed.
#' @return The ENVISIONQueryResult structure or NULL if no results were found.
#'
#' @note For a pipeline of services, only unformatted output supported at this time.
#'
#' @examples
#' #### basic ENVISIONQuery request
#'
#' #convert the Affy probeset IDs to UniProt IDs
#' res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),
#' 	serviceName="ID Conversion",toolName="Affy2Uniprot",typeName="Affymetrix ID");
#' print(res);
#' 
#' #retrieve the pathways for given Uniprot ID(s)
#' res<-ENVISIONQuery(ids="P38398",serviceName="Reactome",typeName="Uniprot ID");
#' print(res[,-3]);
#' 
#' #retrieve protein-protein interactions
#' res<-ENVISIONQuery(ids="P38398",serviceName="Intact",typeName="Uniprot ID");
#' print(res[1:5,]);
#' 
#' #convert EnSembl IDs to Uniprot IDs
#' try({res<-ENVISIONQuery(ids=c("ENSP00000397145","ENSP00000269554"),serviceName="Picr",typeName="Protein ID");
#' print(res);
#' }) 
#'
#' #### ENVISIONQuery request using options and filters
#' 
#' #match Uniprot IDs to EnSembl and TrEMBL
#' options<-list("enfin-picr-search-database"=c("ENSEMBL_HUMAN","TREMBL"));
#' res<-try({ENVISIONQuery(ids="P38398",serviceName="Picr",options=options,typeName="Protein ID");
#' print(res);
#' })
#' 
#' #retrieve the pathways for given Uniprot ID(s)sorting them by coverage
#' #and calcultating the total protein count
#' options<-list("enfin-reactome-add-coverage"="true",
#' 	"enfin-reactome-sort-by-coverage"="true");
#' res<-ENVISIONQuery(ids="P38398",serviceName="Reactome",options=options,typeName="Uniprot ID");
#' print(res[,-3]);
#' 
#' #convert the Affy probeset IDs to UniProt IDs restricting 
#' #output by micro array type and organism species
#' filter<-list(Microarray.Platform="affy_hg_u133_plus_2",
#' 	organism.species="Homo sapiens");
#' res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),filter=filter,
#' 	serviceName="ID Conversion",toolName="Affy2Uniprot",typeName="Affymetrix ID");
#' print(res);
#' 
#' #### pipeline of ENVISIONQuery requests. As of recent version, 
#' #### output formatting for a pipelined request is not supported yet.
#' 
#' #Intact-Reactome cascaded request for UniProt ID(s).   
#' IntactReactomeXML<-ENVISIONQuery(ids="P38398",
#' 	serviceName=c("Intact","Reactome"),typeName="Uniprot ID",verbose=TRUE);
#' #convert xml text into XMLDocument
#' #using XML package for further exploring
#' if(!is.null(IntactReactomeXML)){
#' xmlDoc<-xmlTreeParse(IntactReactomeXML,useInternalNodes = TRUE, asText=TRUE);
#' class(xmlDoc);
#' }
#' 
#' #### interactive ENVISIONQuery requests
#' \dontrun{
#' res<-ENVISIONQuery(ids=c("1553619_a_at","1552276_a_at","202795_x_at"),filter=filter,
#' 	serviceName="menu",toolName="menu",typeName="menu");
#' print(res);
#' }
#' 
#' @export ENVISIONQuery
#' @author  Alex Lisovich, Roger Day

ENVISIONQuery<-function(ids=c("1553619_a_at","1553497_at"),typeName="menu",serviceName="menu",toolName="menu",chunk=1000,
	details=TRUE, writeHTML=FALSE, testMe=FALSE, graphicMenu=getOption("menu.graphics"), 
	formatIt=TRUE, options=list(),filter=list(), compact=TRUE, verbose=FALSE){

   if (testMe){
	typeName<-"Affymetrix ID";
	serviceName="ID Conversion";
	toolName<-"Affy2Uniprot";
	verbose=TRUE;
   }

   serviceNames<-serviceName;
   if(length(serviceNames)>1)
		formatIt<-FALSE;

  res<-tryCatch({ # catch any possible error
	for(i in 1:length(serviceNames)){
		serviceName<-serviceNames[i];
		if(i>1){
			ids<-res;
			typeName<-"Enfin XML";
		}

		# setup web service
		service<-getService(serviceName,graphicMenu=graphicMenu);
		if(is.null(service))
			return(NULL);

		# setup service tool
		tool<-getTool(service,toolName,graphicMenu=graphicMenu);
		if (is.null(tool))
			return(NULL);

		# setup utility service
		utilityTool<-getTool(getService("Utility"),getInputTypes(tool,typeName),
			selection.title="Select Input Type",graphicMenu=graphicMenu);
		if (is.null(utilityTool))
			return(NULL);

		#disable formatting for EnfinXML input
		if(attr(utilityTool,"toolName") %in% c("Enfin XML","Enfine XML file"))
			formatIt=FALSE;

		if(verbose)
			cat("Data retrieval started. It may take several minutes...\n");

		if(formatIt && is.null(getFormatter(tool))){
			warning(paste("No formatting implemented for",attr(tool,"toolName"),"tool"));
			formatIt<-FALSE;
		}

		res<-ENVISIONQuery.loop(ids,chunk,ENVISIONQuery.chunk,verbose=verbose,
				utilityTool,service,tool,formatIt,options,filter,compact,writeHTML);


		if(formatIt && !is.null(getMerger(tool))){
			if(verbose)
				cat("merging formatted output\n");
			merger<-getMerger(tool);	
			res<-merger(res,verbose=verbose);
		
		}

		if (!is.null(res)){
			attr(res,"options")<-options;
	   	 	attr(res, "filter") <- filter;
			attr(res, "type")<-attr(utilityTool,"toolName");
			attr(res, "service")<-attr(service,"serviceName");
			attr(res, "tool")<-attr(tool,"toolName");
		}
	}#for(serviceName in serviceNames)

	invisible(res);
    }, error=function(e){
      	cat("\n",as.character(e),"\n");
		res<-NULL;
   });
   if(is.null(res))
		cat("ENVISIONQuery: No results found\n");
   return(res);
}
