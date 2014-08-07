\name{getServiceClient}
\alias{getServiceClient}
\title{Get the Web Service client finction handler for a given tool...}
\usage{getServiceClient(tool)}
\description{Get the Web Service client finction handler for a given tool}
\value{the function which implements client functionality for a particular data retrieval service}
\author{Alex Lisovich, Roger Day}
\arguments{\item{tool}{The tool handler}}
\examples{#get Java Web Service client for 'FindPathAdv' tool of 'Reactome' service

service<-getService("Reactome");
getToolNames(service);
tool<-getTool(service,"FindPathAdv");
client<-getServiceClient(tool);
print(client);}
