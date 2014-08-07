\name{getToolNames}
\alias{getToolNames}
\title{Get the names of available tools for a given Envision service...}
\usage{getToolNames(service)}
\description{Get the names of available tools for a given Envision service}
\value{Character vector representing the list of of available tool names for a given service.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{service}{The Envision service handler}}
\examples{#get the list of available tools for a given service

service<-getService("ID Conversion");
toolNames<-getToolNames(service);
print(toolNames);}
