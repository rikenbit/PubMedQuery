\name{getTool}
\alias{getTool}
\title{Get the tool handler of a given service using the tool name...}
\usage{getTool(service,toolName="menu",selection.title="Select Tool", graphicMenu=getOption("menu.graphics"))}
\description{Get the tool handler of a given service using the tool name}
\value{the Envision service tool handler.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{service}{Service handler}
\item{toolName}{The name of Envision service tool handler to be retrieved. If equal to "menu" (default)
and the number of tools is greater than 1 menu is contructed allowing to choose one of the available services.}
\item{selection.title}{The selection list title. Default is 'Select Tool'.}
\item{graphicMenu}{If TRUE (default is FALSE), use a GUI window for the pick menus.}}
\examples{#get the tool handler for 'mapProteinsAdv' tool in 'Picr' service

try({ service<-getService("Picr");
  getToolNames(service);
  tool<-getTool(service,"mapProteinsAdv");
})
}
