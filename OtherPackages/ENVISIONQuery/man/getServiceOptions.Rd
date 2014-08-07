\name{getServiceOptions}
\alias{getServiceOptions}
\title{Get default service options.}
\usage{getServiceOptions(serviceName="menu",
    graphicMenu=getOption("menu.graphics"))}
\description{Get default service options.}
\value{The list where each item name and value corresponds to the particular option
name and default value correspondingly.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{serviceName}{The name of Envision service which default options should be retrieved.}
\item{graphicMenu}{If TRUE (default is FALSE), use a GUI window for the pick menus.
If equal to "menu" (default) menu is contructed allowing to choose one of the available services}}
\examples{#get options for 'Reactome' service
options<-getServiceOptions("Reactome");
print(options);

\dontrun{
#get options for a service selected interactively
getServiceOptions("menu");
}}
