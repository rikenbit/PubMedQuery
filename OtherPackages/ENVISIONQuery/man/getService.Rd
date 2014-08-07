\name{getService}
\alias{getService}
\title{Get the service handler using it's name...}
\usage{getService(serviceName="menu", graphicMenu=getOption("menu.graphics"))}
\description{Get the service handler using it's name}
\value{the Envision service handler.}
\author{Alex Lisovich, Roger Day}
\arguments{\item{serviceName}{The name of Envision service which handler should be retrieved. If equal to "menu" (default)
menu is contructed allowing to choose one of the available services}
\item{graphicMenu}{If TRUE (default is FALSE), use a GUI window for the pick menus.}}
\examples{#get an ID Conversion service handle

service<-getService("ID Conversion");

\dontrun{

#get a handle to service selected interactively

service<-getService("menu");
}}
