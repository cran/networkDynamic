%  File networkDynamic/man/as.networkDynamic.Rd
%  Part of the statnet package, http://statnetproject.org
%
%  This software is distributed under the GPL-3 license.  It is free,
%  open source, and has the attribution requirements (GPL Section 7) in
%    http://statnetproject.org/attribution
%
%  Copyright 2012 the statnet development team
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{as.networkDynamic}
\alias{as.networkDynamic}
\alias{as.networkDynamic.default}
%\alias{as.networkDynamic.network.list}
%\alias{as.networkDynamic.data.frame}
%\alias{as.networkDynamic.list}
\alias{as.networkDynamic.network}
\alias{as.networkDynamic.networkDynamic}
\alias{duration.matrix}

\title{
as.networkDynamic
}
\description{
S3 class function for converting between various dynamic network formats and networkDynamic objects and back again.
}
\usage{

%\method{as.network.dynamic}{default}(x,...)

%\method{as.networkDynamic}{network.list}(object,...)

%\method{as.networkDynamic}{data.frame}(object, nodeInfo = NULL, n = NULL, directed = TRUE,
%                 hyper = FALSE, loops = FALSE, multiple = FALSE,
%                 bipartite = FALSE, ...)

%\method{as.networkDynamic}{list}(object,...)

\method{as.networkDynamic}{network}(object, spells = NULL, toggles = NULL, start =
                 min(toggles[, 1]) - 1, end = max(toggles[, 1]), ...)
                 
\method{as.networkDynamic}{networkDynamic}(object,...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{various. For \code{as.dynamic.data.frame} a dataframe with edge timing information}
%  \item{nodeInfo}{data.frame with information on vertex timing}
  \item{spells}{data.frame with information on vertex timing}
  \item{toggles}{data.frame listing times for edge toggles from starting network}
  \item{start}{starting time to be assumed for pre-existing ties when constructing a network from toggles}
  \item{end}{ending time to be assumed for active un-toggled ties when constructing a network from toggles}
  \item{\dots}{ignored right now, possibly additional arguments to control network building}
  The following items set the parameters of the networkDynamic object being built, and probably shouldn't be here
%  \item{n}{See network.initialize} 
%  \item{directed}{See network.initialize}
%  \item{hyper}{See network.initialize} 
%  \item{loops}{See network.initialize} 
%  \item{multiple}{See network.initialize}
 % \item{bipartite}{See network.initialize}
}
\details{
This documentation express the basic utility functions for converting between networkDynamic and other forms of dynamic networks. These are a collection of S3 objects and use the S3 dispatch form to decide which as.networkDynamic function to use automatically. Currently conversion for \code{network.list} is for use with the \code{network.list} object in \code{\link[ergm]{ergm}} package. Note these functions are still in development and are quite fragile. 

\itemize{
\item \code{as.networkDynamic} Generic converter function, handles dispatching based on class of the object.
\item \code{as.networkDynamic.list} Converts network.list objects into networkDynamic objects 
\item \code{as.networkDynamic.data.frame} Converts data.frame objects into networkDynamic objects. In practice this assumes a 6 column matrix (Ego Alter Start End Noncensored duration).
\item \code{as.networkDynamic.list} This assumes either a list of network objects (e.g., the beach data) or two part list composed of a data.frame object one with vertex information and one with edge information (e.g., Skye's .son format) [There is a proposal to alter the spec to edge.list and vertex.list where the user may provide just a list of vertices and time information, just edges list and time information or both]
\item \code{as.networkDynamic.network} This converts networkDynamic object into data.frame objects of the following form: (Ego Alter Start End Noncensored duration).
}


}
\value{
\code{networkDynamic}, \code{\link{network}}, \code{\link{data.frame}}, \code{\link{list}}.
}
%\references{
%
%}
\author{
Pavel, Zack W Almquist <almquist@uci.edu>
}
\note{
NOTE: MORE DETAILS TO COME, THIS IS NOT FINAL MANUAL PAGE FOR THESE FUNCTIONS.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
See Also as \code{\link{duration.matrix}}, \code{\link{activity.attribute}}
}
\examples{

###
### List of networks example
###
# data(windsurferPanels)
#dynBeach<-as.networkDynamic(beach)
# dynBeach

\dontrun{
##################
## modified from dynsim_test.R in ergm tests
##################
library(ergm)
logit<-function(p)log(p/(1-p))
coef.form.f<-function(coef.diss,density) -log(((1+exp(coef.diss))/(density/(1-density)))-1)
S<-100000
n<-200
m<-100
target.stats<-edges<-100
duration<-1000
coef.diss<-logit(1-1/duration)

### Undirected

dyads<-n*(n-1)/2
density<-edges/dyads
coef.form<-coef.form.f(coef.diss,density)

g0<-network.initialize(n,dir=FALSE)

# Get a reasonably close starting network.
set.seed(119)
g1<-san(g0~edges,target.stats=target.stats,verbose=TRUE)

dynsim<-simulate(g1,formation=~edges,dissolution=~edges,coef.form=coef.form,coef.diss=coef.diss,nsim=S,verbose=TRUE)

elexamp<-duration.matrix(dynsim)

###########
### network.list example
###########
class(dynsim)
nsdn<-as.networkDynamic(dynsim)
nsdn
nsdn$mel[[6]]

###########
### edgelist + toggle example
###########
class(elexamp)
eldn<-as.networkDynamic(elexamp)
eldn
eldn$mel[[6]]

elel<-as.networkDynamic(eldn)
head(elel)
}





}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
