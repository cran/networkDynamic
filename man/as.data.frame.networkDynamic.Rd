\name{as.data.frame.networkDynamic}
\alias{as.data.frame.networkDynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Convert a networkDynamic object into alternate representations.
}
\description{
Converts a networkDynamic object into some sort of data frame giving lists of edges and their activity spells.
}
\usage{
  \method{as.data.frame}{networkDynamic}(x, row.names = NULL, optional = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a networkDynamic object}
  \item{row.names}{ignored}
  \item{optional}{ignored}
  \item{\dots}{possible additional arguments}
}
\details{
Converts a networkDynamic object into some sort of data frame giving lists of edges and their activity spells.  Probably has arguments to indicate if it should print timed edgelist or timed vertex list?
}
\value{
A data.frame containing timed relational information describing the networkDynamic object. FORMAT?
}

\author{
Pavel ?
}
\note{
Note: probably not fully implemented
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# still missing
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
