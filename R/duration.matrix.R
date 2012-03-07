#  File networkDynamic/R/duration.matrix.R
#  Part of the statnet package, http://statnetproject.org
#
#  This software is distributed under the GPL-3 license.  It is free,
#  open source, and has the attribution requirements (GPL Section 7) in
#    http://statnetproject.org/attribution
#
#  Copyright 2012 the statnet development team
######################################################################
duration.matrix <- function(nw0, changes, start, end) {
  edges <- as.edgelist(nw0)
  if(!is.directed(nw0)){
    # The following shouldn't be necessary, but just to be safe:
    changes[,2:3] <- cbind(pmin(changes[,2,drop=FALSE],changes[,3,drop=FALSE]),pmax(changes[,2,drop=FALSE],changes[,3,drop=FALSE]))
  }

  allties <- .C("DurationMatrix", as.integer(network.size(nw0)),
                as.integer(nrow(edges)),
                as.integer(edges), 
                as.integer(start), as.integer(end), 
                as.integer(nrow(changes)), as.integer(as.matrix(changes)),
                duration = as.integer(rep(0,6*(nrow(edges)+nrow(changes)))),
                PACKAGE = "networkDynamic")$duration
  allties <- as.data.frame(matrix(allties, ncol=6))
  names(allties) <- c("start", "end", "tail", "head", "left.censored", "right.censored")
  allties <- allties[allties[,3]!=0,] # Get rid of unused rows
  allties$left.censored <- as.logical(allties$left.censored)
  allties$right.censored <- as.logical(allties$right.censored)
  allties$duration <- with(allties, end-start)
  allties
}
