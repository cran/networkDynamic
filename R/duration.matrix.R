duration.matrix <- function(nw0, changes, start, end) {
  edges <- as.matrix(nw0,matrix.type="edgelist")

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
