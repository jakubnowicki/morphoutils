#' Bind landmark arrays
#' 
#' Returns list with, as first element, binded landmark arrays readed from the tps files and, as next elements, 
#' variables (taxon names, localisations, etc.).
#' 
#' @param ... arrays to bind
#' @param list.of.variables list of variables, each with levels corresponding to number of binded arrays
#' @param as.factor logical indicating whether variables should be returned as factor
#' @import abind
#' @export

bind.tps.with.variables <- function(...,list.of.variables,as.factor = TRUE) {
  data <- list(...)
  n.samples <- length(data)
  n.variables <- length(list.of.variables)
  n.specimens.in.samples <- sapply(data,function(x) {dim(x)[3]})
  for (i in 1:n.variables) {
    if (length(list.of.variables[[i]])!=n.samples) {
      stop('wrong number of variables')
    }
  }
  landmarks <- abind::abind(data, along = 3)
  output <- list()
  output[[1]] <- landmarks
  for (i in 1:n.variables) {
    tmp <- mapply(rep,list.of.variables[[i]],n.specimens.in.samples,SIMPLIFY = FALSE)
    tmp <- unlist(tmp)
    tmp <- unname(tmp)
    if (as.factor == TRUE) {
        tmp <- as.factor(tmp)
    }
    output[[i+1]] <- tmp
  }
  if (!is.null(names(list.of.variables))) {
    names(output) <- c('landmarks',names(list.of.variables))
  }
  return(output)
}