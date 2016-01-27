#' Create parameters data frame
#' 
#' Creates data frame containing parameters from landmark array.
#' 
#' @param data landmark array
#' @param parameters.list list of parameters - name of element should be the name of parameter and it has to contain 
#'  landmark numbers (2 or more)
#' @param transverse names of transverse parameters (if they are counted from groups of landmarks)
#' @param saggital names of saggital parameters (if they are counted from groups of landmarks)
#' @param taxon optional taxon name
#' @export 

parameters.data <- function(data,parameters.list, transverse = NULL, saggital = NULL, taxon = NULL) {
    n.specimens <- dim(data)[3]
    n.parameters <- length(parameters.list)
    names.parameters <- names(parameters.list)
    specimens.names <- attributes(data)$dimnames[[3]]
    output <- as.data.frame(matrix(0,ncol=n.parameters,nrow = n.specimens))
    for (i in 1:n.specimens) {
        output[i,] <- unlist(count.parameters(data = data[,,i],parameters.list = parameters.list,
                                              transverse = transverse, saggital = saggital))
    }
    colnames(output) <- names.parameters
    output <- cbind(specimens.names,output)
    if (!is.null(taxon)) {
        output$taxon <- taxon
    }
    return(output)
}