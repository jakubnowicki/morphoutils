#' Count linear parameters
#' 
#' Counts linear parameters from landmark data. If elemens are specified as group of landmarks, object should be rotated
#'  - saggital axis should be parallel to y axis.
#'  
#' @param data landmark matrix
#' @param parameters.list list of parameters - name of element should be the name of parameter and it has to contain 
#' landmark numbers (2 or more)
#' @param transverse names of transverse parameters (if they are counted from groups of landmarks)
#' @param saggital names of saggital parameters (if they are counted from groups of landmarks)
#' @export

count.parameters <- function(data,parameters.list, transverse = NULL, saggital = NULL) {
    n.parameters <- length(parameters.list)
    names.parameters <- names(parameters.list)
    output <- list()
    for (i in 1:n.parameters) {
        if (length(parameters.list[[i]])==2) {
            a <- data[parameters.list[[i]][1],]
            b <- data[parameters.list[[i]][2],]
            output[[i]] <- dist(rbind(a,b))
        } else {
            if (!(names.parameters[i] %in% c(transverse,saggital))) {
                stop('Describe axis of measurements')
            }
            if(names.parameters[i] %in% transverse) {
                output[[i]] <- abs(max(data[parameters.list[[i]],1])-min(data[parameters.list[[i]],1]))
            } else {
                output[[i]] <- abs(max(data[parameters.list[[i]],2])-min(data[parameters.list[[i]],2]))
            }
        }
    }
    names(output) <- names(parameters.list)
    return(output)
}