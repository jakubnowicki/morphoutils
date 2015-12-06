#' Remove selected landmarks
#' 
#' Removes selected landmarks from a list of arrays and from curves matrix.
#' 
#' @param data single array or a list of arrays with landmark data
#' @param landmarks.to.remove vector of landmark numbers to remove
#' @param curves optional curves matrix
#' @export

remove.landmarks <- function(data, landmarks.to.remove, curves = NULL) {
    if (class(data)!='list') {
        data <- list(data)
    }
    n <- length(data)
    output <- list()
    for (i in 1:n) {
        tmp <- data[[i]][-landmarks.to.remove,,]
        output[[i]] <- tmp
    }
    if (is.null(curves) & n == 1) {
        return(output[[1]])
    } else {
        if (is.null(curves)) {
            return(output)
        } else {
            new.curves <- change.curves(curves = curves, points.to.remove = landmarks.to.remove)
            return(c(output, list(new.curves)))
        }
    }
} 