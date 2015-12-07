#' Replace missing landmarks
#' 
#' Replaces missing landmarks with NA. Based on k-means clustering, seeks for separated group of landmarks (for example you can put all missing landmarks in the corner of the picture). Also handles with tpsdig way of writing missing andmarks (-1, -1).
#' 
#' @param data landmark matrix
#' @export

missing.landmarks <- function(data) {
    data <- as.data.frame(data)
    n.landmarks <- dim(data)[1]
    data$row.ID<-1:n.landmarks
    below.zero <- data[data[,1]<0 | data[,2] < 0,]
    if (dim(below.zero)[1]!=0) {
        below.zero[,1:2] <- NA
    }
    over.zero <- data[data[,1] > 0 | data[,2] > 0,]
    k <- kmeans(over.zero[,1:2],2)
    clusters <- k$cluster
    ss <- k$withinss
    size <- k$size
    if (abs(ss[1]-ss[2])>min(ss)) {
        over.zero <- cbind(over.zero,clusters)
        if (size[1]>size[2]) {
            over.zero[over.zero[,4]==2,1:2] <- NA
        } else {
            over.zero[over.zero[,4]==1,1:2] <- NA
        }
    }
    over.zero <- over.zero[,-4]
    output <- rbind(over.zero,below.zero)
    output <- output[order(output$row.ID),]
    output <- unname(as.matrix(output[,1:2]))
    return(output)
}