#' Replace missing landmarks
#' 
#' Replaces missing landmarks with NA. Based on k-means clustering, seeks for separated group of landmarks (for example you can put all missing landmarks in the corner of the picture). Also handles with tpsdig way of writing missing andmarks (-1, -1).
#' 
#' @param data landmark matrix
#' @param method clustering method ('EM' - Expectation-maximization algorithm, 'kmeans' - k-means); default = 'EM'
#' @param clust.parameter minimal proportion of missed landmarks in 'clust method; deafult = 0.45
#' @export
#' @import mclust

missing.landmarks <- function(data,method = 'EM', clust.parameter = 0.45) {
    data <- as.data.frame(data)
    n.landmarks <- dim(data)[1]
    data$row.ID<-1:n.landmarks
    if (method == 'EM') {
        k <- Mclust(data[,1:2],2)
        clusters <- k$classification
        m.uncert <- mean(k$uncertainty)
        size.1 <- length(which(clusters==1))
        size.2 <- length(which(clusters==2))
        if (m.uncert < 0.005) {
            data <- cbind(data,clusters)
            if (size.1>size.2) {
                data[data[,4]==2,1:2] <- NA
            } else {
                data[data[,4]==1,1:2] <- NA
            }
        }
        over.zero <- data
        output <- over.zero[,-4]
    } else {
        if (method == 'kmeans') {
            below.zero <- data[data[,1]<0 | data[,2] < 0,]
            if (dim(below.zero)[1]!=0) {
                below.zero[,1:2] <- NA
            }
            over.zero <- data[data[,1] > 0 | data[,2] > 0,]
            k <- kmeans(over.zero[,1:2],2,nstart = 10)
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
        } else {
            if (method == 'clust') {
                clust <- hclust(dist(data[1:2]))
                class <- cutree(clust,k = 2)
                size.1 <- length(which(class==1))
                size.2 <- length(which(class==2))
                if ((abs(size.2-size.1)/n.landmarks>clust.parameter)) {
                    data <- cbind(data,class)
                    if (size.1>size.2) {
                        data[data[,4]==2,1:2] <- NA
                    } else {
                        data[data[,4]==1,1:2] <- NA
                    }
                }
                over.zero <- data
                output <- over.zero[,-4]
            } else {
                stop("Wrong clustering method")
            }
        }
    }
    output <- output[order(output$row.ID),]
    output <- unname(as.matrix(output[,1:2]))
    return(output)
}