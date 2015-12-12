#' Remove landmarks from vector
#' 
#' Removes landmarks from vector.
#' 
#' @param vector vector
#' @param landmarks landmarks to remove
#' @export

remove.landmarks.from.vector <- function(vector, landmarks) {
    n <- length(landmarks)
    landmarks <- landmarks[order(-landmarks)]
    for (i in 1:n) {
        vector <- rm.landmark.from.curve(vector, landmarks[i])
    }
    return(vector)
}