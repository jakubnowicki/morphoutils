#' Convert multiple landmark numbers
#' 
#' Function to convert a set of landmark numbers between asymmetric and symmetrized version.
#' @param landmark.vector Vector of landmark numbers
#' @param mid Midline landmarks
#' @param left Leftside landmark
#' @param right Righside landmarks
#' @param asym.to.sym Direction of conversion. Default from asymetric to symmetrized
#' @keywords landmarks, conversion
#' @export

convert.nr.vector <- function(landmark.vector,mid,left,right,asym.to.sym = TRUE) {
  for (i in 1:length(landmark.vector)) {
    landmark.vector[i] <- convert.nr(landmark = landmark.vector[i],mid,left,right,asym.to.sym)
  }
  return(landmark.vector)
}