#' Convert landmark number
#' 
#' A function to convert landmark number between asymmetric and symmetrized version.
#' @param landmark Landmark number
#' @param mid Midline landmarks
#' @param left Leftside landmark
#' @param right Righside landmarks
#' @param asym.to.sym Direction of conversion. Default from asymetric to symmetrized
#' @keywords landmarks, conversion
#' @export
#' @examples
#' convert.nr()

convert.nr <- function(landmark,mid,left,right,asym.to.sym = TRUE) {
  asym <- c(mid,right,left)
  sym <- seq_along(asym)
  if (asym.to.sym==TRUE) {
    new.nr <- sym[which(asym==landmark)]
  }
  else
  {
    new.nr <- asym[which(sym==landmark)]
  }
  return(new.nr)
}