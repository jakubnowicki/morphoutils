# usuwa wybrany landmark z krzywej, zmienia numerację pozostałych
#' Remove selected landmark
#' 
#' Function removes selected landmark from a curve and changes numbers of remaining ones
#' @param curve Curve to change
#' @param landmark.nr Number of landmark to remove
#' @export
#' @examples rm.landmark.from.curve 

rm.landmark.from.curve <- function(curve,landmark.nr) {
  curve <- as.vector(curve)
  curve <- curve[curve!=landmark.nr]
  sapply(curve,FUN = function(x,y) {ifelse(x>y,x-1,x)},y=landmark.nr)
}