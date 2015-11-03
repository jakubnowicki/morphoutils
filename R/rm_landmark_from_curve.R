# usuwa wybrany landmark z krzywej, zmienia numerację pozostałych

rm.landmark.from.curve <- function(curve,landmark.nr) {
  curve <- as.vector(curve)
  curve <- curve[curve!=landmark.nr]
  sapply(curve,FUN = function(x,y) {ifelse(x>y,x-1,x)},y=landmark.nr)
}