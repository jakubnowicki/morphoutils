# usuwa wybrane punkty z tabeli semilandmarków

change.curves <- function(curves,points.to.remove) {
  break.points <- find.curves(curves)
  curves.list <- matrix.to.list(curves,break.points)
  n.curves <- length(curves.list)
  n.points <- length(points.to.remove)
  for (i in 1:n.points) {
    point <- points.to.remove[1]
    for (j in 1:n.curves) {
      curves.list[[j]] <- rm.landmark.from.curve(curves.list[[j]],point)
    }
    points.to.remove <- rm.landmark.from.curve(curve = points.to.remove,landmark.nr = point)
  }
  curves.fin <- list.to.matrix(curves.list)
  return(curves.fin)
}