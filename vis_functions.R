require(MASS)

contour2d <- function(x, y, ...){
  # Plots the bivariate density using the MASS package, as well as contours
  DENS = kde2d(x, y)
  filled.contour(DENS,plot.axes = {
    axis(1)
    axis(2)
    contour(DENS,add = TRUE)}, ...)
}
