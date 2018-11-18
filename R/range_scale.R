##' Scale a column on 0 - 1
##'
##' A simple scaling operation that is useful for moving coordinates from images space to texture coordinate space.
##' 
##' @title range_scale
##' @param a vector to be scaled
##' @return a scaled on 0 - 1
##' @export
range_scale <- function(a) (a - min(a, na.rm=TRUE)) / diff(range(a, na.rm=TRUE))
