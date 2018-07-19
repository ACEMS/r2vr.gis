#' Convert a bounding box, defined by min and max latitude and longitude, to an `sf` MULTIPOLYGON
#'
#' @param a_bbox a four number named vector with `xmin`, `xmax`, `ymin`, `ymax`
#' @param crs string to set for the returned multipolygon geometry. 
#'
#' @return a MULTIPOLYGON sf geometry.
#' @export
#'
bbox_to_multipoly <- function(a_bbox, crs){
  points_mat <- rbind(
      c(a_bbox$xmin, a_bbox$ymin),
      c(a_bbox$xmax, a_bbox$ymin),
      c(a_bbox$xmax, a_bbox$ymax),
      c(a_bbox$xmin, a_bbox$ymax),
      c(a_bbox$xmin, a_bbox$ymin))
  colnames(points_mat) <- NULL
  
  mpoly <- st_sfc(st_multipolygon(
  list(list(
      points_mat  
    ))
  ))
  st_crs(mpoly) <- crs
  names(mpoly) <- NULL
  mpoly
}