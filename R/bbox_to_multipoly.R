#' Convert a bounding box, defined by min and max x and y, to an `sf` MULTIPOLYGON
#'
#' @param a_bbox an sf bbox object with with `xmin`, `xmax`, `ymin`, `ymax`.
#'
#' @return a MULTIPOLYGON sf geometry.
#' @export
#'
bbox_to_multipoly <- function(a_bbox){
  if (!inherits(a_bbox, "bbox")) stop("bbox must be of class bbox see sf::st_bbox")

  points_mat <- rbind(
      c(a_bbox$xmin, a_bbox$ymin),
      c(a_bbox$xmax, a_bbox$ymin),
      c(a_bbox$xmax, a_bbox$ymax),
      c(a_bbox$xmin, a_bbox$ymax),
      c(a_bbox$xmin, a_bbox$ymin))
  colnames(points_mat) <- NULL

  mpoly <- sf::st_sfc(sf::st_multipolygon(
  list(list(
      points_mat
    ))
  ))
  sf::st_crs(mpoly) <- sf::st_crs(bbox)
  names(mpoly) <- NULL
  mpoly
}
