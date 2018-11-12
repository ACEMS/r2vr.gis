##' Simple Features Bounding Box to Raster Extent
##'
##' Converts a simple features bounding box to a raster extent.
##' 
##' @title bbox_to_extent
##' @param sf_bbox an object of class bbox created by sf::st_bbox
##' @return a raster extent representing the bbox
##' @export
bbox_to_extent <- function(sf_bbox){
  if (!inherits(sf_bbox, "bbox")) stop("sf_bbox must be of class bbox")
  raster::extent(sf_bbox[c("xmin", "xmax",
                           "ymin", "ymax")])
}
