##' Crop Raster to Simple Features Bounding Box
##'
##' The bounding box is converted to a raster::extent which is used to crop the
##' raster. The bounding box need not have the same CRS as the raster, it is
##' transformed to the raster's CRS automatically.
##' 
##' @title raster_crop_bbox
##' @param a_raster a Raster from the 'raster' package
##' @param sf_bbox a object of class 'bbox' created by sf::st_bbox()
##' @return a_raster cropped to the extent of sf_bbox
##' @export
raster_crop_bbox <- function(a_raster, sf_bbox){
  if (!inherits(a_raster, "Raster")) stop("a_raster must be a Raster class")
  if (!inherits(sf_bbox, "bbox")) stop("sf_bbox must be of class bbox")

  ## convert the bbox to the coord system of the raster
  raster_crs <- raster::crs(a_raster)@projargs

  bbox_raster_crs <-
    sf::st_transform(sf::st_as_sfc(sf_bbox), raster_crs) %>%
    sf::st_bbox()

  extent_raster_crs <- bbox_to_extent(bbox_raster_crs)

  raster::crop(a_raster, extent_raster_crs)
}
