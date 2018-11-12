##' Transform the coordinate reference system of a bounding box
##'
##' sf does not let you transform a bounding box without jumping through some hoops. this function wraps those up.
##' 
##' @title bbox_transform
##' @param bbox an sf bbox object.
##' @param crs an sf crs expression. A string, an EPSG code, an object created
##'   with sf::st_crs() @return a bounding box transformed to the coordinate
##'   reference system defined by `crs`
##' @export
bbox_transform <- function(bbox, crs){
  if (!inherits(bbox, "bbox")) stop("bbox must be and sf bbox object. See sf::st_bbox")

  sf::st_as_sfc(bbox) %>%
    sf::st_transform(crs) %>%
    sf::st_bbox()
}
