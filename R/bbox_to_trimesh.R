##' Triangulate a bounding box into a mesh
##'
##' Triangulate an sf bounding box into a mesh using RTriangle
##'
##' A wrapper for sf_to_trimesh()
##' 
##' @title bbox_to_trimesh
##' @param bbox an sf bbox object
##' @param n_tris a number of triangles to use in the mesh (approximate)
##' @return an RTriangle triangulation object
##' @export
bbox_to_trimesh <- function(bbox, n_tris){
  if (!inherits(bbox, "bbox")) stop("bbox must be of class bbox see sf::st_bbox")

  mpoly_bbox <- bbox_to_multipoly(bbox)
  sf_to_trimesh(mpoly_bbox, n_tris)
}
