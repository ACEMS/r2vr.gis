##' Convert a vector of vertex colour indexes to a matrix of
##' triangle-vertex-face colour indexes.
##'
##' When shading a mesh with colour in the GIS context it is probable that a
##' raster will be used to determine a colour for each vertex. WebVR meshes are
##' shaded at the face level though, so the user is required to provided a
##' mapping to a colour for each vertex in the context of each face.
##'
##' This function takes a vector of colour indexes and a matrix describing
##' triangle faces using vertex indexes and returns a matrix describing triangle
##' faces using colour indexes.
##' 
##' @title vertex_to_face_colours
##' @param vertex_colours a vector of colours, where each colour represents a
##'   colour of a vertex.
##' @param faces a three column matrix of faces described by vertex indicies.
##' @return a matrix with one row per mesh triangle, each column representing a vertex, containing a colour index.
vertex_to_face_colours <- function(vertex_colours, faces){
  matrix(vertex_colours[faces], ncol = 3, byrow = FALSE)
}
