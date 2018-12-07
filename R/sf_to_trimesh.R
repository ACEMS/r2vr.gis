#' Convert a simple features geometry containing a single multipolygon to a triagulated mesh.
#' 
#' Uses RTriangle for Constrained Delaunay Triangulation. Can handle complex shapes comprised of multiple polygons with holes.
#  Format your geometry with st_union() or st_combine() before triangulation to handle internal lines/borders as desired.
#'
#' @param a_mulitpoly_sf a simple features (sf) geometry containing a single multipolygon.
#' @param n_tris approximate number of triangles to uses. This argument is used to set the max triangle size in RTriangle as 
#' `polygon bounding box area`/`n_tris`. 
#' @export
#'
sf_to_trimesh <- function(a_mulitpoly_sf, n_tris = NULL){
  
  if(!methods::is(a_mulitpoly_sf, "sfc_MULTIPOLYGON")){
    stop("sf_to_tri_mesh can only work with sf geometry containing a single MULTIPOLYGON") 
  } 
  if(length(a_mulitpoly_sf) != 1){
    stop("Argument geomerty contained more than 1 MULTIPOLYGON. Use st_union() or st_combine()") 
  }

    # For RTRiangle we need:
    # P - A list of all unique vertices
    # PB - A vector the same length as P indicating if vertex is on boundary
    # PA - not required but maybe be useful for rastersation. Probably want explicit control.
    # S - a list of segments need boundary segments and hole segments
    #     Uses verex indicie in P.
    # SB - a vector the same length as S indicating boundaries
    # H - a vector of holes points in segments # For RTRiangle we need:
    # P - A list of all unique vertices
    # PB - A vector the same length as P indicating if vertex is on boundary
    # PA - not required but maybe be useful for rastersation. Probably want explicit control.
    # S - a list of segments need boundary segments and hole segments
    #     Uses verex indicie in P.
    # SB - a vector the same length as S indicating boundaries
    # H - a vector of holes points in segments

  island_list <-
    purrr::map(a_mulitpoly_sf[[1]], ~.[1]) %>% 
    purrr::flatten() %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::map(~dplyr::mutate(., type = "island"))
  
  hole_list <-
    purrr::map(a_mulitpoly_sf[[1]], ~.[-1]) %>%
    purrr::flatten() %>%
    purrr::map(tibble::as_tibble) %>%
    purrr::map(~dplyr::mutate(., type = "hole"))
  
  all_polys_list <- c(island_list, hole_list)
  all_polys_list <-
    purrr::pmap(list(all_polys_list, seq_along(all_polys_list)),
      function(polygon_df, group_id){
        dplyr::mutate(polygon_df, group = group_id)
      }
    )

  vertex_df <- 
    dplyr::bind_rows(all_polys_list) %>%
    dplyr::rename(x = V1, y = V2)
  
  unique_vertices <- 
    vertex_df %>%
    dplyr::select(x, y) %>%
    unique() %>%
    dplyr::mutate(id = seq_along(x))
  
  # Df containing P, PB, S, SB, where PB = SB
  segment_boundary_df <- 
    dplyr::left_join(vertex_df, unique_vertices, by = c("x","y")) %>%
    dplyr::group_by(group) %>%
    dplyr::mutate(segment_start = id,
           segment_end = dplyr::lead(id),
           boundary_ind = dplyr::if_else(type == "island", 1, 0)) %>%
    dplyr::ungroup()
  
  # Have NAs in segments, fine but before we drop those we need the closed 
  # vertex rings in x,y to calculate some centroids. 
  hole_centroids <-
    segment_boundary_df %>%
    dplyr::filter(type == "hole") %>%
    dplyr::group_by(group) %>%
    dplyr::summarise(centroid = list( 
      sf::st_centroid(sf::st_polygon( list( as.matrix(cbind(x,y)) ) )) )) %>%
    dplyr::pull(centroid) %>%
    purrr::map(as.matrix) %>%
    do.call(rbind, .)

  # Drop segments that contain NAs
  segment_boundary_df <- tidyr::drop_na(segment_boundary_df)

  # This fixed some numerical issues with small coordinate scales
  vertex_boundary_df <-
    segment_boundary_df %>%
    dplyr::select(x,y,boundary_ind) %>%
    unique()  

  rtri_args <- 
    list(
      P = vertex_boundary_df %>%
         dplyr::select(x, y) %>%
         as.matrix(),   
      PB = dplyr::pull(vertex_boundary_df, boundary_ind),
      S = segment_boundary_df %>%
          dplyr::select(segment_start, segment_end) %>%
          as.matrix(),
      SB = dplyr::pull(segment_boundary_df, boundary_ind),
      H = if(is.null(hole_centroids)) NA else hole_centroids
      )
  
  # Calculate the triangle area to give approx n_tris
  if (!is.null(n_tris)){
    bbox <- sf::st_bbox(a_mulitpoly_sf)
    area <- (bbox[3] - bbox[1]) * (bbox[4] - bbox[2])
    tri_area <- area/n_tris
  } else {
    tri_area <- NULL
  }


  rt_pslg <- do.call(RTriangle::pslg, rtri_args)
  
  rt_triangles <- RTriangle::triangulate(rt_pslg, a = tri_area)
}
