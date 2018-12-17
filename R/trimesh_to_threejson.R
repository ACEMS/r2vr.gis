##' Generate threejs model JSON from a triangular mesh.
##'
##' The format of threejs model JSON is described here: https://github.com/mrdoob/three.js/wiki/JSON-Model-format-3
##' In this format each face is described by lists of indicies into lists of vertices, colours and normals. Colours, normals and vertices can be re-used or shared among faces so there are no length requirements on `vertices`, `colours`, or `normals`. All the `face_` arguments must have the same length or be not supplied.
##' @title trimesh_to_threejson
##' @param vertices a matrix of vertices with 3 columns corresponding to: x, y, z.
##' @param face_vertices a matrix with 3 columns of faces as defined by their vertices. column 1 is the row index of a vertex in `vertices`, column 2 is a second row index in of a vertex, and so on for column 3, making up three vertices of a triangular face.
##' @param colours a character vector of colours of form: '0x7cccba' (hex)
##' @param face_vertex_colours a matrix with 3 columns of face vertex colour indicies in `colours`, each row corresponds to a face in `face_vertices`. Row 1, column 1 is the colour index of vertex 1 in face 1.
##' @param normals a 3 column matrix of normal vectors in x, y, z space.
##' @param face_vertex_normals a matrix with 3 columns of face vertex normal indicies in `normals`, each row corresponds to a face in `face_vertices`. Row 1, column 1 is the normal index of vertex 1 in face 1.
##' @param vertex_uvs A 2 column matrix of texture coordinates, the same length as `vertices`.
##' @param texture_file An path to an image file.
##' @param transparency A numeric value between 0 and 1 indicating to what
##'   degree the model should be transparent. 1.0 is fully opaque, 0 is fully
##'   transparent.
##' @return JSON describing the mesh.
##' @export
trimesh_to_threejson <- function(vertices, face_vertices,
                                 colours, face_vertex_colours,
                                 normals, face_vertex_normals,
                                 vertex_uvs, texture_file,
                                 transparency = 1.0) {

 ## format from: https://github.com/mrdoob/three.js/wiki/JSON-Model-format-3
 json_template <- '{
    "metadata": { "formatVersion" : 3 },

    "materials": [ {"DbgColor": 15597568,
      "DbgIndex": 1,
      "DbgName": "land",
      "blending": "NormalBlending",
      "colorDiffuse": [1, 1, 1],
      "colorSpecular": [1, 1, 1],
      ${texture_map}
      "depthTest": true,
      "depthWrite": true,
      "shading": "Phong",
      "specularCoef": 0.0,
      "transparency": ${transparency},
      "transparent": false,
      "vertexColors": 2}],
    "vertices": [ ${vertices} ],
    "normals":  [ ${normals} ],
    "colors":   [ ${colors} ],
    "uvs":      [ ${uvs} ],
    "faces": [
      ${faces}
      ]}'

  ## we are just using a single material
  material_ind <- 0

  ## calculate face definition byte
  face_def <-
    0 +                                   # use triangular faces
    2^1 +                                 # use material for face
    ((!missing(face_vertex_colours)) * 2^7) +    # use face vertex colours
    ((!missing(face_vertex_normals)) * 2^5) +    # use face vertex normals
    ((!missing(vertex_uvs)) * 2^3)      # use face vertex uv

  threejs_json_data <-
    new.env()

  ## transparency
  three_json_data$transparency <- transparency

  ## vertices
  if (!is.matrix(vertices) || (ncol(vertices) != 3)){
    stop("vertices is not a 3 column matrix")
  }
  if (anyNA(vertices)){
    stop("vertices cannot contain NAs")
  }

  ## face - vertex mapping
  ## check supplied pieces conform to expectations
  if (!is.matrix(face_vertices) || (ncol(face_vertices) != 3) ){
    stop("face_vertices is not a 3 column matrix")
  }
  if (anyNA(vertices)){
    stop("face_vertices cannot contain NAs") 
  }

  ## threejs/VR specific transformations
  ## start face_vertices indicies from 0
  ## Our face_vertices is set up to index vertices. In R the indicies start
  ## from 1, but in the threejs JSON they need to start from 0. It's a simple
  ## transform:
  face_vertices <- face_vertices - 1

  ## centre model coordinates
  ## We would like to have a model scale in metres so to match that of VR.
  ## I might be practical to scale the model size when rendering, but at least
  ## if the scale start the same, we will know the scaling factor.
  ## Since the 'metres' we have come from a position on the globe, they can be
  ## quite large in magnitude. This causes vertices to render very far away
  ## from the camera, usually positioned close to 0,0,0.
  vertices <- scale(vertices, center = TRUE, scale = FALSE)

  ## create vertex list
  threejs_json_data$vertices <-
    vertices %>%
    tibble::as_data_frame() %>%
    purrr::transpose() %>%
    purrr::map( ~paste0(., collapse = ',')) %>%
    paste0( ., collapse = ', ')

  ## colours
  if (missing(colours)){
    colours <- ""
  }
  threejs_json_data$colors <-
    colours %>%
    paste0(., collapse=", ")

  ## normals
  if (missing(normals)){
    normals <- ""
  }
  else {
    if (!is.matrix(normals) || ncol(normals) != 3) {
      stop("normals is not a 3 column matrix")
    }
    if (anyNA(normals)) {
      stop("normals cannot contain NAs")
    }
    ## format normals
    normals <-
      paste0(
        paste0(normals[1], ",", normals[2], ",", normals[3]),
        collapse = ", "
      )
  }

  threejs_json_data$normals <- normals

  ## uvs
  threejs_json_data$uvs <- ""
  threejs_json_data$texture_map <- ""
  if (!missing(vertex_uvs)){
    if ((!is.matrix(vertex_uvs) || (nrow(vertices) != nrow(vertex_uvs)) ||
         (ncol(vertex_uvs) != 2) || missing(texture_file))
        ){
      stop("vertex_uvs is needs to be a 2 column matrix of the same length as vertices supplied with an image in texture_file")
    }
    if(anyNA(vertex_uvs)) {
      stop("vertex uvs cannot contain NAs") 
    }
    ## looking good, set up the uvs.
    threejs_json_data$uvs <-
      paste0("[",
             paste0(vertex_uvs[,1], ",", vertex_uvs[,2], collapse = ", "),
             "]")

    ## from https://stackoverflow.com/questions/14872502/jsonloader-with-texture
    threejs_json_data$texture_map <- paste0("\"mapDiffuse\": \"", texture_file, "\",")
  }

  if (!missing(face_vertex_colours)) {
    if (!is.matrix(face_vertex_colours) || (nrow(face_vertex_colours) != nrow(face_vertices))){
      stop("face_vertex_colours is not a 3 column matrix of same length as face_vertices.")
    }
    if (anyNA(face_vertex_colours)){
      stop("face_vertex_colours cannot contain NAs")
    }
  }

  if (!missing(face_vertex_normals)){
    if (!is.matrix(face_vertex_normals) || (nrow(face_vertex_normals) != nrow(face_vertices))){
      stop("face_vertex_normals is not a 3 column matrix of same length as face_vertices.")
    }
    if (anyNA(face_vertex_normals)){
      stop("face_vertex_normals cannont contain NAs")
    }
  }

  ## faces
  ## Build up each face by pasting together the parts that were supplied. Taking advantage
  ## of vectorised paste0()
  faces <-
    paste0(face_def, ", ", face_vertices[, 1], ",", face_vertices[, 2], ",",
           face_vertices[, 3], ", ", material_ind)

  if(!missing(vertex_uvs)){
    ## The uv's have the same dimension as vertices, since we assume each vertex
    ## has a unique texture coordinate.
    faces <-
      paste0(faces,", ", face_vertices[, 1], ",", face_vertices[, 2], ",",
    face_vertices[, 3])
  }

  if(!missing(face_vertex_normals)){
    faces <-
      paste0(faces, ", ", face_vertex_normals[, 1], "," , face_vertex_normals[, 2], ",",
             face_vertex_normals[, 3])
  }

  if(!missing(face_vertex_colours)){
    faces <-
      paste0(faces, ", ", face_vertex_colours[, 1], ",", face_vertex_colours[, 2], ",",
             face_vertex_colours[, 3])
  }

  ## create one flat list
  threejs_json_data$faces <- paste0(faces, collapse = ", ")

  ## interpolate the JSON string template
  stringr::str_interp(string = json_template, threejs_json_data)
}
