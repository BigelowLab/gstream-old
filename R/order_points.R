#' Convert an sf object to a s2_lnglat object
#' 
#' @export
#' @param x an SF object to convert
#' @return list of wk_xy object (one per row of x)
sf_as_s2lnglat = function(x = read_usn(ordered = FALSE)){
  dplyr::rowwise(x) |>
    dplyr::group_map(
      function(tbl, key){
        xy = sf::st_coordinates(tbl) |>
          rlang::set_names(c("x", "y")) |>
          s2::as_s2_lnglat()
      }
    )
} 

#' Given a set of s2 vertices, estimate the max search distance
#' 
#' @export
#' @param vertices either something that can be cast to sf's POINT or s2's wk_xy
#' @param p num, the qauntile as a probability - \code{\link[stats]{quantile}}
estimate_max_distance = function(vertices, p = 0.1){
  
  if (inherits(vertices, "wk_xy")){
    m = s2::s2_distance_matrix(vertices, vertices) 
  } else if (inherits(vertices, c("sf", "sfc"))) {
    p = sf::st_cast(vertices, "POINT")
    m = sf::st_distance(p, p)
  } else {
    stop("m must be sf, sfc or wk_xy class")
  }
  m |>
    quantile(probs = p) |>
    unname()
} 

#' Retrieve the order indices from an unordered list of points 
#' 
#' @export
#' @seealso \href{https://gist.github.com/paleolimbot/0be47836de5008f308959923dac02c5b}{Help from R-sig-geo}
#' @param vertices s2_lnglat unorder points
#' @param max_distance numeric, the maximum neighbor distance
#' @return numeric index vector into the input
order_points = function(vertices = sf_as_s2lnglat()[[1]],
                        max_distance = estimate_max_distance(vertices)){
  
  used_indices <- 1L
  last_used_index <- 1L
  ivertices = seq_along(vertices)
  
  while (TRUE) {
    unused_indices <- setdiff(ivertices, used_indices)
    
    closest_indices <- s2::s2_closest_edges(
      vertices[last_used_index],
      vertices[unused_indices],
      k = 1,
      min_distance = 0,
      max_distance = max_distance
    )[[1]]
    
    if (length(closest_indices) == 0) {
      break
    }
    
    last_used_index <- unused_indices[closest_indices[1]]
    used_indices <- c(used_indices, last_used_index)
  }
  
  # did we start in the middle? If so there are more to go.
  if (length(used_indices) != length(vertices)){
    last_used_index <- 1L
    while (TRUE) {
      unused_indices <- setdiff(seq_along(vertices), used_indices)
      
      closest_indices <- s2::s2_closest_edges(
        vertices[last_used_index],
        vertices[unused_indices],
        k = 1,
        min_distance = 0,
        max_distance = max_distance
      )[[1]]
      
      if (length(closest_indices) == 0) {
        break
      }
      
      last_used_index <- unused_indices[closest_indices[1]]
      used_indices <- c(last_used_index, used_indices)
    }
  }
  
  used_indices
}
