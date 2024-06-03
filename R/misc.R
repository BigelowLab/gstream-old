#' Read configuration
#' 
#' @export
#' @param filename char, the config file (with path)
#' @return configuration list
read_configuration = function(filename = "~/.gstream"){
  yaml::read_yaml(filename)
}


#' Get geometry type code
#' 
#' @export
#' @param x sf or sfc object
#' @param recursive logical, if TRUE drill down to get the type for each
#'   feature.
#' @return character vector such as "POINT" or "POLYGON"
get_geometry_type <- function(x, recursive = FALSE){
  if (recursive[1]){
    klass <- sapply(sf::st_geometry(x), class)
  } else {
    klass <- sf::st_geometry(x) |>
      class()
  }
  sub("sfc_", "", klass[1])
}

#' Compute distances between consecutive points along a LINESTRING path.
#' 
#' @param x sf LINESTRING object
#' @return a list of path lengths, one per row
st_path_lengths = function(x = read_usn(year = "2020") |> 
                             dplyr::filter(date == as.Date("2020-12-19"), 
                                           wall == "north") |>
                             order_usn(), 
                           ...){
  stopifnot(get_geometry_type(x) == "LINESTRING")
  dplyr::rowwise(x) |>
    dplyr::group_map(
      function(tbl, key){
        p = sf::st_cast(tbl, "POINT")
        d = rep(NA_real_, nrow(tbl))
        m = sf::st_distance(p)
        for (i in seq_len(nrow(p)-1)) d[i] = m[i,i+1]
        d
      })
}
