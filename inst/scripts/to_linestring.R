library(sf)
library(dplyr)
library(FastKNN)
library(gstream)

#' Convert a MULTIPOINT to a LINESTRING but an attempt to order
#' from lower left (SW) to upper right (NE)
#' 
#' @param x a MULTIPOINT object
#' @return a LINESTRING object
as_ordered_linestring = function(x = read_usn(year = 2020)){
  
  x = dplyr::rowwise(x)|>
    dplyr::group_map(
      function(tbl, key){
        
        p = dplyr::select(tbl, "geometry") |>
          sf::st_cast("POINT") 
        p = dplyr::bind_cols(p,
                             sf::st_coordinates(p) |>
                               dplyr::as_tibble() |>
                               dplyr::select(dplyr::all_of(c("X", "Y")))) |>
          dplyr::arrange(.data$X, .data$Y) |>
          dplyr::select(-dplyr::all_of(c("X", "Y"))) |>
          dplyr::mutate(orig = seq_len(dplyr::n()), 
                        index = rep(0L, n()),
                        .before = 1) 
        CUR <- p$index[1] <- 1L
        D = sf::st_distance(p)
        
        for (i in seq_len(nrow(p))[-1]){
          
          NEXT = FastKNN::k.nearest.neighbors(CUR, D, k = 1)
          p$index[i] = NEXT
          D[CUR,] <- Inf
          D[,CUR] <- Inf
          CUR = NEXT
          
        }
        
        sf::st_geometry(tbl) <- dplyr::slice(p, p$index) |>
                                 sf::st_geometry() |>
                                 sf::st_combine() |>
                                 sf::st_cast("MULTIPOINT")
        tbl
      }) |>
    dplyr::bind_rows()
}
