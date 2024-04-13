#' Extract data out of the text line with date info
#' 
#' @param x cahr, the line of text with the date
#' @return Date class object
get_date = function(x){
  as.Date(x, format = "RMKS/1. GULF STREAM NORTH WALL DATA FOR %d %B %y:")
}

#' Given one or more formatted strings, retrieve a matrix of lon,lat
#' 
#' @param char, one or more character strings of point data as scraped from the URL
#' @return matrix of point in lon, lat order
extract_points = function(x = c("26.5N79.8W 26.7N79.8W 26.9N79.8W 27.0N79.8W 27.2N79.8W 27.3N79.9W", 
                                "27.4N79.9W 27.5N80.0W 27.6N80.0W 27.8N80.1W 28.0N80.1W 28.1N80.2W")){
  r = lapply(strsplit(x, " ", fixed = TRUE),
             function(s){
               lat = as.numeric(substring(s, 1, 4)) * ifelse(substring(s, 5, 5) == "N", 1, -1)
               lon = as.numeric(substring(s, 6, 9)) * ifelse(substring(s, 10,10) == "W", -1, 1)
               cbind(lon, lat) 
             })
  do.call(rbind, r)
} 

#' Get the GS wall data
#' 
#' @param filename char, the name of file
#' @param verbose logical, if TRUE output messages
#' @return tibble or sf MULTIPOINT object
read_wall_data = function(filename, verbose = FALSE){
  if (verbose) cat("reading:", filename, "\n")
  string = readLines(filename)
  
  ix = grep("GULF STREAM NORTH WALL DATA", string, fixed = TRUE)
  iy = grep("GULF STREAM SOUTH WALL DATA", string, fixed = TRUE)
  iz = grep("^ ", string) |> tail(n = 1L)
  
  date = get_date(string[ix])
  
  nwall = string[seq(from = ix + 1, to = iy - 1, by = 1)] |>
    trimws(which = "both") |>
    extract_points() |>
    dplyr::as_tibble()  |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_union() |>
    sf::st_as_sf() |>
    dplyr::mutate(date = date, wall = "north", .before = 1)
  
  
  swall = string[seq(from = iy + 1, to = iz, by = 1)] |>
    trimws(which = "both") |>
    extract_points() |>
    dplyr::as_tibble() |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_union() |>
    sf::st_as_sf() |>
    dplyr::mutate(date = date, wall = "south",.before = 1)
  
  dplyr::bind_rows(nwall, swall) 
}