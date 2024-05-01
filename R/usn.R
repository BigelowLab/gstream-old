#' List USN files
#' 
#' @export
#' @return cahracter vector of filenames
list_usn = function(){
  path = system.file("usn", package = 'gstream')
  list.files(path, full.names = TRUE)
}

#' Read one or more US Navy wall data files
#' 
#' @export
#' @param year num or char, one or more years to read, or "all" for reading them
#'  all.
#' @return SF multipoint table
read_usn = function(year = "all"){
  
  files = list_usn()
  
  if (!inherits(year, "character")){
    year = sprintf("%0.4i", year)
  }
  
  if (!("all" %in% year)){
    fileyears = sub(".geojson", "", basename(files), fixed = TRUE)
    ix = fileyears %in% year
    files = files[ix]
  }
  
  lapply(files, sf::read_sf) |>
    dplyr::bind_rows()
}



#' Extract data out of the text line with date info
#' 
#' @export
#' @param x cahr, the line of text with the date
#' @return Date class object
get_date_usn = function(x){
  fmt = if (grepl("^RMKS/1", x[1])) {
    "RMKS/1. GULF STREAM NORTH WALL DATA FOR %d %B %y:"
  } else {
    "GULF STREAM NORTH WALL DATA FOR %d %B %y:"
  }
    
  as.Date(x, format = fmt)
}

#' Given one or more formatted strings, retrieve a matrix of lon,lat
#' 
#' @export
#' @param char, one or more character strings of point data as scraped from the URL
#' @return matrix of point in lon, lat order
extract_points_usn = function(x = c("26.5N79.8W 26.7N79.8W 26.9N79.8W 27.0N79.8W 27.2N79.8W 27.3N79.9W", 
                                "27.4N79.9W 27.5N80.0W 27.6N80.0W 27.8N80.1W 28.0N80.1W 28.1N80.2W")){
  r = lapply(strsplit(x, " ", fixed = TRUE),
             function(s){
               lat = as.numeric(substring(s, 1, 4)) * ifelse(substring(s, 5, 5) == "N", 1, -1)
               lon = as.numeric(substring(s, 6, 9)) * ifelse(substring(s, 10,10) == "W", -1, 1)
               cbind(lon, lat) 
             })
  do.call(rbind, r)
} 

#' Read the GS wall data in usn format
#' 
#' @export
#' @param filename char, the name of file, a vector of files, or the nameof a
#'   single tar file (with one or more *.sub files within)
#' @param verbose logical, if TRUE output messages
#' @return tibble or sf MULTIPOINT object
read_wall_data_usn = function(filename, verbose = FALSE){

  if (grepl("^.*\\.tar$", filename[1])) {
    # tar file
    tmpdir = file.path(dirname(filename[1]), "usn-temp")
    if (!dir.exists(tmpdir)) ok = dir.create(tmpdir)
    ok = untar(filename, exdir = tmpdir)
    filenames = list.files(tmpdir, pattern = "^.*\\.sub$", full.names = TRUE)
    xx = lapply(filenames, read_wall_data_usn, verbose = verbose) |>
      dplyr::bind_rows()
    ok = file.remove(filenames)
    unlink(tmpdir, force = TRUE, recursive = TRUE)
    return(xx)
  } else if (length(filename) > 1){
    # vector of filenames
    xx = lapply(filename, read_wall_data_usn, verbose = verbose) |>
      dplyr::bind_rows()
    return(xx)
  }

  if (verbose) cat("reading:", basename(filename[1]), "\n")
  string = readLines(filename)
  
  # kluge to fix garbled files like "gs023nw.sub" and empty files
  if (!dplyr::between(length(string), 50, 500)) {
    if (verbose) cat(filename, "has", length(string), "lines.  Skipping\n") 
    return(NULL)
  }
  
  
  ix = grep("GULF STREAM NORTH WALL DATA", string, fixed = TRUE) |>
    tail(n=1L)
  iy = grep("GULF STREAM SOUTH WALL DATA", string, fixed = TRUE) |>
    tail(n = 1L)
  iz = grep("^ ", string) |> tail(n = 1L)
  if (length(iy) == 0 && length(ix) > 0){
    # north wall only
    nindex = seq(from = ix + 1, to = iz, by = 1)
    sindex = NULL
    date = get_date_usn(string[ix])
  } else if (length(ix)== 0 && length(iy > 0)){
    # south wall only
    nindex = NULL
    sindex = seq(from = iy + 1, to = iz, by = 1)
    date = get_date_usn(string[iy])
  } else if (ix < iy){
    # north first
    nindex = seq(from = ix + 1, to = iy - 1, by = 1)
    sindex = seq(from = iy + 1, to = iz, by = 1)
    date = get_date_usn(string[ix])
  } else if (ix > iy){
    # south first
    nindex = seq(from = ix + 1, to = iz, by = 1)
    sindex = seq(from = iy + 1, to = ix - 1, by = 1)
    date = get_date_usn(string[ix])
  } else {
    if (verbose) cat(filename, "has errors.  Skipping\n")
    return(NULL)
  }
  
  
  nwall = if (!is.null(nindex)){
   string[nindex] |>
    trimws(which = "both") |>
    extract_points_usn() |>
    dplyr::as_tibble()  |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
    sf::st_union() |>
    sf::st_as_sf() |>
    dplyr::mutate(date = date, wall = "north", .before = 1)
  } else {
    NULL
  }
  swall = if (!is.null(sindex)){
    string[sindex] |>
      trimws(which = "both") |>
      extract_points_usn() |>
      dplyr::as_tibble() |>
      sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) |>
      sf::st_union() |>
      sf::st_as_sf() |>
      dplyr::mutate(date = date, wall = "south",.before = 1)
  } else {
    NULL
  }
  dplyr::bind_rows(nwall, swall) 
}



