#' Convert daily north-south wall pairs to a closed polygon (hopefully one!)
#' 
#' @export
#' @param x sf table of wall data.  Only matched north-south pairs for 
#'   each given data are converted. All others are dropped.
#' @param bb bounding box (or object from which we can get a bounding box) to crop to
#'   or NULL to skip
#' @param include char vector of desired elements to add.  Only "area" and "centroid" are known
#' @param use_s2 logical, if TRUE then turn on use of the s2 engine
#' @return sf table of polygons by date
walls_to_polygons = function(x, bb = NULL, verbose = FALSE,
                             include = c("area", "centroid"),
                             use_s2 = FALSE){
  
  on.exit({
    sf::sf_use_s2(orig_s2)
  })
  
  orig_s2 = sf::sf_use_s2(use_s2)
  
  if (!is.null(bb)) x = crop_usn(x, bb)
  
  if (!is.character(include)) include = "none"
  
  pp = dplyr::group_by(x, date) |>
    dplyr::group_map(
      function(tbl, key){
        if (nrow(tbl) != 2) return(NULL)
        if (verbose) cat("w2p", format(tbl$date[1], "%Y-%m-%d"), "\n")
        tbl = dplyr::arrange(tbl, .data$wall)
        n = sf::st_coordinates(dplyr::filter(tbl, .data$wall == "north"))
        s = sf::st_coordinates(dplyr::filter(tbl, .data$wall == "south"))
        s = s[rev(seq_len(nrow(s))), ]
        m = do.call(rbind, list(n, s, n[1,]))[,1:2]
        p = sf::st_polygon(x = list(m)) |>
          sf::st_cast("POLYGON") |>
          sf::st_sfc(crs = sf::st_crs(tbl)) |>
          sf::st_as_sf() |>
          dplyr::mutate(date = tbl$date[1], .before = 1)
        
        if ("centroid" %in% include){
          xy = suppressWarnings(sf::st_centroid(p)) |>
            sf::st_coordinates()
          p = dplyr::mutate(p, xc = xy[1,1], yc = xy[1,2], .after = 1)
        }
      
        if ("area" %in% include) {
          p = dplyr::mutate(p, area = suppressWarnings(sf::st_area(p)), .after = 1)
        }
        
        p
      }, .keep = TRUE) 
  
    do.call(rbind, pp) |>
      sf::st_set_geometry("geometry")
}



#' Crop USN wall data
#' 
#' @export
#' @param x sf table of wall data.  Only matched north-south pairs for 
#'   each given data are converted. All others are dropped.
#' @param bb bounding box (or object from which we can get a bounding box) to crop
#' @param resolve_multiline char, when \code{type} is LINESTRING a conflict may arise when
#'   a wall is composed of multiple lines, which yields MULTILINESTRING.
#'   Set this to retain just the longest LINESTRING or to use "keep" retain as MULTILINESTRING.
#' @param use_s2 logical, if TRUE then turn on use of the s2 engine
#' @return The input cropped.  If a particular line can not be cropped by s2, then 
#'   that row of the input is dropped
crop_usn = function(x, bb = sf::st_bbox(c(xmin = -71, ymin = 32, xmax = -61, ymax = 45),
                                        crs = 4326) |>
                            sf::st_as_sfc(),
                    resolve_multiline = c("keep", "longest")[2],
                    verbose = FALSE,
                    use_s2 = FALSE){
  
  on.exit(sf::sf_use_s2(orig_s2))
  
  if (FALSE) {
    bb = sf::st_bbox(c(xmin = -71, ymin = 32, xmax = -61, ymax = 45),
                     crs = 4326) |>
      sf::st_as_sfc() |>
      sf::st_as_sf()
    resolve_multiline = c("keep", "longest")[2]
    verbose = FALSE
    use_s2 = FALSE
  }
  
  orig_s2 = sf::sf_use_s2(use_s2)
  
  suppressWarnings(sf::st_crop(x, bb)) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(tbl, key){
        if (verbose) cat("crop_usn", tbl$wall[1], format(tbl$date[1], "%Y-%m-%d"), "\n")
        if (inherits(tbl, "try-error")) return(NULL)
        geomtype = get_geometry_type(tbl)
        if (geomtype == "GEOMETRYCOLLECTION") {
          tbl = sf::st_collection_extract(tbl, "LINESTRING")
        }
        if (geomtype != "LINESTRING" && 
            tolower(resolve_multiline[1]) != "keep"){
          tbl = suppressWarnings(sf::st_cast(tbl, "LINESTRING"))
          ix = sf::st_length(tbl) |>
            which.max()
          tbl = dplyr::slice(tbl, ix)
        }  
        tbl
      }) |>
    dplyr::bind_rows()
  
}


#' Convert a MULTIPOINT to a LINESTRING (or MULTIPOINT) but an attempt to order
#' from lower left (SW) to upper right (NE)
#' 
#' @export
#' @param x a MULTIPOINT object
#' @param type char, one of "LINESTRING" (default) or "MULTIPOINT"
#' @param direction chr one of "any", "eastward" (default), "westward"
#' @param resolve_multiline char, when \code{type} is LINESTRING a conflict may arise when
#'   a wall is composed of multiple lines, which yields MULTILINESTRING.
#'   Set this to keep just the longest LINESTRING or to keep them all as MULTILINESTRING.
#' @return a LINESTRING object
order_usn = function(x = usn_example(ordered = FALSE),
                     type = c("LINESTRING", "MULTIPOINT")[1],
                     direction = c("any","eastward", "westward")[2],
                     resolve_multiline = c("keep", "longest")[1]){
  
  dplyr::rowwise(x)|>
    dplyr::group_map(
      function(tbl, key){
        
        p = sf::st_cast(dplyr::select(tbl, attr(tbl, "sf_column")), "POINT")
        index = sf::st_coordinates(p) |>
          dplyr::as_tibble() |>
          rlang::set_names(c("x", "y")) |>
          s2::as_s2_lnglat() |>
          order_points()
        p = dplyr::slice(p, index)
  
        if (tolower(direction[1]) != "any"){
          xy = sf::st_coordinates(p)
          n = nrow(xy)
          if (tolower(direction[1]) == "eastward"){
            # first more eastern (larger) than last?
            # -70, -50  is fine
            # -50 -70 must be reversed
            if (xy[1,1] > xy[n,1]) p = p[rev(seq_len(n)),]
          } else if (tolower(direction[1]) == "westward"){
            # first point is more easterward (smaller) than last?
            if (xy[1,1] < xy[n,1]) p = p[rev(seq_len(n)),]
          }
        }
      
        p = p |>  
          sf::st_geometry() |>
          sf::st_combine() |>
          sf::st_cast(type)
        
        # test if p is MULTILINESTRING, that the user wants LINESTRING and that
        # the user chooses not to keep MULTILINESTRING
        if (tolower(resolve_multiline[1]) != "keep" &&
            get_geometry_type(p) != "LINESTRING" && 
            toupper(type[1]) == "LINESTRING"){
          p = sf::st_cast(p, "LINESTRING")
          ix = which.max(sf::st_length(p))
          p = p[ix]
        }   
        sf::st_geometry(tbl) <- p
                                  
        tbl
      }) |>
    dplyr::bind_rows()
  
  
}

textg = function(x, ..., what = c("all", "ends")){
  if ("ends" %in% what){
    p = sf::st_cast(dplyr::select(x, "geometry"), "POINT")
    n = nrow(p)
    text(dplyr::slice(p,1), labels = 1, ...)
    text(dplyr::slice(p,n), labels = n, ...)
  } else {
    text(p, ...)
  }
}
plotg = function(x, ...){
  plot(sf::st_geometry(x), ...)
}
plot2 = function(x, y){
  plotg(x, axes = T, type = "l", reset = FALSE)
  plotg(y, add = TRUE, color = "orange")
}

#' Remove duplicates (by date and wall)
#'
#' We just keep the first occruence for each duplicate
#' 
#' @export
#' @param x sf MUTLIPOINT table
#' @return the input with duplicates dropped
deduplicate_usn = function(x = read_usn()){
  is_dup = function(x){
    tag = paste(format(x$date, "%Y-%m-%d"), x$wall)
    duplicated(tag)
  }
  dplyr::filter(x, !is_dup(x))
}

#' List USN files
#' 
#' @export
#' @return cahracter vector of filenames
list_usn = function(what = c("orig", "ordered")){
  path = system.file("usn", what[1], package = 'gstream')
  list.files(path, full.names = TRUE)
}

#' Read one or more US Navy wall data files
#' 
#' @export
#' @param year num or char, one or more years to read, or "all" for reading them
#'  all.
#' @param what char, if "ordered" then read the ordered data, or "orig" for unordered.
#' @param deduplicate logical, if TRUE remove duplicates by date-wall
#' @return SF multipoint table
read_usn = function(year = "all", what = "ordered", deduplicate = (what != "ordered")){
  
  files = list_usn(what = what)
  
  if (inherits(year, "Date") || inherits(year, "POSIXt")){
    year = format(year, "%Y")
  } else if (!inherits(year, "character")){
    year = sprintf("%0.4i", year)
  }
  
  if (!("all" %in% year)){
    fileyears = sub(".gpkg", "", basename(files), fixed = TRUE)
    ix = fileyears %in% year
    files = files[ix]
  }
  
  x = lapply(files, sf::read_sf) |>
    dplyr::bind_rows()
  
  if (deduplicate) x = deduplicate_usn(x)
  x
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


#' Retrieve a prepared example file of the north wall
#' 
#' @export
#' @param filename char, the name of the file
#' @param type char, one of "LINESTRING" (default) or "MULTIPOINT"
#' @param ordered logical, if TRUE try to reorder the points
#' @return sf table
usn_example = function(filename = system.file("examples/2020-12-19-north.gpkg",
                                              package = "gstream"),
                       type = c("LINESTRING", "MULTIPOINT")[1],
                       ordered = TRUE){
  x = sf::read_sf(filename)
  if (type == "LINESTRING") x = sf::st_cast(x, "LINESTRING")
  if (ordered) x = order_usn(x, type = type)
  x
}

#' Convert USN LINESTRING features to sfnetworks
#'
#' @export
#' @param x sf LINESTRING table
#' @return list of networks, one per input row
usn_to_network = function(x = usn_example()){
  crs = sf::st_crs(x)
  nets = dplyr::rowwise(x) |>
    dplyr::group_map(
      function(tbl, key){
        nodes = dplyr::select(tbl, attr(x, "sf_column")) |>
          sf::st_cast("POINT")
        ix = seq_len(nrow(nodes))
        nix = length(ix)
        
        edges = sapply(seq_len(nix),
          function(i){
            nexti = ifelse(i < nix, i + 1, 1)
            dplyr::slice(nodes, c(i, nexti)) |> 
              sf::st_combine() |>
              sf::st_cast("LINESTRING")
          }) |>
          sf::st_as_sfc(crs = crs) |>
          sf::st_as_sf()
        n = nrow(edges)
        edges$from = seq_len(n)
        edges$to = c(seq_len(n-1) + 1, 1)

        sfnetworks::sfnetwork(nodes, edges, message = FALSE)
      }) # group_map_function
  nets
}
