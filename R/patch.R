#' Read the patches bounding boxes
#' 
#' @export
#' @param file chr, the name of the file to read
#' @return sf POLYGON object with one or more patch polygons
read_patch_bbs = function(filename = system.file("patches/patches_bb.gpkg", package = "gstream")){
  sf::read_sf(filename)
}

#' Plot the patches
#' 
#' @export
#' @param x sf table of patches
#' @param what chr, the patches to include in the plot
#' @return invisible NULL
plot_patch_location = function(x = read_patch_bbs(),
                                what = c("cold_blob", "warm_spot")){
  if (!requireNamespace("rnaturalearth")) stop("please install rnaturalearth package first")
  coast = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")
  plot(bb['name'], axes = TRUE, main = "Patches", reset = FALSE)
  plot(sf::st_geometry(coast), add = TRUE, col = "black")
}

#' Read the patch data
#' 
#' @export
#' @param filename chr, the name of the file to read
#' @return tibble of class "patch_month"
read_patch_month = function(filename = system.file("patches/patches_data_month.csv.gz", package = "gstream")){
 x = readr::read_csv(filename, col_types = "Dccnnnnnn")
 class(x) <- c("patch_month", class(x))
 x
}


#' Compute differences of patch_month data
#' 
#' @export
#' @param x tibble of class "patch_month"
#' @param what chr, the name of the variable to plot
#' @return tibble with cold - warm difference
diff_patch_month = function(x, what = "median"){
  
  x |>
    dplyr::select(dplyr::all_of(c("date", "region", "source", what[1]))) |>
    dplyr::group_by(source) |>
    dplyr::group_map(
      function(tbl, key){
        tidyr::pivot_wider(tbl,
                           values_from = dplyr::all_of(what[1]),
                           names_from = dplyr::all_of("region")) |>
          dplyr::mutate(dT = .data$cold_blob - .data$warm_spot)
      }, .keep = TRUE) |>
    dplyr::bind_rows()
}


#' Plot patch_month data
#' 
#' @export
#' @param x tibble of class "patch_month"
#' @param what chr, the name of the variable to plot
#' @return ggplot2 object
plot.patch_month = function(x = read_patch_month(), what = "median"){
  
  what = tolower(what[1])
  if (!what %in% colnames(x)) stop("please specify a variable in x - this wasn't found:", what)
  d = diff_patch_month(x, what = what) |>
    dplyr::group_by(source)
    
  
  ggplot2::ggplot(data = d,
                  mapping = ggplot2::aes(x = date, y = dT )) +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x') + 
    ggplot2::labs(x = "Date", y = "cold - warm (C)",
                  title = sprintf("Cold Blob - Warm Spot for %s", what)) + 
    ggplot2::facet_wrap(~source, scales = "free_y", ncol = 1)
  
}
