#' Read the moc_transport data file
#' 
#' @export
#' @param filename the name of the file to read
#' @return a tibble of class "rapid_moc_transports"
read_moc_transports = function(
    filename = system.file("rapid-amoc/moc_transports.nc", package = "gstream")){
  
  on.exit({
    ncdf4::nc_close(nc)
  })
  
  nc = ncdf4::nc_open(filename)
  date = as.Date("2004-01-01") + as.vector(nc$dim$time$vals)
  
  x = sapply(names(nc$var),
             function(vname) {
               ncdf4::ncvar_get(nc, vname, start = 1, count = length(date)) |>
                 as.vector()
             },
             simplify = FALSE) |>
  dplyr::as_tibble() |>
  dplyr::mutate(date = date, 
                .before = 1)
  
  u = sapply(names(nc$var),
             function(vname) {
                nc$var[[vname]]$units
             },
             simplify = FALSE) |>
    unname()
  
  longnames = readr::read_csv(system.file("rapid-amoc/moc_transports_lut.csv", package = "gstream"),
                              col_types = "cc")

  attr(x, "longnames") <- longnames
  attr(x, "units") <- u
  class(x) <- c("rapid_moc_transports", class(x))
  x
}

#' Plot the moc_transports data set
#' 
#' @export
#' @param x moc_transports data frame (class of "rapid_moc_transports")
#' @return ggplot2 object
plot.rapid_moc_transports = function(x = read_moc_transports()){
  lutable = attr(x, "longnames")
  lut = lutable$longname
  names(lut) = lutable$name
  # transform from wide to long
  x = tidyr::pivot_longer(x,
                          !dplyr::any_of("date"),
                          names_to = "name",
                          values_to = "value") |>
    dplyr::mutate(name = factor(lut[.data$name], levels = lutable$longname))
  
  ggplot2::ggplot(data = x, 
                  mapping = ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line() + 
    ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x') + 
    ggplot2::facet_wrap(~name, scales = "free_y", ncol = 2)
  
}

