#' Import GSI data from the [ecoodata](https://noaa-edab.github.io/ecodata/index.html) package.
#' 
#' @export
#' @return tibble pf class "gsi"
read_gsi = function(){
  if (requireNamespace("ecodata")){
    x = ecodata::gsi
    dt = sprintf("%0.2f.01", x$Time)
    dt = gsub(".", "-", dt, fixed = TRUE)
    x = x |>
      dplyr::mutate(date = as.Date(dt, format = "%Y-%m-%d"), .before = 1)
  } else {
    stop("please first install the ecodata package from github")
  }
  class(x) <- c("gsi", class(x))
  x
}


#' Plot gsi data time series
#' 
#' @export
#' @param x the gsi data
#' @param smooth logical, if TRUE add a smoothing line
#' @return a ggplot2 object
plot.gsi = function(x, smooth = TRUE){
  x = dplyr::group_by(x, .data$Var)
  gg = ggplot2::ggplot(data = x, 
                       mapping = ggplot2::aes(x = .data$date, y = .data$Value)) + 
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "GSI", 
                  title = "Gulf Stream Index",
                  caption = "data source: https://noaa-edab.github.io/ecodata/")
  if (smooth){
    gg = gg + 
      ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
  }
  gg + 
    ggplot2::facet_wrap(~.data$Var)
}

