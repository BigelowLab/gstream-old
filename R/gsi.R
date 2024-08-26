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
#' @param smooth logical, if TRUE add a smoothing line (for timeseries only)
#' @param by chr, one of 'none' (default timeseries), 'month' (ghost plot by month over the years)
#'  or 'year' a box plot by year
#' @return a ggplot2 object
plot.gsi = function(x, smooth = TRUE, by = c("none", "month", "year")[1]){
  

  
  by = tolower(by[1])
  if (by == 'month'){
    
    xx = dplyr::mutate(x,
                      year = format(.data$date, "%Y") |> as.integer(),
                      mon = format(.data$date, "%m") |> as.integer()) 
    yy = dplyr::slice_max(xx, year)
    
    month_format = function(x){ month.abb[x] }
    
    gg = ggplot2::ggplot(data = xx,
                         mapping = ggplot2::aes_(x = ~mon, y = ~Value, group = ~year)) +
      ggplot2::geom_line(color = "grey", alpha = 0.5) +
      ggplot2::geom_line(data = yy,
                         mapping = ggplot2::aes_(x = ~mon, y = ~Value),
                         color = "black") +
      ggplot2::labs(x = "Month", y = "Gulf Stream Index",
                    caption = "data source: https://noaa-edab.github.io/ecodata/") +
      ggplot2::scale_x_continuous(label = month_format)
      
  } else if (by == "year"){
    
    xx = dplyr::mutate(x,
                       year = format(.data$date, "%Y") |> as.numeric()) 
    mm = range(xx$year)
    gg = ggplot2::ggplot(data = xx, 
                         mapping = ggplot2::aes_(x = ~year, y = ~Value, group = ~year)) + 
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Year", y = "GSI", 
                    title = "Gulf Stream Index",
                    caption = "data source: https://noaa-edab.github.io/ecodata/") +
      ggplot2::scale_x_continuous(breaks = seq(from = mm[1]-5, to = mm[2]+5, by = 10))
    
  } else {
    
    gg = ggplot2::ggplot(data = dplyr::group_by(x, .data$Var), 
                         mapping = ggplot2::aes(x = .data$date, y = .data$Value)) + 
      ggplot2::geom_line() +
      ggplot2::labs(x = "Date", y = "GSI", 
                    title = "Gulf Stream Index",
                    caption = "data source: https://noaa-edab.github.io/ecodata/")
    if (smooth){
      gg = gg + 
        ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
    }
   
  }
  gg + ggplot2::facet_wrap(~.data$Var)

}

