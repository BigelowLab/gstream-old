#' Import (from matlab) the GSGI index data
#' 
#' @export
#' @param file char, the name of the file to import
#' @return a tibble of data and of class "gsgi"
import_gsgi = function(file = "/mnt/s1/projects/ecocast/coredata/gstream/gsi/NOAA_OISST_GS_dSST_index_monthly_1993_2019_release.mat"){
  if (!requireNamespace("R.matlab")){
    stop("R.matlab package is required - please install it first")
  }
  
  x = R.matlab::readMat(file)
  yr = as.vector(x$YR)
  date = sapply(yr,
                function(y){
                  paste(y, sprintf("%0.2i",1:12) , "01", sep = "-")
                }) |>
    as.vector() |>
    as.Date()
  
  r = dplyr::tibble(
    date = date,
    SST.N.deseason = as.vector(x$SST.N.deseason),
    SST.S.deseason = as.vector(x$SST.S.deseason),
    dSST.deseason = as.vector(x$dSST.deseason)
  )
  
  attr(r, "LON") <- as.vector(c(x$LON1, x$LON2))
  r
}



#' Read the GS SST gradient index
#' 
#' @export
#' @param filename char, the name of the file to read
#' @return tibble of date, deseasoned SST north and south and the difference (gradient)
read_gsgi = function(file = system.file("gsgi/gsgi.csv.gz", package = "gstream")){
  x = readr::read_csv(file, col_types = "Dnnn")
  class(x) <- c("gsgi", class(x))
  x
}


#' Plot gsgi data time series
#' 
#' @export
#' @param x the gsgi data
#' @param smooth logical, if TRUE add a smoothing line
#' @param by chr, one of 'none' (default timeseries), 'month' (ghost plot by month over the years)
#'  or 'year' a box plot by year
#' @return a ggplot2 object
plot.gsgi = function(x, smooth = TRUE, by = c("none", "month", "year")[1]){
  
  month_format = function(x){ month.abb[x] }
  
  by = tolower(by[1])
  if (by == "month"){
   
    xx = dplyr::mutate(x, 
                      year = format(date, "%Y") |> as.numeric(), 
                      mon = format(date, "%m") |> as.numeric())
    yy = dplyr::slice_max(xx, year)
    gg = ggplot2::ggplot(data = xx, 
                         mapping = ggplot2::aes_(x = ~mon, y = ~dSST.deseason, group = ~year))  +
      ggplot2::geom_line(color = "grey", alpha = 0.5) + 
      ggplot2::geom_line(data = yy,
                         mapping = ggplot2::aes_(x = ~mon, y = ~dSST.deseason),
                         color = "black", show.legend = TRUE) +
      ggplot2::labs(x = "Month", y = "Gulf Stream SST Gradient Index",
                    caption = "data source: https://www2.whoi.edu/staff/ykwon/data/") +
      ggplot2::scale_x_continuous(label = month_format)
      
     
  } else if (by == "year"){
    
    xx = dplyr::mutate(x,
                       year = format(.data$date, "%Y") |> as.numeric()) 
    mm = range(xx$year)
    gg = ggplot2::ggplot(data = xx, 
                         mapping = ggplot2::aes_(x = ~year, y = ~dSST.deseason, group = ~year)) + 
      ggplot2::geom_boxplot() +
      ggplot2::labs(x = "Year", y = "Gulf Stream SST Gradient Index",
                    caption = "data source: https://www2.whoi.edu/staff/ykwon/data/") + 
      ggplot2::scale_x_continuous(breaks = seq(from = mm[1], to = mm[2], by = 5))
    
    
    
  } else {
  
    gg = ggplot2::ggplot(data = x, ggplot2::aes(x = .data$date, y = .data$dSST.deseason)) + 
      ggplot2::geom_line() +
      ggplot2::labs(x = "Date", y = "dSST (deseasoned, C)", 
                    title = "Gulf Stream SST Gradient Index",
                    caption = "data source: https://www2.whoi.edu/staff/ykwon/data/")
    if (smooth){
      gg = gg + 
        ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
    }
  }
  gg
}