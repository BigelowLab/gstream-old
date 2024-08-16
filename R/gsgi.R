#' Import (from matlab) the GSGI index data
#' 
#' @export
#' @param file char, the name of the file to import
#' @return a tibble of data
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
  readr::read_csv(file, col_types = "Dnnn")
}


#' Plot gsgi data time series
#' 
#' @export
#' @param x the gsgi data
#' @param smooth logical, if TRUE add a smoothing line
#' @return a ggplot2 object
plot_gsgi = function(x, smooth = TRUE){
  
  gg = ggplot2::ggplot(data = x, ggplot2::aes(x = date, y = dSST.deseason)) + 
    ggplot2::geom_line() +
    ggplot2::labs(x = "Date", y = "dSST (deseasoned, C)", 
                  title = "Gulf Stream SST Gradient Index",
                  caption = "data source: https://www2.whoi.edu/staff/ykwon/data/")
  if (smooth){
    gg = gg + 
      ggplot2::geom_smooth(method = 'loess', formula = 'y ~ x')
  }
  gg
}