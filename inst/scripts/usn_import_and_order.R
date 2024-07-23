#' List the orig (not raw but unordered) data files and decode the years
#' for each file
#'   read the gpkg file
#'   order and orient eastward
#'   save as ordered
#' It's fairly incestuous as we are saving to package data
suppressPackageStartupMessages({
  library(gstream)
})

path = system.file("usn", package = "gstream")

rawfiles = list.files(file.path(path, "orig"),
                      pattern = glob2rx("*.gpkg"),
                      full.names = TRUE)


names(years) <- years <- gsub(".gpkg", "", basename(rawfiles))

xx = lapply(years, read_usn, what = "orig")

xx = lapply(xx, order_usn)


pkgpath = "/mnt/s1/projects/ecocast/corecode/R/gstream"
ok = lapply(names(xx),
            function(year){
              sf::write_sf(xx[[year]],
                           file.path(file.path(pkgpath, "inst", "usn", "ordered",
                                               sprintf("%s.gpkg", year))))
            })


