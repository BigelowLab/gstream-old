

# see yamls in the data directory
# like this...
#  input:
#    filename: /Users/ben/Downloads/world.tif
#  crop: 
#    xmin: -90
#    ymin: 18
#    xmax: -34
#    ymax: 50
#  output:
#    path: /Users/ben/Library/CloudStorage/Dropbox/data/gstream/agg/oisst
#  agg:
#    period: year
#    stat: sum

suppressPackageStartupMessages({
  library(gstream)
  library(yaml)
  library(sf)
  library(stars)
  library(dplyr)
})


main = function(cfg){
  
  x = gstream::read_usn() 
  S = stars::read_stars(cfg$input$filename)  |>
    rlang::set_names("count") |>
    dplyr::mutate(count = 0)
  bbox = cfg$input$crop |> 
    unlist() |>
    sf::st_bbox(crs = sf::st_crs(S))
  
  y = sf::st_crop(S, bbox) |> sf::st_normalize()
  
  
  if (!dir.exists(cfg$output$path)) ok = dir.create(cfg$output$path, 
                                                    recursive = TRUE)
  
  fmt = switch(cfg$agg$period,
    "month" = "%Y-%m-01",
    "year" = "%Y-01-01")
  
  
  pattern = switch(cfg$agg$period,
                   "month" = "%s_mon_count.tif",
                   "year" = "%s_year_count.tif")
  nwall = x |>
    dplyr::filter(wall == "north") |>
    dplyr::mutate(adate = format(date, fmt), .before = 1) |>
    dplyr::group_by(adate) |>
    dplyr::group_walk(
      function(tbl, key){
        p = sf::st_cast(tbl, "POINT") 
        ix = st_cells(y, p)
        m = (y[[1]] * 0) |> as.vector()
        for (i in ix) m[i] = m[i] + 1
        m[m <= 0] <- NA
        dim(m) <- dim(y)
        y[[1]] <- m
        year = format(tbl$date[1], "%Y")
        path = file.path(cfg$output$path, year)
        ok = dir.create(path, recursive = TRUE, showWarnings = FALSE)
        filename = file.path(path, sprintf(pattern, key$adate))
        stars::write_stars(y, filename)
      }
    ) 
  
  return(0)
  
}


args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) args = "/Users/ben/Library/CloudStorage/Dropbox/code/projects/gsi/inst/scripts/agg-oisst-year.yaml"
cfg = yaml::read_yaml(args)

if (!interactive()){
  ok = main(cfg)
  quit(save = "no", status = ok)
}
