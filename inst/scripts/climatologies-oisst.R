suppressPackageStartupMessages({
  library(gstream)
  library(yaml)
  library(sf)
  library(stars)
  library(dplyr)
})


main = function(cfg){
  
  fullfiles = list.files(cfg$input$path, pattern = glob2rx("*.tif"), 
                         recursive = TRUE, full.names = TRUE)
  
  if ("year" %in% cfg$agg$period){
    period = "year"
    files = fullfiles[grepl(period, fullfiles, fixed = TRUE)]
    files = files[-length(files)]  # all but the last
    dates = substring(basename(files), 1, nchar("2010-01-01")) |>
      as.Date()
    
    x = stars::read_stars(files, along = list(time = dates)) |>
      stars::st_apply(c("x", "y"), mean, na.rm = TRUE)
    
    ofile = file.path(cfg$output$path,
                      "climatology",
                      sprintf("%s_%s_ac_mean.tif", 
                              format(dates[1], "%Y"),
                              format(dates[length(dates)], "%Y")))

    ok = dir.create(dirname(ofile), recursive = TRUE, showWarnings = FALSE)
    stars::write_stars(x, ofile)
    
    
    x = stars::read_stars(files, along = list(time = dates)) |>
      stars::st_apply(c("x", "y"), sum, na.rm = TRUE)
    
    ofile = file.path(cfg$output$path,
                      "climatology",
                      sprintf("%s_%s_ac_count.tif", 
                              format(dates[1], "%Y"),
                              format(dates[length(dates)], "%Y")))
    
    stars::write_stars(x, ofile)
    
    
  }
  
  if ("mon" %in% cfg$agg$period){
    period = "mon"
    files = fullfiles[grepl(period, fullfiles, fixed = TRUE)]
    dates = substring(basename(files), 1, nchar("2010-01-01")) |>
      as.Date()
    
    # remove current year
    years = format(dates, "%Y")
    thisyear = format(Sys.Date(), "%Y")
    ix = !(years %in% thisyear)
    files = files[ix]
    dates = dates[ix]
    years = years[ix]
    
    yearstring = paste(years[1], years[length(years)], sep = "-")
    months = format(dates, "%b")
    
    ff = split(files, months)
    dd = split(dates, months)
    yy = split(years, months)
    for (thismonth in names(ff)){
      
      x = stars::read_stars(ff[[thismonth]],
                            along = list(time = dd[[thismonth]]))  |>
        stars::st_apply(c("x", "y"), mean, na.rm = TRUE)
     
      ofile = file.path(cfg$output$path,
                        "climatology",
                        sprintf("%s_%s_%s_mc_mean.tif", 
                                yy[[thismonth]][1],
                                yy[[thismonth]][length(yy[[thismonth]])],
                                thismonth))
      
      ok = dir.create(dirname(ofile), recursive = TRUE, showWarnings = FALSE)
      stars::write_stars(x, ofile)
      
      
      x = stars::read_stars(ff[[thismonth]],
                            along = list(time = dd[[thismonth]]))  |>
        stars::st_apply(c("x", "y"), sum, na.rm = TRUE)
      
      ofile = file.path(cfg$output$path,
                        "climatology",
                        sprintf("%s_%s_%s_mc_sum.tif", 
                                yy[[thismonth]][1],
                                yy[[thismonth]][length(yy[[thismonth]])],
                                thismonth))
      
      stars::write_stars(x, ofile)
      
    }
  
  
}


args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) args = "/Users/ben/Library/CloudStorage/Dropbox/code/projects/gsi/inst/scripts/climatology-oisst.yaml"
cfg = yaml::read_yaml(args)

if (!interactive()){
  ok = main(cfg)
  quit(save = "no", status = ok)
}
