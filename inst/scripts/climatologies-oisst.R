suppressPackageStartupMessages({
  library(gstream)
  library(yaml)
  library(sf)
  library(stars)
  library(dplyr)
})


main = function(cfg){
  
  
}


args = commandArgs(trailingOnly = TRUE)
if (length(args) == 0) args = "/Users/ben/Library/CloudStorage/Dropbox/code/projects/gsi/inst/scripts/climatology-oisst.yaml"
cfg = yaml::read_yaml(args)

if (!interactive()){
  ok = main(cfg)
  quit(save = "no", status = ok)
}
