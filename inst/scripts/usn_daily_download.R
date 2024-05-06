library(yaml)
cfg = read_yaml("~/.gstream")
today = format(Sys.Date(), "%Y-%m-%d.sub") 
download.file(cfg$usn$dailyuri, file.path(cfg$usn$rawpath, today))
