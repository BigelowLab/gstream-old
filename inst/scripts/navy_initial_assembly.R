library(gstream)

inpath = "~/Downloads/gswall"
outpath = "/Users/ben/Library/CloudStorage/Dropbox/code/projects/gsi/inst/usn"

tars = list.files(inpath, pattern = glob2rx("*.tar"), full.names = TRUE)
verbose = TRUE
for (tarfile in tars){
  cat("reading", basename(tarfile), "\n")
  x = read_wall_data_usn(tarfile, verbose = verbose)
  outfile = file.path(outpath, format(x$date[1], "%Y.geojson"))
  sf::write_sf(x, outfile)
}
