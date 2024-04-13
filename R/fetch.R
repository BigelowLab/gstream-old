#' Fetch a single file
#' 
#' @export
#' @param year num or char, the 4 digit year
#' @param base_uri char, the base uri
#' @return sf MULTIPOINT data frame
fetch_year = function(year = 2010,
                      base_uri = 'https://ftp.opc.ncep.noaa.gov/grids/experimental/GStream'){
  
  src = file.path(base_uri, sprintf("NavoGS_%0.4i.tar", as.integer(year[1])))
  tmpdir = file.path(tempdir(check = TRUE), "gsi")
  if (!dir.exists(tmpdir)) ok = dir.create(tmpdir, recursive = TRUE)
  dest = tempfile(fileext = ".tar", tmpdir = tmpdir)
  ok = download.file(src, dest, mode = 'wb')
  ok = untar(dest, exdir = tmpdir)
  files = list.files(tmpdir, 
                     pattern = "^.*\\.sub$",
                     recursive = TRUE, full.names = TRUE)
  x = lapply(files, read_wall_data) |>
    dplyr::bind_rows()
  ok = unlink(tmpdir, force = TRUE, recursive = TRUE)
  x
}