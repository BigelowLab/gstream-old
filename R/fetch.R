known_files = function(){
  
}

#' Fetch a single file
#' 
#' @export
#' @param filename the name of the file
#' @param base_uri char, the base uri
#' @return sf MULTIPOINT data frame
fetch_year = function(filename = "NavoGS_2010.tar",
                      base_uri = 'https://ftp.opc.ncep.noaa.gov/grids/experimental/GStream'){
  
  src = file.path(base_uri, filename)
  tmpdir = file.path(tempdir(check = TRUE), "gsi")
  if (!dir.exists(tmpdir)) ok = dir.create(tmpdir, recursive = TRUE)
  dest = tempfile(fileext = ".tar", tmpdir = tmpdir)
  ok = download.file(src, dest, mode = 'wb')
  ok = untar(dest, exdir = tmpdir)
  files = list.files(tmpdir, 
                     pattern = "^.*\\.sub$",
                     recursive = TRUE, full.names = TRUE)
  x = lapply(files, read_wall_data_usn) |>
    dplyr::bind_rows()
  ok = unlink(tmpdir, force = TRUE, recursive = TRUE)
  x
}