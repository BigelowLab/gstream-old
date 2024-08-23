# The purpose of this script is to assemble a table of patch data for one or 
# more patches (defined by bounding boxes) drawn from one or more data sources 
# (ERSST, OISST).  The output is a long form table that includes
# date, patch, source followed by summary stats (min, q1, median, mean, d3, max).
#

suppressPackageStartupMessages({
  library(charlier)
  library(twinkle)
  library(murtools)
  library(ersst)
  library(oisster)
  library(gstream)
  library(readr)
  library(stars)
  library(sf)
  library(dplyr)
})

#' Adds the desired patch BBs to the inst/patches directory
#' 
#' Just run this once and then forget it
#' 
#' @param patches chr, the patch names
#' @returns the bounding box patches
install_patches = function(patches = c("cold_blob", "warm_spot", "nh")){
  cofbb::get_bb(patches, form = "sf") |>
    sf::write_sf("inst/patches/patches_bb.gpkg")
}


#' Given a set of values compute the standard summary of 
#' [min, q25, median, mean, q75, max]
#' 
#' @param x numeric vector of values
#' @param na.rm logical, if TRUE remove NA values before computing summaries
#' @return named vector of summary values
sixnum = function(x, na.rm = TRUE){
  m = mean(x, na.rm = na.rm)
  r = fivenum(x, na.rm = na.rm)
  c(r[1:3], m, r[4:5]) |>
    rlang::set_names(c("min", "q25", "median", "mean", "q75", "max"))
}

#' Get the databases for the differenrt sources
get_oisst_db = function(path = oisster::oisst_path("world")){
  oisster::read_database(path) |>
    dplyr::filter(per == "day")
  
}
get_ersst_db = function(path = ersst::ersst_path("v5")){
  ersst::read_database(path) |>
    dplyr::filter(!anomaly)
}

#' The compute cycle on a stack, with one or more patches
get_stats = function(sarray, bb, fun = sixnum, source = "unknown"){
  dates = stars::st_get_dimension_values(sarray, "time")
  
  bb |> 
    dplyr::rowwise() |>
    dplyr::group_map(
      function(tbl, key){
        r = aggregate(sarray, tbl, sixnum)
        vnames = stars::st_get_dimension_values(r, "sixnum")
        
        r = r[[1]] |> as.matrix()
        dim(r) <- c(6, length(dates)) 
        r = t(r) 
        colnames(r) <- vnames
        dplyr::as_tibble(r) |>
          dplyr::mutate(date = dates,
                        region = tbl$name, 
                        source = source, 
                        .before = 1) |>
          dplyr::arrange(date)
      }) |>
    dplyr::bind_rows()
}



paths = c(
  ersst = ersst::ersst_path("v5"),
  oisst =  oisster::oisst_path("world"))

dbs = list(
  ersst = get_ersst_db(path = paths[['ersst']]),
  oisst = get_oisst_db(path = paths[['oisst']]))

bbs = read_patch_bbs() |>
  dplyr::filter(name != "nh")


r = list()


# for ERSST we can just use st_extract at once - the data is lightweight and it is fast
# but we do need to convert lon form [-180, 180] to [0,360]
bb2 = sf::st_shift_longitude(bbs)
nm = "ersst"
ff = ersst::compose_filename(dbs[[nm]], path = paths[[nm]])
ok = file.exists(ff)
s = stars::read_stars(ff[ok],
                      along = list(time = dplyr::filter(dbs[[nm]], ok) |> dplyr::pull(date)))
r[[nm]] = get_stats(s, bb2, source = "ersst")


# for oisst we are probably better off opening each file once, extract each roi and
# repeating plus we need to compute monthlies

nm = "oisst"
r[[nm]] = dbs[[nm]] |>
  #dplyr::slice(1:90) |>
  dplyr::mutate(
    mon = as.Date(format(.data$date, "%Y-%m-01")),
    filename = oisster::compose_filename(.data, path = paths[[nm]])) |>
  dplyr::group_by(mon) |>
  dplyr::group_map(
    function(tbl, key){
      s = stars::read_stars(tbl$filename, along = list(time = tbl$date))
      lapply(seq_len(nrow(bbs)),
                 function(i) {
                   bbi = dplyr::slice(bbs,i)
                   r = suppressWarnings(sf::st_crop(s, sf::st_bbox(bbi), normalize = TRUE))[[1]] |>
                     as.vector() |>
                     sixnum() |>
                     as.list() |>
                     dplyr::as_tibble() |>
                     dplyr::mutate(date = key$mon,
                                   region = bbi$name,
                                   source = "oisst",
                                   .before = 1)
                 }) |>
        dplyr::bind_rows()
    }) |>
  dplyr::bind_rows()

r = dplyr::bind_rows(r) |>
  readr::write_csv("inst/patches/patches_data_month.csv.gz")


charlier::sendmail(to = "btupper@bigelow.org",
                    subject = "extraction complete",
                    message = "tahdah")
