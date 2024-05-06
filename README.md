gstream
================

Serving Gulf Stream datasets directly from R.

## Requirements

[R v4.1+](https://www.r-project.org/)

[rlang](https://CRAN.R-project.org/package=rlang)

[dplyr](https://CRAN.R-project.org/package=dplyr)

[sf](https://CRAN.R-project.org/package=sf)

## Installation

Use the [remotes](https://CRAN.R-project.org/package=remotes) package to
install directly from github.

    remotes::install("BigelowLab/gstream)

## Data from US Navy

### Arhcived data

[NOAA’s Ocean Prediction Center](https://ocean.weather.gov/) provides a
FTP server\](<https://ftp.opc.ncep.noaa.gov/grids/experimental/GStream>)
for downloads by year. We have downloaded these and repackaged into
spatial format files - these are included with the `gstream` package.

``` r
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(gstream)
  library(rnaturalearth)
})
```

    ## Warning: package 'sf' was built under R version 4.3.2

``` r
x = read_usn() |>
  dplyr::glimpse()
```

    ## Rows: 18,310
    ## Columns: 3
    ## $ date     <date> 2010-01-22, 2010-01-22, 2010-01-25, 2010-01-25, 2010-01-27, …
    ## $ wall     <chr> "north", "south", "north", "south", "north", "south", "north"…
    ## $ geometry <MULTIPOINT [°]> MULTIPOINT ((-80.2 25), (-8..., MULTIPOINT ((-77.5…

This reads in all of the data stored with the package. We can then do a
simple plot of all of the locations.

``` r
bb = sf::st_bbox(x)
coast = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")

plot(x['wall'], pch = 3, axes = TRUE, reset = FALSE)
plot(sf::st_geometry(coast), add = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### Downloading daily updates and configuration

**Note** that you don’t need to create the configuration file if you are
not downloading data.

The daily data is hosted by by [NOAA’s Ocean Prediction
Center](https://ocean.weather.gov/) In particular they post the US
Navy’s [daily Gulf Stream point
data](https://ocean.weather.gov/gulf_stream_latest.txt) for the north
and south walls. These can be downloaded. We provide a mechanism for
storing the URL of the daily data, the path to where you want to store
the downloads and a simple script for downloading. The configuration can
be stored anywhere, but by default we look for it isn `~/.gstream`.

``` r
cfg = read_configuration()
cfg
```

    ## $usn
    ## $usn$rawpath
    ## [1] "/Users/ben/Dropbox/data/gstream/usn/raw"
    ## 
    ## $usn$dailyuri
    ## [1] "https://ocean.weather.gov/gulf_stream_latest.txt"
    ## 
    ## $usn$ftpuri
    ## [1] "https://ftp.opc.ncep.noaa.gov/grids/experimental/GStream"

Obviously, you will want to modify the `rawpath` to suit your own needs.
We then set up a cron job to make the daily download at local 6pm.

    # gstream data
    0 18 * * * /usr/local/bin/Rscript /Users/ben/Library/CloudStorage/Dropbox/code/projects/gsi/inst/scripts/usn_daily_download.R >> /dev/null 2>&1

### Ordering USN data

The USN data is not ordered, that is the points for a given day are not
following a polyline. Is it possible to order them? If not, is it
possible to approximate an order?

``` r
d = dplyr::filter(x, date == as.Date("2020-12-19"), wall == "north")
plot(sf::st_geometry(d), type = "l", axes = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Maybe translate
[this](https://stackoverflow.com/questions/37742358/sorting-points-to-form-a-continuous-line)
into R? Or
[this](https://stackoverflow.com/questions/60495463/r-find-nearest-neighbor-for-selected-point)?

#### Find the most western point

This isn’t necessarily the starting point, but we can return to that
later.

It looks like it scanes south to north.

``` r
points = sf::st_geometry(d) |>
  sf::st_cast("POINT") |>
  sf::st_as_sf() |>
  sf::st_set_geometry("geometry") |>
  #dplyr::slice(100:105) |>
  dplyr::mutate(id = seq_len(dplyr::n()), .before = 1)

xy = sf::st_coordinates(points) |>
  dplyr::as_tibble()
nx = nrow(xy)

ilat = order(xy[["Y"]])
xy = slice(xy, ilat)
points = dplyr::slice(points, ilat)
plot(points, type = "b")
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
xy = as.matrix(xy)
nx = nrow(xy)
nindex = rep(NA_real_, nx)
index = as.character(nindex)
index[1] = as.character(which.min(xy[,1]))
m = st_distance(points, points)
dimnames(m) = list(seq_len(nx), seq_len(nx))

for (i in seq(1, nx)){
  ix = order(m[index[i],]) # mixed 
  nindex[i+1] = ix[2] + if ((ix[2]) %in% nindex) 1 else 0
  index[i+1] = as.character(nindex[i+1])
}
```
