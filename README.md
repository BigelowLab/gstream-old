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

### Data from US Navy

[FTP server](https://ftp.opc.ncep.noaa.gov/grids/experimental/GStream).
A manually prepared dataset that identifies the north wall and south
wall as unordered points.

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

``` r
bb = sf::st_bbox(x)
coast = rnaturalearth::ne_coastline(scale = "medium", returnclass = "sf")

plot(x['wall'], pch = 3, axes = TRUE, reset = FALSE)
plot(sf::st_geometry(coast), add = TRUE)
```

![](README_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->
