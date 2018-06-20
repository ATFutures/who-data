<!-- README.md is generated from README.Rmd. Please edit that file -->

# who-data

[![Build
Status](https://travis-ci.org/ATFutures/who.svg)](https://travis-ci.org/ATFutures/who)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

Data for [`who` repo](https://github.com/ATFutures/who).

# worldpop download and pre-processing (does not need to be run)

Data must be manually downloaded from
[worldpop](http://www.worldpop.org.uk/). See `?worldpop_files (type =
"zip")` for list of which files need to be obtained. These should be in
the relevant `<city>/popdens` directories (where `<city>` is “accra” or
“kathmandu”). Then upload to the release `v0.0.1-worldpop-zip-gha-npl`
with

``` r
library (whodata)
upload_worldpop_zipfiles ()
```

These should never be needed again, but can easily be downloaded from
the repo with

``` r
download_worldpop_zipfiles ()
```

These files can then be converted to local `.tif` files with

``` r
crop_worldpop_tif (city = "accra")
crop_worldpop_tif (city = "kathmandu")
```

And then uploaded via piggyback to corresponding release
`v0.0.2-worldpop-tif-gha-npl` with

``` r
upload_worldpop_tiffiles ()
```
