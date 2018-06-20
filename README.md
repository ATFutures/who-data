<!-- README.md is generated from README.Rmd. Please edit that file -->

# who-data

[![Build
Status](https://travis-ci.org/ATFutures/who-data.svg)](https://travis-ci.org/ATFutures/who-data)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

Data for [`who` repo](https://github.com/ATFutures/who). To obtain local
copes of all data, simply clone this repo, then run the following in the
main repo directory:

``` r
devtools::load_all (".", export_all = FALSE)
download_who_data ()
```

That should be all that is required for to generate flow layers for each
city via the [`flowlayers` repo](https:github.com/ATFutures/flowlayers).

# Population density download and pre-processing

(None of this should need to be run; it is code used to generate the
initial files.)

## worldpop (Accra and Kathmandu)

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

## Bristol

There are no worldpop data for Europe, but the EC Joint Research Centre
Data Catalogue offers 250m resolution global population density `tif`
files. The main JRCDC page describing the [relevant
file](http://data.jrc.ec.europa.eu/dataset/jrc-ghsl-ghs_pop_gpw4_globe_r2015a)
leads leading to the download of
`GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0.zip`, which is 1GB, and is
not archived in this repo. This file can then be processed with

``` r
crop_global_tif (city = "Bristol")
```

# Street network download

``` r
library (sf) # pre-load required
cities <- who_cities () # accra, kathmandu, bristol
junk <- lapply (cities, function (i) get_who_streets (city = i))
junk <- lapply (cities, function (i) get_who_buildings (city = i))
junk <- lapply (cities, function (i) get_who_busstops (city = i))
```
