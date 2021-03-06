<!-- README.md is generated from README.Rmd. Please edit that file -->

# who-data

[![Build
Status](https://travis-ci.org/ATFutures/who-data.svg)](https://travis-ci.org/ATFutures/who-data)
[![Project Status: Concept - Minimal or no implementation has been done
yet.](http://www.repostatus.org/badges/0.1.0/concept.svg)](http://www.repostatus.org/#concept)

Data for [`who`](https://github.com/ATFutures/who) and
[`who3`](https://github.com/ATFutures/who3) repos. This README describes
the process to download and process `who3` data. To do that, clone this
repo, then run the following in the main repo directory:

``` r
devtools::load_all (".", export_all = FALSE)
```

This project currently processes data for the two cities of Accra,
Ghana, and Kathmandu, Nepal. Most functions accept a single `city`
argument as one of the two of these (without country specifications).
Having downloaded and locally installed the package, the following lines
are necessary to pre-process and locally save required data.

``` r
city <- "Kathmandu" # or Accra
hw <- who3_network (city) # way network
bus <- who3_bus_network (city)
bus <- who3_bus_centrality (city)
cent <- who3_centrality (city) # time-based centrality for motorcars
b <- who3_buildings (city)
netf <- who3_flow (city) # calculate pedestrian flows from buses and activity centres
```

The main results are then returned from an additional function which
appends columns quantifying densities of motorcars, and equivalent
dispersed values representing dispersal of pollutant away from vehicular
sources of origin. The single additional parameter of this function
specifies the width of a Gaussian diespersal kernel in metres.

``` r
netf <- who3_disperse_centrality (city, disperse_width = 200) 
```

That returns an `sf`-formatted `data.frame` with columns for pedestrian
flows (`"flow"`), vehicular flows (`"centrality"`), and dispersed
vehicular flows (`"centrality_disp"`). The direct exposure of
pedestrians to vehicular emissions can then be obtained by multiplying
`flow * centrality_disp`.
