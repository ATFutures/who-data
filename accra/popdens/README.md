The [worldpop data](http://www.worldpop.org.uk) are at national level, and have
to be manually downloaded. In all cases, data should be obtained by navigating
to the desired country, and clicking "Browse Individual Files". The data for
Ghana are:

1. Population -> Individual Files -> `GHA15adj_040213` & `GHA15_040213`
2. Age Structures -> Individual Files -> (all of them)

These are national level `.tif` files which then have to be reduced to the Accra
region only using `code/crop-tif.R`
