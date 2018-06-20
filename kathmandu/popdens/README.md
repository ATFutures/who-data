The [worldpop data](http://www.worldpop.org.uk) are at national level, and have
to be manually downloaded. In all cases, data should be obtained by navigating
to the desired country, and clicking "Browse Individual Files". Unlike Ghana,
Nepal does **not** have Age Structure files, so only four demographic files are
included: the 2015 and 2020 raw and adjusted population densities. (Adjustment
is for UN total national populations.)

These are national level `.tif` files which then have to be reduced to the
Kathmandu region only using `code/crop-tif.R`
