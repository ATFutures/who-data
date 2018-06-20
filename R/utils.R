#' who_cities
#' List cities currently being analysed
#' @export
who_cities <- function ()
{
    c ("accra", "bristol", "kathmandu")
}

#' download_who_data
#'
#' Download all WHO data from github repo via \pkg{piggyback}. This function can
#' be used both for initial download, and for periodic refreshment of data. Only
#' those data which have been updated will be downloaded anew.
#' @export
download_who_data <- function ()
{
    download_worldpop_tiffiles ()
    download_osm ()
}
