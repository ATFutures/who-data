#' who_cities
#' List cities currently being analysed
#' @export
who_cities <- function ()
{
    c ("accra", "bristol", "kathmandu")
}

#' upload_popdens_nodes
#' @export
upload_popdens_nodes <- function ()
{
    flist <- file.path (c ("accra", "kathmandu"), "osm", "popdens_nodes.Rds")
    junk <- lapply (flist, function (i)
                    {
                        message ("uploading ", i)
                        piggyback::pb_upload (i, repo = "ATFutures/who-data",
                                      tag = "v0.0.3-osmdata")
                    })
}

#' download_popdens_nodes
#' @export
download_popdens_nodes <- function ()
{
    flist <- file.path (c ("accra", "kathmandu"), "osm", "popdens_nodes.Rds")
    junk <- lapply (flist, function (i)
                    {
                        message ("downloading ", i)
                        piggyback::pb_download (i, repo = "ATFutures/who-data",
                                                tag = "v0.0.3-osmdata")
                    })
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
    download_popdens_nodes ()
    download_osm ()
    download_bristol_pop ()
}
