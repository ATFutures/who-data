#' who_cities
#' List cities currently being analysed
#' @export
who_cities <- function ()
{
    c ("accra", "bristol", "kathmandu")
}

match_city <- function (city = NULL)
{
    cities <- c ("accra", "kathmandu")
    if (!is.null (city))
        city <- match.arg (tolower (city), cities)
    else
        city <- cities
    return (city)
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
#' @param city Default uploads both cities, otherwise one of Accra or Kathmandu.
#' @export
download_popdens_nodes <- function (city = NULL)
{
    flist <- file.path (match_city (city), "osm", "popdens_nodes.Rds")
    junk <- lapply (flist, function (i)
                    {
                        message ("downloading ", i)
                        piggyback::pb_download (i, repo = "ATFutures/who-data",
                                                tag = "v0.0.3-osmdata")
                    })
}

#' upload_flows
#' @param city Default uploads both cities, otherwise one of Accra or Kathmandu.
#' @param overwrite Should generally be set to \code{TRUE}.
#' @export
upload_flows <- function (city = NULL, overwrite = TRUE)
{
    flist <- list.files (file.path (match_city (city), "flows"),
                         full.names = TRUE)
    junk <- lapply (flist, function (i)
                    {
                        message ("uploading ", i)
                        piggyback::pb_upload (i, repo = "ATFutures/who-data",
                                      tag = "v0.0.4-flowlayers",
                                      overwrite = overwrite)
                    })
}

#' download_flows
#' @export
download_flows <- function ()
{
    flist <- file.path ("flows", c ("flow_foot_act_bus_k15.Rds",
                                    "flow_foot_bus_act_k15.Rds",
                                    "flow_foot_bus_res_k15.Rds",
                                    "flow_foot_res_bus_k15.Rds"))
    #flist <- unlist (lapply (flist, function (i)
    #                         c (paste0 ("accra/", i),
    #                            paste0 ("kathmandu/", i))))
    # TODO: Reinstate the above once Kathmandu flows have been redone
    # MP: Nov 2018
    flist <- unlist (lapply (flist, function (i) (paste0 ("accra/", i))))
    junk <- lapply (flist, function (i)
                    {
                        message ("downloading ", i)
                        piggyback::pb_download (i, repo = "ATFutures/who-data",
                                      tag = "v0.0.4-flowlayers")
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
    download_flows ()
}
