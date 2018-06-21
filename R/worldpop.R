# functions to process worldpop data

#' worldpop_files
#'
#' Get lists of worldpop files
#' @param type either "zip" (pre-processed) or "tif" (processed)
#' @return Character vector of file names to be used to extract files from
#' github releases via \pkg{piggyback}
#' @export
worldpop_files <- function (type = "zip")
{
    type <- match.arg (type, c ("zip", "tif"))

    fnames_gha <- c ("GHA14_A0005_adjv1", "GHA14_A0510_adjv1",
                     "GHA14_A1015_adjv1", "GHA14_A1520_adjv1",
                     "GHA14_A2025_adjv1", "GHA14_A2530_adjv1",
                     "GHA14_A3035_adjv1", "GHA14_A3540_adjv1",
                     "GHA14_A4045_adjv1", "GHA14_A4550_adjv1",
                     "GHA14_A5055_adjv1", "GHA14_A5560_adjv1",
                     "GHA14_A6065_adjv1", "GHA14_A65PL_adjv1",
                     "GHA15adj_040213")
    fnames_npl <- c ("NPL_ppp_2015_adj_v2", "NPL_ppp_2020_adj_v2")

    if (type == "zip")
    {
        fnames_gha <- file.path ("accra", "popdens",
                                 paste0 (fnames_gha, ".zip"))
        fnames_npl <- file.path ("kathmandu", "popdens",
                                 paste0 (fnames_npl, ".zip"))
    } else
    {
        fnames_gha <- file.path ("accra", "popdens",
                                 paste0 (fnames_gha, ".tif"))
        fnames_npl <- file.path ("kathmandu", "popdens",
                                 paste0 (fnames_npl, ".tif"))
    }
    c (fnames_gha, fnames_npl)
}

#' upload_worldpop_zip
#'
#' upload the worldpop zip files to repo via piggyback
#' @export
upload_worldpop_zipfiles <- function ()
{
    flist <- worldpop_files (type = "zip")
    piggyback::pb_track (c ("accra/popdens/*.zip", "kathmandu/popdens/*.zip"))
    junk <- lapply (flist, function (i)
                    {
                        message ("uploading ", i)
                        piggyback::pb_upload (i, repo = "ATFutures/who-data",
                                      tag = "v0.0.1-worldpop-zip-gha-npl")
                    })
}

#' download_worldpop_zip
#'
#' download the worldpop zip files from repo via piggyback
#' @export
download_worldpop_zipfiles <- function ()
{
    flist <- worldpop_files (type = "zip")
    junk <- lapply (flist, function (i)
                    {
                        message ("downloading ", i)
                        piggyback::pb_download (i, repo = "ATFutures/who-data",
                                        tag = "v0.0.1-worldpop-zip-gha-npl")
                    })
}

#' remove_worldpop_zip
#'
#' Remove all worldpop zip files from local storage
#' @export
remove_worldpop_zip <- function ()
{
    junk <- file.remove (worldpop_files (type = "zip")) #nolint
}

#' upload_worldpop_tif
#'
#' upload the worldpop tif files to repo via piggyback
#' @export
upload_worldpop_tiffiles <- function ()
{
    flist <- worldpop_files (type = "tif")
    piggyback::pb_track (c ("accra/popdens/*.tif", "kathmandu/popdens/*.tif",
                            "bristol/popdens/*.tif"))
    junk <- lapply (flist, function (i)
                    {
                        message ("uploading ", i)
                        piggyback::pb_upload (i, repo = "ATFutures/who-data",
                                      tag = "v0.0.2-worldpop-tif-gha-npl")
                    })
}

#' download_worldpop_tif
#'
#' download the worldpop tif files from repo via piggyback
#' @export
download_worldpop_tiffiles <- function ()
{
    flist <- worldpop_files (type = "tif")
    junk <- lapply (flist, function (i)
                    {
                        message ("downloading ", i)
                        piggyback::pb_download (i, repo = "ATFutures/who-data",
                                        tag = "v0.0.2-worldpop-tif-gha-npl")
                    })
}

#' upload_bristol_pop
#' Upload the \code{tif} file disected with \link{crop_global_tif}
#' @export
upload_bristol_pop <- function ()
{
    f <- list.files (file.path ("bristol", "popdens"),
                     full.names = TRUE)
    piggyback::pb_upload (f, repo = "ATFutures/who-data",
                          tag = "v0.0.2-worldpop-tif-gha-npl")
}

#' download_bristol_pop
#' Download the \code{tif} file disected with \link{crop_global_tif}
#' @export
download_bristol_pop <- function ()
{
    # hard-coded because pb_list does not current work
    # TODO: use pb_list once it's working again
    f <- "bristol/popdens/GHS_POP_GPW42015_GLOBE_R2015A_54009_250_v1_0.tif"
    message ("downloading ", f)
    piggyback::pb_download (f, repo = "ATFutures/who-data",
                            tag = "v0.0.2-worldpop-tif-gha-npl")
}
