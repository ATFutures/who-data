#' whodata
#'
#' @name whodata
#' @docType package
#' @author Mark Padgham, Robin Lovelace
#' @importFrom dplyr mutate select
#' @importFrom here here
#' @importFrom jsonlite fromJSON
#' @importFrom magrittr %>% %<>% extract2
#' @importFrom methods slot
#' @importFrom osmdata add_osm_feature getbb opq osmdata_sf trim_osmdata
#' @importFrom piggyback pb_track pb_upload
#' @importFrom raster crop raster writeRaster
#' @importFrom rgdal readOGR
#' @importFrom sf sf_project st_centroid st_geometry st_transform
#' @importFrom sp point.in.polygon
#' @importFrom utils packageVersion setTxtProgressBar
#' @importFrom utils txtProgressBar unzip
NULL
