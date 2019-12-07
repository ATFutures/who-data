#' who3_bp
#'
#' get bounding polygons for WHO3 cities (Accra, Kathmandu)
#' @param city Name of city for which to obtain bounding polygon
#' @return Matrix of coordinates of bounding polygon
#' @export
who3_bp <- function (city) {
    if (grepl ("accra", city, ignore.case = TRUE)) {
        city <- "Accra Metropolitan"
        res <- osmdata::getbb(place_name = city, format_out = "polygon")
    } else if (grepl ("kath", city, ignore.case = TRUE)) {
        bp1 <- osmdata::getbb(place_name = "Kathmandu",
                              format_out = "sf_polygon")
        bp2 <- osmdata::getbb(place_name = "Patan Nepal",
                              format_out = "sf_polygon")
        p <- sf::st_sf (sf::st_union (c (bp1$geometry, bp2$geometry)))
        res <- sf::st_coordinates (p) [, 1:2]
    } else
        stop ("City must be either Accra or Kathmandu")

    return (res)
}

#' Get street network for WHO# cities (Accra and Kathmandu)
#'
#' @inheritParams who3_bp
#' @param save If `TRUE`, save data to internal `who-data` directory.
#' @param quiet If `FALSE`, display progress information on screen.
#' @return SC-formatted street network for nominated city.
#' @export
who3_network <- function (city, save = FALSE, quiet = FALSE) {
    bp <- who3_bp (city)

    hw <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "highway") %>%
        osmdata::osmdata_sc (quiet = quiet) %>%
        osmdata::trim_osmdata (bp)
    if (save) {
        fname <- file.path (tolower (city), "osm",
                            paste0 (tolower (city), "-hw-sc.Rds"))
        saveRDS (hw, file = fname)
    }
    return (hw)
}
