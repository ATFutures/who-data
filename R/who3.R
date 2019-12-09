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

    if (grepl ("accra", city, ignore.case = TRUE)) {
        # The massive roundabout in Accra between Dr Busia Hwy and Ring Roads
        # West and Central is under construction, and in centrality terms, the
        # massive diversion is the most central bit of the whole city. Need to
        # re-insert the actual roundabout into the network by setting all `key =
        # "highway", value = "construction"`, which actually all have `key =
        # "construction", value = "motorway"`, to `"highway" = "motorway".`
        key <- value <- NULL # suppress no visible binding notes
        ids <- hw$object %>%
            dplyr::filter (key == "construction", value == "motorway")
        index <- which (hw$object$object_ %in% ids$object_ &
                        hw$object$key == "highway")
        hw$object$value [index] <- "motorway"
    }

    if (save) {
        fname <- file.path (tolower (city), "osm",
                            paste0 (tolower (city), "-hw-sc.Rds"))
        saveRDS (hw, file = fname)
    }
    return (hw)
}

#' who3_centrality
#'
#' @inheritParams who3_network
#' @return A \pkg{dodgr} representation of the street network with a centrality
#' column calculated from time-based weighting for motorcar transport.
#' @export
who3_centrality <- function (city, save = FALSE, quiet = FALSE) {
    city <- tolower (city)
    f <- file.path (city, "osm", paste0 (city, "-hw-sc.Rds"))
    if (!file.exists (f))
        stop ("File [", f, "] does not exist;\nplease first run ",
              "'who3-network' to download street network data")

    hw <- readRDS (f)
    if (!quiet)
        message ("Preparing street network ... ", appendLF = FALSE)
    net <- dodgr::weight_streetnet (hw, wt_profile = "motorcar",
                                    turn_penalty = TRUE)
    netc <- dodgr::dodgr_contract_graph (net)
    netc$d <- netc$time
    netc$d_weighted <- netc$time_weighted
    if (!quiet)
        message ("\rPrepared street network ...  \n",
                 "Calculating centrality ... ", appendLF = FALSE)

    st <- system.time (
        netc <- dodgr::dodgr_centrality (netc)
    )
    if (!quiet)
        message ("\rCalculated centrality in ", 
                 as.numeric (st [3]), " seconds")
    net <- dodgr::dodgr_uncontract_graph (netc)

    if (save) {
        f <- file.path (city, "flows", "centrality-edge.Rds")
        saveRDS (net, f)
        if (!quiet)
            message ("Network save to [", f, "]")
    }
    return (net)
}

#' who3_building
#'
#' @inheritParams who3_network
#' @export
who3_building <- function (city, save = FALSE, quiet = FALSE) {
    bp <- who3_bp (city)
    bldg <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "building", value = "!residential") %>%
        osmdata::osmdata_sf (quiet = quiet) %>%
        osmdata::trim_osmdata (bp)
    if (save) {
        fname <- file.path (tolower (city), "osm",
                            paste0 (tolower (city), "-bldg.Rds"))
        saveRDS (bldg, file = fname)
    }
    return (bldg)
}

#' who3_bus_data
#'
#' Get bus network data for WHO3 cities (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_bus_data <- function (city, save = FALSE) {
    if (grepl ("accra", city, ignore.case = TRUE)) {
        res <- osmdata::opq ("accra ghana") %>%
            osmdata::add_osm_feature (key = "bus") %>%
            osmdata::add_osm_feature (key = "type", value = "route") %>%
            osmdata::osmdata_sc (quiet = TRUE)
    } else if (grepl ("kathmandu", city, ignore.case = TRUE)) {
        res <- osmdata::opq ("kathmandu") %>%
            osmdata::add_osm_feature (key = "route", value = "bus") %>%
            osmdata::osmdata_sf (quiet = FALSE) %>%
            osmdata::osm_poly2line ()
        l <- res$osm_multilines
        # then extract all constituent lines
        ids <- do.call (c, lapply (l$geometry, function (i) names (i))) %>%
            unname () %>%
            unique ()
        net <- res$osm_lines [res$osm_lines$osm_id %in% ids, ]
        # Then bus stops
        b <- osmdata::opq ("kathmandu") %>%
            osmdata::add_osm_feature (key = "public_transport") %>%
            osmdata::osmdata_sf ()
        stop_types <- names (table (b$osm_points$public_transport))
        stops <- b$osm_points [b$osm_points$public_transport %in%
                               stop_types, ]
    }
}

