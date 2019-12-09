#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' who3_bp
#'
#' get bounding polygons for WHO3 cities (Accra, Kathmandu)
#' @param city Name of city for which to obtain bounding polygon
#' @return Matrix of coordinates of bounding polygon
#' @export
who3_bp <- function (city) {
    if (grepl ("accra", city, ignore.case = TRUE)) {
        #city <- "Greater Accra Region"
        bnames <- c ("Accra Metropolitan", "Ga West", "Ga East",
                     "Ashaiman", "Adenta", "Ledzokuku-Krowor") 
        res <- lapply (bnames, function (i)
                       osmdata::getbb(place_name = i, format_out = "polygon"))
        # names which return multiple polygons:
        bnames2 <- c ("Tema", "Ga South")
        pnum <- c (1, 1)
        res2 <- lapply (seq (bnames2), function (i)
                        osmdata::getbb(place_name = bnames2 [i],
                                       format_out = "polygon") [[pnum [i] ]])

        res <- lapply (c (res, res2), function (i) sf::st_polygon (list (i)))
        res <- do.call (c, res) # auto-cast to multipolygon
        # should not buffer WSG84 data, but near enough here
        res <- sf::st_coordinates (sf::st_buffer (res, 0)) [, 1:2]
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
#' Get bus stop and bus network data for WHO3 cities (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_bus_data <- function (city, save = FALSE) {
    # Accra includes the bus stops within the network, Kathmandu does not
    if (grepl ("accra", city, ignore.case = TRUE)) {
        net <- osmdata::opq ("accra ghana") %>%
            osmdata::add_osm_feature (key = "bus") %>%
            osmdata::add_osm_feature (key = "type", value = "route") %>%
            osmdata::osmdata_sc (quiet = TRUE)

        bus_stops <- net$relation_members %>%
            dplyr::filter (type == "node" & role == "platform")
        bus_stops <- unique (bus_stops$member)
        xy <- net$vertex [net$vertex$vertex_ %in% bus_stops, ]
        xy <- sf::st_multipoint (as.matrix (xy [, 1:2])) %>%
            sf::st_sfc (crs = 4326) %>%
            st_cast ("POINT")
        bus_stops <- sf::st_sf (id = bus_stops, geometry = xy)

        # remove stops beyond the bounding polygon:
        p <- who3_bp ("Accra")
        p <- sf::st_polygon (list (p)) %>%
            sf::st_sfc (crs = 4326) %>%
            sf::st_sf ()
        index_in <- suppressMessages (
                which (as.integer (sf::st_within (xy, p)) == 1)
                )
        bus_stops <- bus_stops [index_in, ]
    } else if (grepl ("kathmandu", city, ignore.case = TRUE)) {
        b <- osmdata::opq ("kathmandu") %>%
            osmdata::add_osm_feature (key = "public_transport") %>%
            osmdata::osmdata_sf ()
        stop_types <- names (table (b$osm_points$public_transport))
        bus_stops <- b$osm_points [b$osm_points$public_transport %in%
                                   stop_types, ]
        # remove all stops beyond the bounding polygon:
        p <- who3_bp ("Kathmandu")
        p <- sf::st_polygon (list (p)) %>%
            sf::st_sfc (crs = 4326) %>%
            sf::st_sf ()
        index_in <- suppressMessages (
                which (as.integer (sf::st_within (bus_stops, p)) == 1)
                )
        col_index <- which (names (bus_stops) %in%
                            c ("osm_id", "name", "public_transport",
                               "geometry"))
        bus_stops <- bus_stops [index_in, col_index]
    }
    return (bus_stops)
}

