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
who3_network <- function (city, save = TRUE, quiet = FALSE) {
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
who3_centrality <- function (city, save = TRUE, quiet = FALSE) {
    city <- tolower (city)
    f <- file.path (city, "flows", "centrality-edge.Rds")
    if (file.exists (f))
        net <- readRDS (f)
    else {
        net <- who3_centrality_internal (city, save = save, quiet = quiet)
        if (save) {
            saveRDS (net, f)
            if (!quiet)
                message ("Network save to [", f, "]")
        }
    }
    return (net)
}

who3_centrality_internal <- function (city, save = TRUE, quiet = FALSE) {
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

    return (net)
}

#' who3_building
#'
#' @inheritParams who3_network
#' @export
who3_building <- function (city, save = TRUE, quiet = FALSE) {
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

#' who3_bus_network
#'
#' Get bus network data for WHO3 cities (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_bus_network <- function (city) {
    city <- tolower (city)
    f <- file.path (city, "osm", paste0 (city, "-bus-net.Rds"))
    savefile <- TRUE

    if (file.exists (f)) {
        savefile <- FALSE
        net <- readRDS (f)
    } else {
        if (grepl ("accra", city, ignore.case = TRUE)) {
            net <- osmdata::opq ("accra ghana") %>%
                osmdata::add_osm_feature (key = "bus") %>%
                osmdata::add_osm_feature (key = "type", value = "route") %>%
                osmdata::osmdata_sc (quiet = TRUE)
        } else if (grepl ("kathmandu", city, ignore.case = TRUE)) {
            x <- osmdata::opq ("kathmandu") %>%
                osmdata::add_osm_feature (key = "route", value = "bus") %>%
                osmdata::osmdata_sf (quiet = TRUE) %>%
                osmdata::osm_poly2line ()
            l <- x$osm_multilines
            # then extract all constituent lines
            ids <- do.call (c, lapply (l$geometry, function (i) names (i))) %>%
                unname () %>%
                unique ()
            net <- x$osm_lines [x$osm_lines$osm_id %in% ids, ]
        }
    }

    if (savefile)
        saveRDS (net, f)

    return (net)
}

#' who3_bus_stops
#'
#' Get bus stop data for WHO3 cities (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_bus_stops <- function (city, save = TRUE) {
    # Accra includes the bus stops within the network, Kathmandu does not
    if (grepl ("accra", city, ignore.case = TRUE)) {
        f <- file.path ("accra", "osm", "accra-bus-net.Rds")
        if (!file.exists (f))
            net <- who3_bus_network (city = "accra")
        net <- readRDS (f)

        bus_stops <- net$relation_members %>%
            dplyr::filter (type == "node" & role == "platform")
        bus_stops <- unique (bus_stops$member)
        xy <- net$vertex [net$vertex$vertex_ %in% bus_stops, ]
        xy <- sf::st_multipoint (as.matrix (xy [, 1:2])) %>%
            sf::st_sfc (crs = 4326) %>%
            sf::st_cast ("POINT")
        bus_stops <- sf::st_sf (id = bus_stops, geometry = xy,
                                stringsAsFactors = FALSE)

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

#' who3_bus_centrality
#'
#' Calculate centrality of bus networks for WHO3 cities, and assign vertex
#' centrality values to each bus stop, in order to use these as proxies for
#' numbers of passengers.
#'
#' @inheritParams who3_network
#' @export
who3_bus_centrality <- function (city) {
    city <- tolower (city)
    f <- file.path (city, "flows", paste0 (city, "-bus.Rds"))
    if (!file.exists (f)) {
        message ("Centrality file [", f, "] does not exist;\n",
                 "calculating centrality ... ", appendLF = FALSE)
        bus <- who3_bus_centrality_internal (city)
        message ("Calculating centrality ... done")
        saveRDS (bus, f)
    } else
        bus <- readRDS (f)
    return (bus)
}

who3_bus_centrality_internal <- function (city) {
    net <- who3_bus_network (city)
    bus_stops <- who3_bus_stops (city)
    dodgr::dodgr_cache_off ()
    net <- dodgr::weight_streetnet (net, wt_profile = 1)
    # re-map bus stop vertices to network:
    v <- dodgr::dodgr_vertices (net)
    index <- dodgr::match_points_to_graph (v, sf::st_coordinates (bus_stops))
    bus_stops$id <- v$id [index]

    flowmat <- array (1, dim = rep (nrow (bus_stops), 2))
    netc <- dodgr::dodgr_contract_graph (net, verts = bus_stops$id)
    netc <- dodgr::dodgr_flows_aggregate (netc, from = bus_stops$id,
                                          to = bus_stops$id,
                                          flows = flowmat, norm_sums = TRUE)
    netm <- dodgr::merge_directed_graph (netc)

    if (grepl ("kathmandu", city, ignore.case = TRUE)) {
        # rename sf-network column names to sc-style
        netm <- dplyr::rename (netm, .vx0 = from_id, .vx1 = to_id)
    }

    # then convert edge centrality to values at actual bus stops
    out_flow <- netm %>%
        dplyr::select (.vx0, flow) %>%
        dplyr::rename (out_flow = flow, id = .vx0) %>%
        dplyr::group_by (id) %>%
        dplyr::summarise (out_flow = sum (out_flow))
    in_flow <- netm %>%
        dplyr::select (.vx1, flow) %>%
        dplyr::rename (in_flow = flow, id = .vx1) %>%
        dplyr::group_by (id) %>%
        dplyr::summarise (in_flow = sum (in_flow))
    out_flow <- tibble::tibble (id = bus_stops$id) %>%
        dplyr::left_join (out_flow, by = "id") %>%
        dplyr::filter (!is.na (out_flow)) %>%
        dplyr::rename (flow = out_flow)
    in_flow <- tibble::tibble (id = bus_stops$id) %>%
        dplyr::left_join (in_flow, by = "id") %>%
        dplyr::filter (!is.na (in_flow)) %>%
        dplyr::rename (flow = in_flow)
    # join both together:
    bus <- dplyr::bind_rows (out_flow, in_flow) %>%
        dplyr::group_by (id) %>%
        dplyr::summarise (flow = sum (flow))

    return (bus)
}
