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
    cities <- c ("accra", "kathmandu", "bristol")
    chk <- vapply (cities, function (i) grepl (i, city), logical (1))
    if (!any (chk))
        stop ("City must be one of [", paste0 (cities, collapse = ", "), "]")

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
    } else if (grepl ("bristol", city, ignore.case = TRUE)) {
        p <- osmdata::getbb(place_name = "Bristol England",
                              format_out = "sf_polygon")
        res <- sf::st_coordinates (p) [, 1:2]
    }

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
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces

    d <- file.path (here::here(), city_dir, "osm")
    f <- file.path (d, paste0 (city_dir, "-hw-sc.Rds"))
    if (file.exists (f))
        hw <- readRDS (f)
    else {
        hw <- who3_network_internal (city, quiet = quiet)
        if (save) {
            if (!file.exists (d))
                dir.create (d, recursive = TRUE)
            saveRDS (hw, file = f)
        }
    }
    return (hw)
}

who3_network_internal <- function (city, quiet = FALSE) {
    bp <- who3_bp (city)

    if (grepl ("accra", city))
        city <- "Greater Accra Region"
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
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces

    d <- file.path (here::here(), city_dir, "flows")
    f <- file.path (d, paste0 (city_dir, "-centrality-edge.Rds"))
    if (file.exists (f))
        net <- readRDS (f)
    else {
        net <- who3_centrality_internal (city, save = save, quiet = quiet)
        if (save) {
            if (!file.exists (d))
                dir.create (d, recursive = TRUE)
            saveRDS (net, f)
            if (!quiet)
                message ("Network save to [", f, "]")
        }
    }
    return (net)
}

who3_centrality_internal <- function (city, save = TRUE, quiet = FALSE) {
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    f <- file.path (here::here(), city_dir, "osm",
                    paste0 (city_dir, "-hw-sc.Rds"))
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
                 "Calculating centrality between ", nrow (netc),
                 " edges ... ", appendLF = FALSE)

    dist_threshold <- 20000 # TODO: Expose as paeam
    st <- system.time (
        netc <- dodgr::dodgr_centrality (netc, dist_threshold = dist_threshold)
    )
    if (!quiet)
        message ("\rCalculated centrality in ",
                 as.numeric (st [3]), " seconds")
    net <- dodgr::dodgr_uncontract_graph (netc)

    return (net)
}

#' who3_buildings
#'
#' @inheritParams who3_network
#' @export
who3_buildings <- function (city, save = TRUE, quiet = FALSE) {
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    f <- file.path (here::here(), city_dir, "osm", paste0 (city_dir, "-bldg.Rds"))
    if (grepl ("accra", city)) {
        f <- file.path (here::here(), "accra", "osm", "accra-gplaces.Rds")
        if (!file.exists (f))
            stop ("Accra places of interest must first be extracted with ",
                  "the 'get_accra_gplaces' function")
        b <- readRDS (f)
        lon <- lat <- x <- y <- NULL # suppress no visible binding notes
        bldg <- dplyr::rename (b, x = lon, y = lat)
    } else {
        if (file.exists (f)) {
            b <- readRDS (f)
            save <- FALSE
        } else
            b <- who3_building_internal (city, save, quiet)
        if (save) {
            dname <- file.path (here::here(), tolower (city_dir), "osm")
            fname <- file.path (dname, paste0 (tolower (city_dir), "-bldg.Rds"))
            if (!file.exists (dname))
                dir.create (dname, recursive = TRUE)
            saveRDS (b, file = fname)
        }

        b <- b$osm_polygons
        suppressWarnings (xy <- sf::st_centroid (b$geometry))
        xy <- sf::st_coordinates (xy)
        bldg <- data.frame (id = b$osm_id,
                            name = b$name,
                            x = xy [, 1],
                            y = xy [, 2],
                            stringsAsFactors = FALSE)
    }

    if (!quiet)
        message ("mapping buildings to street network junctions ... ",
                 appendLF = FALSE)
    net <- who3_network (city) %>%
        dodgr::weight_streetnet (wt_profile = "foot") %>%
        dodgr::dodgr_contract_graph ()
    v <- dodgr::dodgr_vertices (net)

    # Then aggregate buildings to street network intersections:
    index <- dodgr::match_points_to_graph (v, bldg [, c ("x", "y")])
    bldg$id <- v$id [index]
    id <- NULL # suppress no visible binding notes
    bldg <- dplyr::group_by (bldg, id) %>%
        dplyr::summarise (n = length (id),
                          x = x [1],
                          y = y [1])
    if (!quiet)
        message ("\rmapping buildings to street network junctions ... done")

    return (bldg)
}

who3_building_internal <- function (city, save = TRUE, quiet = FALSE) {
    bp <- who3_bp (city)
    bldg <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "building") %>%
        osmdata::osmdata_sf (quiet = quiet) %>%
        osmdata::trim_osmdata (bp)
    return (bldg)
}

#' who3_bus_network
#'
#' Get bus network data for WHO3 cities (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_bus_network <- function (city) {
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    d <- file.path (here::here(), city_dir, "osm")
    f <- file.path (d, paste0 (city_dir, "-bus-net.Rds"))
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
        } else if (grepl ("kathmandu|bristol", city, ignore.case = TRUE)) {
            x <- osmdata::opq (city) %>%
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

    if (savefile) {
        if (!file.exists (d))
            dir.create (d, recursive = TRUE)
        saveRDS (net, f)
    }

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
        f <- file.path (here::here(), "accra", "osm", "accra-bus-net.Rds")
        if (!file.exists (f))
            net <- who3_bus_network (city = "accra")
        net <- readRDS (f)

        type <- role <- NULL # suppress no visible binding notes
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
        b <- osmdata::opq (city) %>%
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
    } else if (grepl ("bristol", city, ignore.case = TRUE)) {
        b <- osmdata::opq (city) %>%
            osmdata::add_osm_feature (key = "highway", value = "bus_stop") %>%
            osmdata::osmdata_sf ()
        bus_stops <- b$osm_points [which (b$osm_points$highway == "bus_stop"), ]
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
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    d <- file.path (here::here(), city_dir, "flows")
    f <- file.path (d, paste0 (city_dir, "-bus.Rds"))
    if (!file.exists (f)) {
        message ("Centrality file [", f, "] does not exist;\n",
                 "calculating centrality ... ", appendLF = FALSE)
        bus <- who3_bus_centrality_internal (city)
        message ("Calculating centrality ... done")
        if (!file.exists (d))
            dir.create (d, recursive = TRUE)
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

    if ("from_id" %in% names (netm)) {
        # rename sf-network column names to sc-style
        .vx0 <- .vx1 <- from_id <- to_id <- NULL
        netm <- dplyr::rename (netm, .vx0 = from_id, .vx1 = to_id)
    }

    # suppress no visible binding notes
    flow <- id <- NULL
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
    # join coordinates back on to vertices:
    x <- y <- NULL # suppress no visible binding notes
    v <- dodgr::dodgr_vertices (netc) %>%
        dplyr::select (id, x, y)
    bus <- dplyr::left_join (bus, v, by = "id")

    return (bus)
}

#' who3_flow
#'
#' Generate pedestrian flows for one WHO3 city (Accra, Kathmandu).
#' @inheritParams who3_network
#' @export
who3_flow <- function (city, save = TRUE, quiet = FALSE) {
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    d <- file.path (here::here(), city_dir, "flows")
    f <- file.path (d, paste0 (city_dir, "-flows.Rds"))
    fsf <- file.path (d, paste0 (city_dir, "-flows-sf.Rds"))
    if (!(file.exists (f) & file.exists (fsf))) {
        res <- who3_flow_internal (city, quiet = quiet)
        net <- res$net
        netsf <- res$netsf
        if (save) {
            if (!file.exists (d))
                dir.create (d, recursive = TRUE)
            saveRDS (net, file = f)
            saveRDS (netsf, file = fsf)
        }
    } else {
        net <- readRDS (f)
        netsf <- readRDS (fsf)
    }
    return (list (net = net, netsf = netsf))
}

who3_flow_internal <- function (city, quiet = FALSE) {
    hw <- who3_network (city)
    dodgr::dodgr_cache_off ()
    if (!quiet)
        message ("Preparing street network ... ", appendLF = FALSE)
    net <- dodgr::weight_streetnet (hw, wt_profile = "foot")
    net <- net [net$component == 1, ]
    if (!quiet)
        message ("\rPreparing street network ... done")

    # ----- dispersal from bus stops:
    bus <- who3_bus_centrality (city)
    # convert density of bus centrality using NYC calibration values
    bus$flow <- 125000 * bus$flow / max (bus$flow)
    if (!quiet)
        message ("Dispersing flows from ", format (nrow (bus), big.mark = ","),
                 " bus stops ... ", appendLF = FALSE)
    netc <- dodgr::dodgr_contract_graph (net, verts = bus$id)
    st <- system.time (
        netf <- dodgr::dodgr_flows_disperse (netc, from = bus$id,
                                             dens = bus$flow, k = 400)
        )
    net1 <- dodgr::dodgr_uncontract_graph (netf)
    if (!quiet)
        message ("\rDispersing flows from ", format (nrow (bus), big.mark = ","),
                 " bus stops in ", st [3], " seconds")

    # ----- dispersal from buildings
    b <- who3_buildings (city, quiet = quiet)
    # Standardise densities of buildings at intersections according to values
    # from NYC calibration of 200 pedestrians per day per building. What is
    # unknown is total number of buildings for each city, so values are
    # standardised to Accra data, which has a total of 20,020 buildings in a
    # city of 2.5 million. From this we derive a scale of 1 activity building
    # per 100 people. Note the particular necessity of this because Kathmandu
    # has enormously more  buildings (142,278), or one for every 10 people. The
    # residential buildings are not marked as such, so all these must be taken
    # as activity centres, with their densities effectively reduced by 10 here.
    get_population <- function(city) {
        switch(tolower (city),
               "accra" = 2.27e6,
               "bristol" = 463000,
               "kathmandu" = 1.74e6)
    }
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    b$n <- b$n * get_population (city_dir) / 100 / sum (b$n)
    # Then scale to value of 200 walking trips per building per day from NYC
    # calibration:
    b$n <- 200 * b$n
    if (grepl ("bristol", city))
        b$n <- 30 * b$n

    if (!quiet)
        message ("Dispersing flows from ", format (nrow (b), big.mark = ","),
                 " buildings ... ", appendLF = FALSE)

    # this dispersal takes a bit longer, because there are 10 times as many
    # points, over 2.5 times the distance
    netc <- dodgr::dodgr_contract_graph (net, verts = b$id)
    st <- system.time (
               netf <- dodgr::dodgr_flows_disperse (netc, from = b$id,
                                                    dens = b$n, k = 1000)
    )
    net2 <- dodgr::dodgr_uncontract_graph (netf)
    if (!quiet)
        message ("\rDispersed flows from ", format (nrow (b), big.mark = ","),
                 " buildings in ", signif (st [3], 3), " seconds")

    net1$flow <- net1$flow + net2$flow

    netm <- dodgr::merge_directed_graph (net1)
    netsf <- dodgr::dodgr_to_sf (netm)

    rmcols <- c ("edge_id", "from_lon", "from_lat", "to_lon", "to_lat",
                 "oneway.bicycle", "oneway:bicycle", "d_weighted",
                 "time", "time_weighted", "component")
    index <- which (!names (netsf) %in% rmcols)
    netsf <- netsf [, index]

    return (list (net = net1, netsf = netsf))
}

#' who3_disperse_centrality
#'
#' Take a network with centrality column, disperse values away from network
#' edges, and map onto pedestrian network.
#' @inheritParams who3_network
#' @param disperse_width Width in metres defining Gaussian dispersal of
#' vehicular emissions.
#' @export
who3_disperse_centrality <- function (city, disperse_width = 200) {
    city <- tolower (city)
    city_dir <- gsub ("\\s.*", "", city) # take just first bit before spaces
    message ("Preparing networks ... ", appendLF = FALSE)

    fnet <- file.path (here::here (), city_dir, "flows",
                       paste0 (city_dir, "-flows.Rds"))
    if (!file.exists (fnet))
        stop ("Flow file [", fnet, "] not found;\n",
              "please first run 'who3_flow'", call. = FALSE)
    netf <- readRDS (fnet)
    if (grepl ("accra", city)) {
        # remove osm_id: 390798097 which is a motorway with alleged bus stops
        id <- "390798097"
        netf$flow [which (netf$object_ == id)] <- 0
    }

    fcent <- file.path (here::here (), city_dir, "flows",
                        paste0 (city_dir, "-centrality-edge.Rds"))
    if (!file.exists (fcent))
        stop ("Flow file [", fcent, "] not found;\n",
              "please first run 'who3_centrality'", call. = FALSE)
    cent <- readRDS (fcent)
    cent$centrality <- cent$centrality / max (cent$centrality)

    netf$centrality <- 0
    cent <- tibble::add_column (cent, flow = 0, .after = "component")
    index1 <- match (netf$edge_, cent$edge_)
    index1 <- index1 [!is.na (index1)]
    index2 <- match (cent$edge_, netf$edge_)
    index2 <- index2 [!is.na (index2)]
    netf$centrality [index2] <- cent$centrality [index1]

    # The 'cent' network includes additional edges not present in the
    # pedestrian-weighted version (such as motorways). These need to be added
    # too:
    netf <- rbind (netf, cent [which (!cent$edge_ %in% netf$edge_), ])

    message ("\rPreparing networks ... done")

    v <- dodgr::dodgr_vertices (netf)
    dist_to_lonlat_range <- function (verts, d = 20) {
        xy0 <- c (mean (verts$x), mean (verts$y))
        names (xy0) <- c ("x", "y")
        minf <- function (a, xy0) {
            abs (geodist::geodist (xy0, xy0 + a) - d)
        }
        stats::optimise (minf, c (0, 0.1), xy0)$minimum
    }
    sig <- dist_to_lonlat_range (v, d = disperse_width)

    message ("Dispersing from centrality ... ", appendLF = FALSE)
    # map centrality values on to vertices:
    .vx0 <- .vx1 <- centrality <- id <- NULL # suppress no visible binding notes
    cent_from <- netf %>%
        dplyr::select (.vx0, centrality) %>%
        dplyr::rename (id = .vx0)
    cent_to <- netf %>%
        dplyr::select (.vx1, centrality) %>%
        dplyr::rename (id = .vx1)
    vc <- dplyr::bind_rows (cent_from, cent_to) %>%
        dplyr::group_by (id) %>%
        dplyr::summarise (centrality = sum (centrality)) %>%
        dplyr::left_join (dodgr::dodgr_vertices (netf), by = "id")

    xy <- suppressWarnings (spatstat::ppp (vc$x, vc$y,
                                           range (vc$x), range (vc$y),
                                           marks = vc$centrality))

    st <- system.time (
        d <- spatstat::Smooth.ppp (xy, weights = vc$centrality,
                                   sigma = sig, at = "points")
        )
    vc$centrality_disp <- d
    message ("\rDispersing from centrality ... done in ",
             signif (st [3], 3), " seconds")

    message ("Mapping back on to network ... ", appendLF = FALSE)
    # Then those values back on to the network edges of netf --> the pedestrian
    # network
    if (".vx0" %in% names (netf)) {
        from_col <- ".vx0"
        to_col <- ".vx1"
    } else {
        from_col <- "from_id"
        to_col <- "to_id"
    }

    netf$c_from_d <- netf$c_to_d <- 0

    index <- which (vc$id %in% netf [[from_col]])
    netf$c_from_d [match (vc$id [index], netf [[from_col]])] <-
        vc$centrality_disp [index]

    index <- which (vc$id %in% netf [[to_col]])
    netf$c_to_d [match (vc$id [index], netf [[to_col]])] <-
        vc$centrality_disp [index]

    netf$centrality_disp <- netf$c_from_d + netf$c_to_d
    netf$c_from_d <- netf$c_to_d <- NULL
    netf$centrality_disp <- netf$centrality_disp / max (netf$centrality_disp)

    cols <- c ("flow", "centrality", "centrality_disp") # columns to keep
    netsf <- dodgr::merge_directed_graph (netf, col_names = cols) %>%
        dodgr::dodgr_to_sf ()
    message ("\rMapping back on to network ... done")

    return (netsf)
}
