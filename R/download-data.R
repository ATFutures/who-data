#' get_who_streets
#'
#' Extract OSM streets for given location (\code{city}), and save them in the
#' data directory
#'
#' @param city Name of city for which streets are to be obtained
#' @param n Number of chunks into which to divide the file (see details)
#' @return The \pkg{sf}-formatted data object (invisibly)
#'
#' @note github only stores single files under 5MB, so setting n > 1 enables a
#' file to be divided into individual chunks smaller than this limit which can
#' be stored and easily \code{rbind}-ed back together on loading.
#'
#' @export
get_who_streets <- function (city = "kathmandu", n = 1)
{
    is_sf_loaded ()
    region_shape <- osmdata::getbb(place_name = city, format_out = "polygon")
    if (is.list (region_shape))
        region_shape <- region_shape [[1]]

    dat <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "highway") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        osmdata::trim_osmdata (region_shape) %>%
        osmdata::osm_poly2line () %>%
        magrittr::extract2 ("osm_lines")

    # Reduce to only fields with > 1 unique value
    len <- apply (dat, 2, function (i) length (unique (i)))
    dat <- dat [, which (len > 1)]

    indx <- seq (nrow (dat))
    if (n == 1)
        indx <- list (indx)
    else
        indx <- split (indx, cut (indx, n))

    np <- file_number_ext (n)

    for (i in seq (indx))
    {
        write_who_data (dat [indx [[i]], ], city = city, suffix = "hw",
                        n = np [i])
    }

    invisible (dat)
}

#' get_who_buildings
#'
#' Extract OSM buildings for given location (\code{city}), and save them in the
#' data directory
#' @param city Name of city for which buildings are to be obtained
#' @param n Number of chunks into which to divide the file (see details)
#' @return The \pkg{sf}-formatted data object (invisibly)
#'
#' @export
get_who_buildings <- function (city = "kathmandu", n = 1)
{
    is_sf_loaded ()
    region_shape <- osmdata::getbb(place_name = city, format_out = "polygon")
    if (is.list (region_shape))
        region_shape <- region_shape [[1]]

    dat <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "building") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        osmdata::trim_osmdata (region_shape)

    dat$osm_points <- dat$osm_lines <- dat$osm_multilines <- NULL

    # Reduce to only fields with > 1 unique value
    len <- apply (dat$osm_polygons, 2, function (i) length (unique (i)))
    dat$osm_polygons <- dat$osm_polygons [, which (len > 1)]
    len <- apply (dat$osm_multipolygons, 2, function (i) length (unique (i)))
    dat$osm_multipolygons <- dat$osm_multipolygons [, which (len > 1)]

    indx1 <- seq (nrow (dat$osm_polygons))
    indx2 <- seq (nrow (dat$osm_multipolygons))
    if (n == 1)
    {
        indx1 <- list (indx1)
        indx2 <- list (indx2)
    } else
    {
        indx1 <- split (indx1, cut (indx1, n))
        indx2 <- split (indx2, cut (indx2, n))
    }

    np <- file_number_ext (n)

    dat_full <- dat
    for (i in seq (n))
    {
        dat <- dat_full
        dat$osm_polygons <- dat$osm_polygons [indx1 [[i]], ]
        dat$osm_multipolygons <- dat$osm_multipolygons [indx2 [[i]], ]
        write_who_data (dat, city = city, suffix = "bldg", n = np [i])
    }

    invisible (dat_full)
}

lonlat2UTM <- function(lonlat)
{                  
    utm <- (floor ( (lonlat [1] + 180) / 6) %% 60) + 1   
    if (lonlat [2] > 0)
        utm + 32600
    else               
        utm + 32700                                      
}                                                
                                                     
#' get_bus_polygon_centroids
#'
#' @param dat An \pkg{osmdata} object representing either \code{key = "bus"} or
#' \code{key = "public_transport"}, both of which may return polygons
#' representing bus stations or platforms
#' @return Modified verion of input in which centroids of polygons have been
#' added to the \code{$osm_points}.
#' @export
get_bus_polygon_centroids <- function (dat)
{
    polys <- dat$osm_polygons
    xy <- NULL
    if (nrow (polys) > 0)
    {
        # sample one point to determine UTM
        utm <- lonlat2UTM (polys$geometry [[1]] [[1]] [1, ])
        suppressWarnings ({
            xy <- sf::st_transform (polys, utm) %>%
                sf::st_centroid () %>%
                sf::st_transform (., sf::st_crs (polys)$proj4string) %>%
                sf::st_geometry ()
        })
    }
    return (xy)
}

#' get_who_busstops
#'
#' Extract OSM bus stops for given location (\code{city}), and save them in the
#' data directory. Note that coordinates only are extracted, because some bus
#' facilitites are polygonal stations which are simply converted to centroids.
#'
#' @param city Name of city for which bus stops are to be obtained
#' @return The \pkg{sf}-formatted data object (invisibly)
#'
#' @export
get_who_busstops <- function (city = "kathmandu")
{
    region_shape <- osmdata::getbb(place_name = city, format_out = "polygon")
    if (is.list (region_shape))
        region_shape <- region_shape [[1]]

    dat1 <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "highway", value = "bus_stop") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        osmdata::trim_osmdata (region_shape) %>%
        magrittr::extract2 ("osm_points")
    dat2 <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "public_transport",
                                  value = "stop_position") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        osmdata::trim_osmdata (region_shape) %>%
        magrittr::extract2 ("osm_points")
    dat3 <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "public_transport") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        get_bus_polygon_centroids ()
    dat4 <- osmdata::opq (bbox = city) %>%
        osmdata::add_osm_feature (key = "bus") %>%
        osmdata::osmdata_sf (quiet = FALSE) %>%
        get_bus_polygon_centroids ()

    dat <- c (dat1$geometry, dat2$geometry, dat3, dat4)
    dat <- dat [!duplicated (dat)]

    write_who_data (dat, city = city, suffix = "bs")

    invisible (dat)
}

# n is a number appended to file name when divided into chunks
write_who_data <- function (dat, city, suffix, n = NULL)
{
    nm <- paste0 (city, "_", suffix, n)
    assign (nm, dat)
    data_dir <- get_who_data_dir (city = city)
    fname <- file.path (data_dir, paste0 (city, "-", suffix, n, ".Rds"))
    saveRDS (get (nm), fname)
    message ("saved ", fname)
}

#' get_who_data_dir
#'
#' Find the "who-data" directory corresponding to the "who" directory of this
#' project, and the sub-directory within that corresponding to the OSM data of
#' the named city.  The sub-dir will be created if it does not already exist.
#'
#' @param city Name of city for which data are obtained, and name of
#' corresponding sub-directory in "who-data" where data are to be stored.
#'
#' This assumes this repo ("who") sits in the same root directory as the
#' corresponding one named "who-data". The latter is where the function
#' \code{get_who_data} stores data.
#' @noRd
get_who_data_dir <- function (city)
{
    dh <- file.path (here::here (), city)
    if (!file.exists (dh))
        dir.create (dh)
    dh <- file.path (dh, "osm")
    if (!file.exists (dh))
        dir.create (dh)

    return (dh)
}

# convert a range number into a series of 0-padded file number extensions
file_number_ext <- function (n)
{
    np <- ""
    if (n > 1)
    {
        np <- sapply (seq (n), function (i)
                      formatC (i, width = ceiling (log10 (n + 1)), flag = "0"))
    }
    return (np)
}

is_sf_loaded <- function ()
{
    if (!any (grepl ("package:sf", search ())))
        message ("It is generally necessary to pre-load the sf package ",
                 "for this functions to work correctly")
}

