
#' get_accra_gplaces
#'
#' Get list of places according to googel
#'
#' @param radius Radius to be used for googel nearby place search (in metres)
#' @return A \code{data.frame} of place locations, names, and types
#' @export
get_accra_gplaces <- function (radius = 200)
{
    xy <- accra_coordinates ()
    api_key <- Sys.getenv ("GOOGLE_API")
    pb <- txtProgressBar (style = 3)
    places <- NULL
    for (i in seq (nrow (xy)))
    {
        places <- rbind (places, radar_search (xy [i, ], radius = radius,
                                               api_key = api_key))
        setTxtProgressBar (pb, i / nrow (xy))
    }
    close (pb)

    places <- places [!duplicated (places$place_id), ]
    saveRDS (places, file = file.path (here::here (), "accra", "osm",
                                       "accra-gplaces.Rds")
    return (places)
}

# google returns a maximum of 200 places within the circle of specified radius
radar_search <- function (location, radius = 200, api_key)
{
    url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
    url_full <- paste0 (url, "location=", paste0 (location, collapse = ","),
                        "&radius=", radius, "&key=", api_key)
    res <- jsonlite::fromJSON (url_full)$results
    if (length (res) > 0)
    {
        xy <- data.frame (lon = res$geometry$location$lng,
                          lat = res$geometry$location$lat)
        res %<>% dplyr::select (c ("name", "place_id", "reference", "types")) %>%
            dplyr::mutate (lon = xy$lon, lat = xy$lat)
    } else
        res <- NULL

    return (res)
}

# NOTE: Function returns (y, x) because the googel places API requires 
# (lat, lon)
accra_coordinates <- function (radius = 200)
{
    xy_bdry <- accra_boundary ()
    # Start at SW corner
    sw <- matrix (rev (apply (xy_bdry, 2, min)), nrow = 1) # (y, x), not (x, y)!
    #sw <- as.matrix (cbind (5.521483, -0.271064)) # SW corner of accra
    swgp <- sf::sf_project ("+proj=longlat +datum=WGS84", "+init=epsg:25000", sw)

    # Determine number of times to repeat in latitudinal and longitudinal
    # directions for hexagonal packing of circles with radius radiusM for 30 km
    # W-E and 20 km N-S coverage

    lon_shift <- 2 * radius * (sin (60 * pi / 180))
    lat_shift <- sin (60 * pi / 180) * lon_shift
    lon_rep <- ceiling (30000 / lon_shift)
    lat_rep <- ceiling (20000 / lat_shift)
    # distance to the next row of y coords is sin(60) * 1000, so 500 + x times
    # this for each next row

    xgp <- rep (c (seq (swgp [, 2], swgp [, 2] + 30000, lon_shift),
                   seq (swgp [, 2] + 0.5 * lon_shift,
                        swgp [, 2] + (30000 - 0.5 * lon_shift), lon_shift)),
                floor (lat_rep / 2))
    # in case there is an odd number of rows, add another full row of x coords at the top
    if (lat_rep - (2 * floor (lat_rep / 2)) != 0)
        xgp <- c (xgp, seq (swgp [, 2], swgp [, 2] + 30000, lon_shift))

    ygp <- rep (swgp [, 1], lon_rep)
    for (i in 2:lat_rep)
    {
        if (i %% 2 == 0)
            ygp <- c (ygp, rep (swgp [, 1] + (i - 1) * lat_shift, lon_rep - 1))
        else
            ygp <- c (ygp, rep (swgp [, 1] + (i - 1) * lat_shift, lon_rep))
    }
    xy <- sf::sf_project ("+init=epsg:25000","+proj=longlat +datum=WGS84",
                    as.matrix (cbind (ygp, xgp)))

    # clip to Accra metropolitan district
    indx <- which (sp::point.in.polygon (xy [, 2], xy [, 1],
                                         xy_bdry [, 1], xy_bdry [, 2]) == 1)
    xy [indx, ]
}

# Accra metropolitan district shapefile
accra_boundary <- function ()
{
    accraSHP <- rgdal::readOGR (file.path (here::here (), "accra",
                                           "AccraMetroAdmin/"),
                                verbose = FALSE)
    xy <- slot (slot (accraSHP, "polygons") [[1]], "Polygons") [[1]]
    slot (xy, "coords")
}
