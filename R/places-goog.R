
# google returns a maximum of 200 places within the circle of specified radius
radar_search <- function (location, radius = 200)
{
    api_key <- Sys.getenv ("GOOGLE_API")
    url <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?"
    url_full <- paste0 (url, "location=", paste0 (location, collapse = ","),
                        "&radius=", radius, "&key=", api_key)
    res <- jsonlite::fromJSON (url_full)$results
    xy <- data.frame (lon = res$geometry$location$lng,
                      lat = res$geometry$location$lat)
    res %<>% dplyr::select (c ("name", "place_id", "reference", "types")) %>%
        dplyr::mutate (lon = xy$lon, lat = xy$lat)
}

accra_coordinates <- function (radius = 200)
{
    sw <- as.matrix (cbind (5.521483, -0.271064)) # SW corner of accra
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

    xgp <- rep (c (seq (swgp [, 2], swgp [, 2] + 30000, lonShift),
                   seq (swgp [, 2] + 0.5 * lonShift,
                        swgp [, 2] + (30000 - 0.5 * lonShift), lonShift)),
                floor (latRep / 2))
    # in case there is an odd number of rows, add another full row of x coords at the top
    if (latRep - (2 * floor (latRep / 2)) != 0)
        xgp <- c (xgp, seq (swgp [, 2], swgp [, 2] + 30000, lonShift))

    ygp <- rep (swgp [, 1], lonRep)
    for (i in 2:latRep)
    {
        if (i %% 2 == 0)
            ygp <- c (ygp, rep (swgp [, 1] + (i - 1) * latShift, lonRep - 1))
        else
            ygp <- c (ygp, rep (swgp [, 1] + (i - 1) * latShift, lonRep))
    }
    xy <- sf::sf_project ("+init=epsg:25000","+proj=longlat +datum=WGS84",
                    as.matrix (cbind (ygp, xgp)))

    # clip to Accra metropolitan district shapefile - should already be in WGS84
    accraSHP <- rgdal::readOGR (file.path (here::here (), "accra",
                                           "AccraMetroAdmin/"),
                                verbose = FALSE)
    xy_bdry <- slot (slot (accraSHP, "polygons") [[1]], "Polygons") [[1]]
    xy_bdry <- slot (xy_bdry, "coords") # these are [lon, lat]
    indx <- which (sp::point.in.polygon (xy [, 2], xy [, 1],
                                         xy_bdry [, 1], xy_bdry [, 2]) == 1)
    # proj4string (accraSHP)
    xy [indx, ]
}

