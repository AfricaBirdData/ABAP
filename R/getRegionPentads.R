#' Get SABAP2 pentads for a certain region
#'
#' @param .country A character string with the name of the country the region of
#' interest is at. Capitalize first letters of each word.
#' @param .province A character string. If provided it subsets the region of
#' interest to a specific province. Capitalize first letters of each word.
#' @param .path A directory where administrative boundaries layer should be found
#' or stored at.
#'
#' @return A simple feature object with those pentads that are contained in the
#' region of interest. A pentads is considered contained if half of its area
#' lies within the region. Note that the function uses raster::getData function,
#' which will download the administrative boundaries data once per session.
#' Alternatively, you may provide a path where this info will be stored and
#' retrieved from permanently.
#' @export
#'
#' @examples
#' # getRegionPentads(country = "South Africa", province = "North West")
getRegionPentads <- function(.country, .province = NULL, .path = NULL){

    pentads <- SABAP::pentads_sabap

    # Download/load geographic data
    if(is.null(.path)){
        region <- raster::getData("GADM", download = TRUE, country = .country,
                                  level = 1, path = tempdir()) %>%
            sf::st_as_sf()
    } else {
        region <- raster::getData("GADM", download = TRUE, country = .country,
                                  level = 1, path = .path) %>%
            sf::st_as_sf()
    }

    # Cut to study area
    if(!is.null(.province)){
        region <- region %>%
            dplyr::filter(NAME_1 == .province)
    }

    # Find pentads that touch the region
    pentads_reg <- sf::st_intersection(pentads, region)

    # Remove pentads with less than 50% of area in the province:

    # Calculate area
    aa <- sf::st_area(pentads_reg) %>%
        as.numeric()

    # Some pentads might appear more than once (we are interested in total area)
    keep <- pentads_reg %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(area = aa) %>%
        dplyr::group_by(Name) %>%
        dplyr::summarize(total_area = sum(area)) %>%
        dplyr::filter(total_area > (max(total_area)/2)) %>%
        dplyr::pull(Name) %>%
        unique()

    out <- pentads %>%
        dplyr::filter(Name %in% keep)

    return(out)
}

