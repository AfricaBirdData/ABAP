#' Get ABAP pentads for a certain region
#'
#' @param .region_type The type of region we are interested on.
#' Three options: "country", "province", "project" and "pentad".
#' @param .region A character string corresponding to the specific region we are
#' interested in. It can be either a country in Southern Africa, a South African
#' province, a project (currently, "SABAP1" or "SABAP2") or a pentad code.
#'
#' @return A simple feature object with those pentads that are contained in the
#' region of interest. A pentads is considered contained if half of its area
#' lies within the region.
#' @export
#'
#' @examples
#' # getRegionPentads(.region_type = "country", .region = "South Africa")
getRegionPentads <- function(.region_type, .region){

    .region_type <- tolower(.region_type)
    .region <- tolower(.region)
    .region <- gsub(" ", "", .region)

    url <- paste0("https://api.birdmap.africa/sabap2/v2/pentads/", .region_type,
                  "/", .region, "?format=geoJSON")

    # Extract data
    pentads <- sf::read_sf(url)

    # Clean and return
    pentads <- pentads %>%
        dplyr::rename_with(.fn = ~gsub(".", "_", .x, fixed = TRUE)) # remove points in names (GEE doesn't like)

    if(!is.null(pentads$format)){
        pentads <- pentads %>%
            dplyr::select(-format)
    }

    return(pentads)
}

