#' Get ABAP pentads for a certain region
#'
#' @param .region_type The type of region we are interested on.
#' Three options: "country", "province", "project" and "pentad".
#' @param .region A character string corresponding to the specific region we are
#' interested in. It can be either a country in Southern Africa, a South African
#' province, a project (currently, "SABAP1" or "SABAP2") or a pentad code.
#'
#' @details At the moment, ABAP serves data from eleven countries: South Africa,
#' Botswana, eSwatini, Lesotho, Malawi, Mozambique, Namibia, Zambia, Zimbabwe,
#' Kenya and Nigeria. Only South African provinces are available: Western Cape,
#' Eastern Cape, Northern Cape, Free State, Limpopo, Mpumalanga, KwaZulu-Natal,
#' Gauteng, and Northwest Province.
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

    # Download geojson data
    geojson_file <- httr::GET(url)
    geojson_content <- httr::content(geojson_file, encoding = "UTF-8", as = "text")

    # Extract data
    pentads <- sf::read_sf(geojson_content)

    # Clean and return
    pentads <- pentads %>%
        dplyr::rename_with(.fn = ~gsub(".", "_", .x, fixed = TRUE)) # remove points in names (GEE doesn't like)

    if(!is.null(pentads$format)){
        pentads <- pentads %>%
            dplyr::select(-format)
    }

    return(pentads)
}

