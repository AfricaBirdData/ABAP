
#' Search species in ABAP data base
#'
#' @param species A character string with any part of the common or taxonomic
#' name, or, the SAFRING code. Note that full names will
#' not work; choose a single word.
#'
#' @return A tibble with taxonomic information and codes of all species
#' matching the search. At the moment, if a species code is passed, the new API
#' is used. For convenience functionality for entering a common name is
#' maintained through the old API, but this might be discontinued.
#' @export
#'
#' @examples
#' searchAbapSpecies("Duck")
#' searchAbapSpecies(169)
#' searchAbapSpecies("Fulvous")
searchAbapSpecies <- function(species){

    url <- paste0("https://api.birdmap.africa/sabap2/v2/search/species/", species)

    # Extract data
    myfile <- httr::RETRY("GET", url) %>%
        httr::content(as = "text", encoding = "UTF-8")

    if(myfile == ""){
        stop("We couldn't retrieve your querry. Please check your spelling and try again.")
    }

    jsonfile <- rjson::fromJSON(myfile)

    # Reformat
    out <- ABAP::jsonToTibble(jsonfile$data)

    return(out)

}
