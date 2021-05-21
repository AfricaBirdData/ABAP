
#' Search species in SABAP2 data base
#'
#' @param species A character string with any part of the common or taxonomic name, or, the SABAP2 species code. Note that full names will not work; choose a single word.
#'
#' @return A tibble with taxonomic information and SABAP2 codes of all species matching the search
#' @export
#'
#' @examples
#' searchSabapSpecies("Duck")
#' searchSabapSpecies(169)
#' searchSabapSpecies("Fulvous")
searchSabapSpecies <- function(species){

  url <- paste0("http://api.adu.org.za/sabap2/v2/search/species/", species)

  # Extract data
  myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
  jsonfile <- rjson::fromJSON(myfile)

  # Reformat
  out <- SABAP::jsonToTibble(jsonfile$data)

  return(out)
}
