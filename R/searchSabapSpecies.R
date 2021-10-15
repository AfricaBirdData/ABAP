
#' Search species in SABAP2 data base
#'
#' @param species A character string with any part of the common or taxonomic
#' name, or, the SABAP2 species code (SAFRING code). Note that full names will
#' not work; choose a single word.
#'
#' @return A tibble with taxonomic information and SABAP2 codes of all species
#' matching the search. At the moment, if a species code is passed, the new API
#' is used. For convenience functionality for entering a common name is
#' maintained through the old API, but this might be discontinued.
#' @export
#'
#' @examples
#' searchSabapSpecies("Duck")
#' searchSabapSpecies(169)
#' searchSabapSpecies("Fulvous")
searchSabapSpecies <- function(species){

  # test if species contains letters as opposed to only numbers
  old <- grepl("[a-z]", species, ignore.case = TRUE)

  if(old){
    url <- paste0("http://api.adu.org.za/sabap2/v2/search/species/", species)
  } else {
    url <- paste0("http://api.birdmap.africa/sabap2/v2/species/info/", species, "?curt=1")
  }

  # Extract data
  myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
  jsonfile <- rjson::fromJSON(myfile)

  # Reformat
  out <- SABAP::jsonToTibble(jsonfile$data)

  if(old){
    names(out)[1] <- "SAFRING_No"
  }

  return(out)
}
