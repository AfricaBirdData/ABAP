
#' Search species in SABAP2 data base
#'
#' @param species A character string with any part of the common or taxonomic name, or, the SABAP2 species code
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

<<<<<<< HEAD
  # Extract data
  myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
  jsonfile <- rjson::fromJSON(myfile)

  # Reformat
  out <- SABAP::jsonToTibble(jsonfile$data)
=======
  # Read in data
  myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)

  jsonfile <- rjson::fromJSON(myfile)

  out <- jsonfile$data %>%
    lapply(function(x) {
      x[sapply(x, is.null)] <- NA
      x <- unlist(x)
      x[x == "N/A"] <- NA
      cnames <- names(x)
      df <- data.frame()
      df <- rbind(df, x)
      names(df) <- cnames
      return(df)
    }) %>%
    data.table::rbindlist(fill = TRUE) %>%
    dplyr::as_tibble()
>>>>>>> fccea4bcdf3fe760048e52025e63874656682e29

  return(out)
}
