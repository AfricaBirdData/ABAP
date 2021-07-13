#' Download data from SABAP2 cards for a region and species
#'
#' @param .spp_code Character string or integer corresponding to the SABAP2 code
#' of the species of interest. See searchSabapSpecies.
#' @param .region_type The type of region we are interested on.
#' Three options: "country", "province" and "pentad".
#' @param .region A character string corresponding to the specific region we are
#' interested in. It can be either a country in Southern Africa, a South African
#' province or a pentad code.
#' @param .years A numeric vector with elements corresponding to the years we
#' want data for.
#'
#' @return A tibble in which each row corresponds to one SABAP2 card. The column
#' 'Spp' gives either the code of the species of interest, if it was detected
#' in that card, or "-" if it wasn't.
#' @export
#'
#' @examples
#' getSabapData(123, region_type = "country", region = "South Africa")
#' getSabapData(169, region_type = "province", region = "Eastern Cape", years = 2008)
#' getSabapData(95, region_type = "pentad", region = "2505_2850", years = c(2008, 2009))
getSabapData <- function(.spp_code,
                         .region_type = c("country", "province", "pentad"),
                         .region, .years = NULL){

  if(is.null(.region_type)){
    .region_type <- "country"
  }

  if(!is.null(.years)){
    .years <- paste0("&year=", .years)
  }

  .region <- tolower(.region)
  .region <- gsub(" ", "", .region)

  url <- paste0("http://api.adu.org.za/sabap2/v2/cards/species/85ee37929696cba93e1cdda4dbb3f93a/", .spp_code, "/",
                .region_type, "/", .region, "?format=csv&inclnull=1", .years)

  # Extract data
  myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)

  # Format
  out <- myfile %>%
    textConnection() %>%
    utils::read.csv(header = TRUE) %>%
    dplyr::as_tibble()

  out <- out %>%
    readr::type_convert(col_types = readr::cols(
      .default = readr::col_integer(),
      CardNo = readr::col_character(),
      StartDate = readr::col_date(format = ""),
      EndDate = readr::col_date(format = ""),
      StartTime = readr::col_character(),
      Pentad = readr::col_character(),
      Spp = readr::col_character(),
      Sequence = readr::col_character(),
      Common_name = readr::col_character(),
      Taxonomic_name = readr::col_character()))

  return(out)
}
