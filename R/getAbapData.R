#' Download data from ABAP cards for a region and species
#'
#' @param .spp_code Character string or integer corresponding to the SAFRING code
#' of the species of interest. See searchAbapSpecies.
#' @param .region_type The type of region we are interested on.
#' Three options: "country", "province" and "pentad".
#' @param .region A character string corresponding to the specific region we are
#' interested in. It can be either a country in Southern Africa, a South African
#' province or a pentad code.
#' @param .years A numeric vector with elements corresponding to the years we
#' want data for.
#' @param .adhoc If TRUE, **only** ad-hoc lists are returned. Defaults to FALSE.
#'
#' @return A tibble in which each row corresponds to one ABAP card. The column
#' 'Spp' gives either the code of the species of interest, if it was detected
#' in that card, or "-" if it wasn't.
#' @export
#'
#' @examples
#' getAbapData(123, .region_type = "country", .region = "South Africa")
#' getAbapData(169, .region_type = "province", .region = "Eastern Cape", .years = 2008)
#' getAbapData(95, .region_type = "pentad", .region = "2505_2850", .years = c(2008, 2009))
getAbapData <- function(.spp_code,
                         .region_type = c("country", "province", "pentad"),
                         .region, .years = NULL, .adhoc = FALSE){

  if(is.null(.region_type)){
    .region_type <- "country"
  } else if(!.region_type %in% c("country", "province", "pentad")){
    stop(".region_type must be one of 'country', 'province', 'pentad'")
  }

  if(!is.null(.years)){
    .years <- paste(.years, collapse = ",")
    .years <- paste0("&year=", .years)
  }

  .region <- tolower(.region)
  .region <- gsub(" ", "", .region)

  url <- paste0("http://api.birdmap.africa/sabap2/v2/cards/species/85ee37929696cba93e1cdda4dbb3f93a/", .spp_code, "/",
                .region_type, "/", .region, "?format=csv&inclnull=1", .years)

  if(.adhoc){
    url <- paste0(url, "&adhoc=1")
  }

  # Extract data
  myfile <- httr::RETRY("GET", url) %>%
    httr::content(as = "text", encoding = "UTF-8")

  if(myfile == ""){
    stop("We couldn't retrieve your querry. Please check your spelling and try again.")
  }

  # Format
  out <- myfile %>%
    textConnection() %>%
    utils::read.csv(header = TRUE) %>%
    dplyr::as_tibble()

  # If there are multiple regions (e.g. multiple pentads), then multiple headers
  # are produced and inserted as data. Eliminate these
  out <- out %>%
    dplyr::filter(Pentad != "Pentad")

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
