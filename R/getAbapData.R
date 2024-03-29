#' Download data from ABAP cards for a region and species
#'
#' @param .spp_code Character string or integer corresponding to the SAFRING
#'   code of the species of interest. See searchAbapSpecies.
#' @param .region_type The type of region we are interested on. Four options:
#'   "country", "province", "pentad" and "group". See details.
#' @param .region A character string corresponding to the specific region we are
#'   interested in. It can be either a country in Southern Africa, a South
#'   African province or a pentad code.
#' @param .years A numeric vector with elements corresponding to the years we
#'   want data for.
#' @param .adhoc If TRUE, **only** ad-hoc lists are returned. If FALSE
#'   (default), no ad-hoc lists are returned.
#'
#' @returns A tibble in which each row corresponds to one ABAP card. The column
#' 'Spp' gives either the code of the species of interest, if it was detected
#' in that card, or "-" if it wasn't. See \code{\link{abap_visit_vars}} for more
#' information about the columns.
#'
#' @details At the moment, ABAP serves data from eleven countries: South Africa,
#' Botswana, eSwatini, Lesotho, Malawi, Mozambique, Namibia, Zambia, Zimbabwe,
#' Kenya and Nigeria. Only South African provinces are available: Western Cape,
#' Eastern Cape, Northern Cape, Free State, Limpopo, Mpumalanga, KwaZulu-Natal,
#' Gauteng, and Northwest Province.
#'
#' If we specify "group" in the `.region` argument the
#' function returns all records for a specific group of pentads. Groups of
#' pentads must be created from the birdmap.africa websites (e.g.,
#' \href{https://sabap2.birdmap.africa/}{SABAP2} or
#' \href{https://kenya.birdmap.africa/}{Kenya Bird Map}). You will need to
#' create an account. Once logged in, you can create groups from the `coverage`
#' menu. Then, these groups can be viewed from you home menu. The name of the
#' group is the last part of the URL displayed in the browser's navigation bar.
#' For example, I created a group named "test_group", and the URL for this group
#' is `https://kenya.birdmap.africa/coverage/group/xxxx_tst_grp`. The group name
#' that we need to pass on to the `getAbapData()` function is `xxxx_tst_grp`,
#' the last part of the URL, where `xxxx` is your citizen scientist number
#' (provided when creating an account).
#'
#' @export
#'
#' @examples
#' getAbapData(123, .region_type = "country", .region = "South Africa")
#' getAbapData(169, .region_type = "province", .region = "Eastern Cape", .years = 2008)
#' getAbapData(95, .region_type = "pentad", .region = "2505_2850", .years = c(2008, 2009))
getAbapData <- function(.spp_code,
                         .region_type = c("country", "province", "pentad", "group"),
                         .region, .years = NULL, .adhoc = FALSE){

  .region_type <- match.arg(.region_type)

  if(!is.null(.years)){
    .years <- paste(.years, collapse = ",")
    .years <- paste0("&year=", .years)
  }

  .region <- tolower(.region)
  .region <- gsub(" ", "", .region)

  url <- paste0("https://api.birdmap.africa/sabap2/v2/cards/species/85ee37929696cba93e1cdda4dbb3f93a/", .spp_code, "/",
                .region_type, "/", .region, "?format=csv&inclnull=1", .years)

  if(.adhoc){
    url <- paste0(url, "&adhoc=1")
  }

  # Extract data
  myfile <- httr::RETRY("GET", url) %>%
    httr::content(as = "text", encoding = "UTF-8")

  if(myfile == ""){
    stop("We couldn't retrieve your query. Please check your spelling and try again.")
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
