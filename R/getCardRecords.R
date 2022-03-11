#' Download the list of records from an ABAP card
#'
#' @param .CardNo An ABAP card code
#'
#' @return A tibble in which each row corresponds to an entry to an ABAP card.
#' @export
#'
#' @examples
#' getCardRecords("3335_2320_011617_20211219")
getCardRecords <- function(.CardNo){

    url <- paste0("https://api.birdmap.africa/sabap2/v2/card/full/0/", .CardNo)

    # Extract data
    myfile <- httr::RETRY("GET", url) %>%
        httr::content(as = "text", encoding = "UTF-8")

    if(myfile == ""){
        stop("We couldn't retrieve your query. Please check your spelling and try again.")
    }

    jsonfile <- rjson::fromJSON(myfile)

    out <- jsonfile$data$cards[[1]]$records %>%
        data.table::rbindlist(fill = TRUE) %>%
        dplyr::as_tibble()

    return(out)

}
