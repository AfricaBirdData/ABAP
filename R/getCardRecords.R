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

    jsonfile <- rjson::fromJSON(myfile)

    out <- jsonfile$data$cards[[1]]$records %>%
        dplyr::bind_rows() %>%
        dplyr::as_tibble()

    if(sum(dim(out)) == 0){
        warning("There are no species records for that entry. You may need to check the card number.")
    }

    return(out)

}
