#' Helper function to transform a JSON object to a tibble
#'
#' @param jsonfile An object obtained when a JSON file is downloaded
#'
#' @return A tibble
#' @export
#' @examples
#' url <- "http://api.adu.org.za/cwac/sites/list?province=north20%west"
#' myfile <- RCurl::getURL(url, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
#' jsonfile <- rjson::fromJSON(myfile)
#' jsonToTibble(jsonfile)
jsonToTibble <- function(jsonfile){

  out <- jsonfile %>%
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

  return(out)

}
