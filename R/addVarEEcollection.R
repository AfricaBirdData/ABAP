#' Add variable from Google Earth Engine Collection
#'
#' @param ee_pentads A feature collection with the pentads we want to annotate.
#' We need to upload an sf object with the pentads to GEE.
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param dates A character vector with two elements c(start, end). Format must be
#' "yyyy-mm-dd".
#' @param temp_reducer A character string specifying the function to summarize
#' collection across time. It is common to use "mean", "sum" or "count", but
#' there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param spt_reducer A character string specifying the function apply when
#' extracting values for each pentad. It is common to use "mean", "sum" or
#' "count", but there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param bands Select specific bands from the image. If not specified, pentads
#' will be annotated with all bands (one column for each band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#'
#' @return A dataframe similar to \code{ee_pentads} with variables added from the
#' \code{bands} selected from \code{collection}. Note that following \href{https://github.com/r-spatial/rgee}{rgee}
#' the name of the new variables will be the selected band (\code{bands} or else
#' all bands from \code{collection} followed by the spatial reducer \code{spt_reducer}.
#' The temporal reducer \code{temp_reducer} does not appear in the
#' name, and therefore, it is up to the user to keep track of how the temporal
#' reducer summarized the collection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the remote data asset
#' ee_data <- ee$FeatureCollection(assetId)  # assetId must correspond to an asset in your GEE account
#'
#'# Annotate with TerraClimate dataset
#' pentads_tmmn <- addVarEEcollection(ee_pentads = ee_data,
#'                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                    dates = c("2010-01-01", "2011-01-01"),
#'                                    temp_reducer = "mean",
#'                                    spt_reducer = "mean",
#'                                    bands = "tmmn")
#' }
addVarEEcollection <- function(ee_pentads, collection, dates,
                               temp_reducer, spt_reducer,
                               bands = NULL, unmask = FALSE){

    lifecycle::deprecate_warn(
        when = "0.3.0",
        what = "ABAP::addVarEEcollection()",
        with = "ABDtools::addVarEEcollection()",
        details = "A new version of addVarEEcollection() is now on package ABDtools (https://github.com/AfricaBirdData/ABDtools).
        The ABAP function has been discontinued",
        id = NULL,
        always = FALSE,
        env = rlang::caller_env(),
        user_env = rlang::caller_env(2)
    )

    message("This function has been discontinued. Please, use ABDtools::addVarEEcollection() instead")
    message("(https://github.com/AfricaBirdData/ABDtools)")
}
