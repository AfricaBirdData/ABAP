#' Upload pentads to Google Earth Engine server
#'
#' @param pentads An \href{https://github.com/r-spatial/sf}{sf} object with the pentads to upload
#' @param asset_id A character string with the name we want our pentads to be
#' saved on the server with.
#' @param load If TRUE (default), the GEE asset is loaded into the R session.
#' @param max_p Maximum number of pentads the function will try to upload
#' without splitting into pieces. The default is a sensible choice but you can
#' try modify it to serve your purposes.
#'
#' @return Pentads are uploaded to GEE. In addition a GEE feature collection can
#' be loaded into the R environment
#' @details An \href{https://github.com/r-spatial/sf}{sf} object is uploaded to GEE servers via
#' "getInfo_to_asset" (see \link[rgee]{sf_as_ee}). If there are many pentads,
#' the function will upload to objects and merge them in the server under the
#' name provided. Please be conscious that two intermediate objects will also
#' be stored under names "p1" and "p2". We recommend visiting your account and
#' cleaning unnecesary objects regularly.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load ABAP pentads
#' pentads <- getRegionPentads(.region_type = "province", .region = "North West")
#'
#' # Set an ID for your remote asset (data in GEE)
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'pentads')
#'
#' # Upload to GEE (if not done already - do this only once per asset)
#' uploadPentadsToEE(pentads = pentads,
#'                   asset_id = assetId,
#'                   load = FALSE)
#'
#' # Load the remote asset to you local computer to work with it
#' ee_pentads <- ee$FeatureCollection(assetId)
#'
#' # Alternatively we can upload to GEE and load the asset in one call
#' ee_pentads <- uploadPentadsToEE(pentads = pentads,
#'                                 asset_id = assetId,
#'                                 load = TRUE)
#' }
uploadPentadsToEE <- function(pentads, asset_id, load = TRUE, max_p = 16250){

    lifecycle::deprecate_warn(
        when = "0.3.0",
        what = "ABAP::uploadPentadsToEE()",
        with = "ABDtools::uploadFeaturesToEE()",
        details = "ABAP::uploadPentadsToEE() is replaced by ABDtools::uploadFeaturesToEE() from package ABDtools (https://github.com/AfricaBirdData/ABDtools).
        The ABAP function has been discontinued",
        id = NULL,
        always = FALSE,
        env = rlang::caller_env(),
        user_env = rlang::caller_env(2)
    )

  message("This function has been discontinued. Please, use ABDtools::uploadFeaturesToEE() instead")
  message("(https://github.com/AfricaBirdData/ABDtools)")

}
