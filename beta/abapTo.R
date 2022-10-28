#' Format ABAP data for other packages
#'
#' @description Format raw data coming from ABAP to be used on other packages
#' that provide functionality for occupancy analysis.
#' @param abap_data ABAP data downloaded using \code{\link{getAbapData}}
#' @param to A character string with the package data must be format to. Currently,
#' the options are: "unmarked" and "spOccupancy".
#' @param season A character string defining whether the data must be prepared
#' for single season or multiple season occupancy analysis.
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#'
#' @return An object of the target package
#' @export
#'
#' @examples
#' abap_single <- getAbapData(.spp_code = 212,
#'                            .region_type = "province",
#'                            .region = "Eastern Cape",
#'                            .years = 2012)
#'
#' um_single_df <- abapTo(abap_single, to = "unmarked", season = "single")
#' spOcc_single_df <- abapTo(abap_single, to = "spOccupancy", season = "single")
abapTo <- function(abap_data, to = c("unmarked", "spOccupancy"), season = c("single", "multi"),
                   pentads = NULL){

    if(length(to) > 1){
        to <- "unmarked"
    }

    if(length(season) > 1){
        season <- "single"
    }

    if(to == "unmarked" & season == "single"){
        out <- abapToUnmarked_single(abap_data, pentads)
    }

    if(to == "unmarked" & season == "multi"){
        out <- abapToUnmarked_multi(abap_data)
    }

    if(to == "spOccupancy" & season == "single"){
        out <- abapToSpOcc_single(abap_data, pentads)
    }

    return(out)

}
