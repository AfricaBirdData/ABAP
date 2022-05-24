#' Add Google Earth Engine covariate data to a list for use in spOccupancy
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}} functions. `addEEtoSpOcc_single` can then be used to add these types of data to a list in order to fit single-species occupancy models using \code{\link[spOccupancy]{spPGOcc}} (spatial) or \code{\link[spOccupancy]{PGOcc}} (non-spatial) from the `spOccupancy`package.
#'
#' @param spOcc an list containing ABAP detection/non-detection data returned by \code{\link{abapToSpOcc_single}}.
#' @param ee_data a data frame with GEE data extracted using \code{\link{addVarEEcollection}} or \code{\link{addVarEEimage}}.
#' @param ee_name a user-defined character string giving the desired name of the GEE site or site-year covariate.
#'
#' @note The numeric ranges of various GEE data can be vastly different so it is advised that you scale your covariate data before running an occupancy model. See example below for how to do this.
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @seealso \code{\link[spOccupancy]{spPGOcc}}, \code{\link[spOccupancy]{PGOcc}}, \code{\link{addVarEEimage}}, \code{\link{addVarEEcollection}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(unmarked)
#'
#' ## Extract ABAP pentad data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                 .region = "Eastern Cape")
#'
#' ## Extract single season ABAP bird data
#' abap_single <- getAbapData(.spp_code = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = 2012)
#'
#' ## Create spOcc list (with X & Y coords as site covariates)
#' spOcc_single <- abapToSpOcc_single(abap_single, abap_pentads)
#' str(spOcc_single)
#'
#' ## Start up GEE
#' ee_check()
#' ee_Initialize(drive = TRUE)
#'
#' ## Create assetId for pentads of interest
#' assetId <- sprintf("%s/%s", ee_get_assethome(), 'EC_pentads')
#'
#' ## Upload to pentads to GEE (only run this once per asset)
#' uploadPentadsToEE(pentads = abap_pentads,
#'                  asset_id = assetId,
#'                  load = FALSE)
#'
#' ## Load the remote asset into R session
#' pentads <- ee$FeatureCollection(assetId)
#'
#' ## Extract mean NDVI for each pentad
#' ndvi_mean <- addVarEEcollection(ee_pentads = pentads,
#'                                   collection = "MODIS/006/MOD13A2",
#'                                   dates = c("2010-01-01", "2013-01-01"),
#'                                   temp_reducer = "mean",
#'                                   spt_reducer = "mean",
#'                                   bands = "NDVI")
#'
#' ## Extract mean estimate of surface water occurrence for each pentad
#' water_mean <- addVarEEimage(ee_pentads = pentads,
#'                            image = "JRC/GSW1_3/GlobalSurfaceWater",
#'                            reducer = "mean",
#'                            bands = "occurrence")
#'
#' ## Add to NDVI covariate to spOcc list
#' spOcc_single_ee <- addEEtoSpOcc_single(spOcc = spOcc_single,
#'                                        ee_data = ndvi_mean,
#'                                        ee_name = "NDVI_mean")
#' str(spOcc_single_ee)
#'
#' ## Add surface water covariate to spOcc list
#' spOcc_single_ee <- addEEtoSpOcc_single(spOcc = spOcc_single,
#'                                        ee_data = water_mean,
#'                                        ee_name = "water_mean")
#' str(spOcc_single_ee)
#'
#' ## Add both covariates simultaneously using the pipe operator
#' spOcc_single_ee <- spOcc_single %>%
#'     addEEtoSpOcc_single(., ndvi_mean, "NDVI_mean") %>%
#'     addEEtoSpOcc_single(., water_mean, "water_mean")
#'
#' str(spOcc_single_ee)
#'
#' ## Scale site covariates if neccessary
#' spOcc_single_ee$occ.covs <- scale(spOcc_single_ee$occ.covs)
#' head(spOcc_single_ee$occ.covs)
#' summary(spOcc_single_ee$occ.covs)
#' }
addEEtoSpOcc_single <- function(spOcc, ee_data, ee_name) {

    reducer_names <- c(
        "mean", "max", "min", "count", "median",
        "sum", "stdDev", "variance"
    )

    builtin_names <- c(
        "id", "Name", "country", "full_qdgc",
        "pentad", "province", "qdgc", "geometry"
    )

    cols_keep <- reducer_names[reducer_names %in% names(ee_data)]

    if (length(cols_keep) == 0) {
        cols_keep <- names(ee_data)[!names(ee_data) %in% builtin_names]
    }

    spOcc_pentads <- dimnames(spOcc$y)[[1]]

    site_cov <- ee_data %>%
        sf::st_drop_geometry() %>%
        dplyr::filter(pentad %in% spOcc_pentads) %>%
        dplyr::arrange(match(pentad, spOcc_pentads)) %>%
        dplyr::select(dplyr::all_of(cols_keep)) %>%
        dplyr::rename_with(.cols = 1, ~ {{ ee_name }})

    if (is.null(spOcc$occ.covs)) {
        spOcc$occ.covs <- as.matrix(site_cov)
    } else {
        spOcc$occ.covs <- as.matrix(cbind(spOcc$occ.covs, site_cov))
    }

    return(spOcc)
}
