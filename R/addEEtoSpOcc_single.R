#' Add Google Earth Engine covariate data to a list for use in spOccupancy
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted
#' to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}}
#' functions. `addEEtoSpOcc_single` can then be used to add these types of data
#' to a list in order to fit single-species occupancy models using \code{\link[spOccupancy]{spPGOcc}}
#' (spatial) or \code{\link[spOccupancy]{PGOcc}} (non-spatial) from the `spOccupancy`
#' package.
#'
#' @param spOcc a list containing ABAP detection/non-detection data returned by
#' \code{\link{abapToSpOcc_single}}.
#' @param ee_data a data frame with GEE data extracted using \code{\link{addVarEEcollection}}
#' or \code{\link{addVarEEimage}}. The data frame needs to contain a column called
#' `pentads` with the pentad ID from data extracted using the GEE functions.
#' The remaining columns should only contain the GEE covariate values that are
#' intended to be added to the `spOccupancy` list. For ease of use in occupancy
#' model formula notation it's recommended that the variable names in the data
#' frame are concise and informative, and don't contain spaces. See the example
#' below for how to create this data frame after extracting GEE data.
#'
#' @note The numeric ranges of various GEE data can be vastly different so it
#' is advised that you scale your covariate data before running an occupancy
#' model. See the example below for how to do this.
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @seealso \code{\link{abapToSpOcc_single}}, \code{\link[spOccupancy]{spPGOcc}},
#' \code{\link[spOccupancy]{PGOcc}}, \code{\link{addVarEEimage}},
#' \code{\link{addVarEEcollection}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(dplyr)
#'
#' ## Extract ABAP pentad data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#'
#' ## Extract single season ABAP bird data
#' abap_single <- getAbapData(.spp_code = 212,
#'                            .region_type = "province",
#'                            .region = "Eastern Cape",
#'                            .years = 2012)
#'
#' ## Create spOcc list (with XY coordinates as site covariates)
#' spOcc_single <- abapToSpOcc_single(abap_single, abap_pentads)
#' str(spOcc_single)
#'
#' ## Start up GEE
#' ee_check()
#' ee_Initialize(drive = TRUE)
#'
#' ## Create assetId for pentads of interest
#' assetId <- file.path(ee_get_assethome(), 'EC_pentads')
#'
#' ## Upload to pentads to GEE (only run this once per asset)
#' uploadPentadsToEE(pentads = abap_pentads,
#'                   asset_id = assetId,
#'                   load = FALSE)
#'
#' ## Load the remote asset into R session
#' pentads <- ee$FeatureCollection(assetId)
#'
#' ## Extract spatial mean NDVI for each pentad
#' ndvi_mean <- addVarEEcollection(ee_pentads = pentads,
#'                                 collection = "MODIS/006/MOD13A2",
#'                                 dates = c("2010-01-01", "2013-01-01"),
#'                                 temp_reducer = "mean",
#'                                 spt_reducer = "mean",
#'                                 bands = "NDVI")
#'
#' ## Extract spatial standard deviation of NDVI for each pentad
#' ndvi_sd <- addVarEEcollection(ee_pentads = pentads,
#'                               collection = "MODIS/006/MOD13A2",
#'                               dates = c("2010-01-01", "2013-01-01"),
#'                               temp_reducer = "mean",
#'                               spt_reducer = "stdDev",
#'                               bands = "NDVI")
#'
#' ## Create a site covariate data frame for input into addEEtoSpOcc_single().
#' ## Note the first column is called "pentad" which is a requirement for the
#' ## function to work properly.
#' my_ee_data <- bind_cols(pentad = ndvi_mean$pentad,
#'                         ndvi_SD = ndvi_sd$NDVI_stdDev,
#'                         ndvi_MEAN = ndvi_mean$NDVI_mean)
#'
#' ## Add GEE covariates to spOcc list
#' spOcc_single_ee <- addEEtoSpOcc_single(spOcc = spOcc_single,
#'                                        ee_data = my_ee_data)
#' str(spOcc_single_ee)
#'
#'
#' ## Scale site covariates if necessary
#' spOcc_single_ee$occ.covs <- scale(spOcc_single_ee$occ.covs)
#' head(spOcc_single_ee$occ.covs)
#' summary(spOcc_single_ee$occ.covs)
#'
#'
#' ## A slightly different example:
#' ## Annotate ABAP data with multiple bands from a GEE collection
#' pentads_tc <- addVarEEcollection(ee_pentads = pentads,
#'                                  collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                  dates = c("2010-01-01", "2011-01-01"),
#'                                  temp_reducer = "mean",
#'                                  spt_reducer = "mean",
#'                                  bands = c("tmmx", "tmmn"))
#'
#' ## Select the variables to transfer to the spOcc list, making sure
#' ## 'pentad' is amongst them
#' my_ee_data <- pentads_tc %>%
#'     select(pentad, tmmx_mean, tmmn_mean)
#'
#' ## Add GEE covariates to spOcc list
#' spOcc_single_ee <- addEEtoSpOcc_single(spOcc = spOcc_single,
#'                                        ee_data = my_ee_data)
#'
#' str(spOcc_single_ee)
#'
#' }

addEEtoSpOcc_single <- function(spOcc, ee_data) {

    if(!requireNamespace("spOccupancy", quietly = TRUE)) {
        warning("Package spOccupancy doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

    if(isFALSE("pentad" %in% names(ee_data))){
        stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
    }

    spOcc_pentads <- dimnames(spOcc$y)[[1]]

    site_cov <- ee_data %>%
        as.data.frame() %>%
        dplyr::select(-dplyr::any_of("geometry")) %>%
        dplyr::filter(pentad %in% spOcc_pentads) %>%
        dplyr::arrange(match(pentad, spOcc_pentads))

    # check that pentads in spOccupancy data and covariate data are the same
    if(!identical(site_cov$pentad, spOcc_pentads)){
        stop("Pentads in spOccupancy data and covariate data don't match")
    }

    site_cov <- site_cov %>%
        dplyr::select(-pentad)

    if(is.null(spOcc$occ.covs)) {
        spOcc$occ.covs <- as.matrix(site_cov)
    } else {
        spOcc$occ.covs <- as.matrix(cbind(spOcc$occ.covs, site_cov))
    }


    return(spOcc)
}
