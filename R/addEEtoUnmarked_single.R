#' Add Google Earth Engine covariate data to a single-season Unmarked frame
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}} functions. `addEEtoUnmarked_single` can then be used to add these types of data to a single-season Unmarked frame. GEE data can subsquently be used as covariates in single-season occupancy models such as \code{\link[unmarked]{occu}}, \code{\link[unmarked]{occuRN}}, \code{\link[ubms]{stan_occu}} or \code{\link[ubms]{stan_occuRN}}.
#'
#' @param umf a single-season Unmarked frame containing ABAP detection/non-detection data returned by \code{\link{abapToUnmarked_single}}.
#' @param ee_data a data frame with GEE data extracted using \code{\link{addVarEEcollection}} or \code{\link{addVarEEimage}}. The data frame needs to contain a column called `pentads` with the pentad ID from data extracted using the GEE functions. The remaining columns should only contain the GEE covariate values that are intended to be added to the `unmarked` frame. For ease of use in occupancy model formula notation it's recommended that the variable names in the data frame are concise and informative, and don't contain spaces. See the example below for how to create this data frame after extracting GEE data.
#'
#' @return an object of class \code{\link[unmarked]{unmarkedFrameOccu}} with survey and site covariates.
#'
#' @note The numeric ranges of various GEE data can be vastly different so it is advised that you scale your covariate data before running an occupancy model. This can be done within the formula notation of the various \code{\link[unmarked]{occu}} functions.
#'
#' @seealso \code{\link{abapToUnmarked_single}}, \code{\link{addVarEEimage}}, \code{\link{addVarEEcollection}}
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(rgee)
#' library(unmarked)
#' library(dplyr)
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
#' ## Create unmarked frame (with X & Y coords as site covariates)
#' umf_single <- abapToUnmarked_single(abap_single, abap_pentads)
#' summary(umf_single)
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
#' ## Extract spatial mean NDVI for each pentad
#' ndvi_mean <- addVarEEcollection(ee_pentads = pentads,
#'                                   collection = "MODIS/006/MOD13A2",
#'                                   dates = c("2010-01-01", "2013-01-01"),
#'                                   temp_reducer = "mean",
#'                                   spt_reducer = "mean",
#'                                   bands = "NDVI")
#'
#' ## Extract spatial standard deviation of NDVI for each pentad
#' ndvi_sd <- addVarEEcollection(ee_pentads = pentads,
#'                                   collection = "MODIS/006/MOD13A2",
#'                                   dates = c("2010-01-01", "2013-01-01"),
#'                                   temp_reducer = "mean",
#'                                   spt_reducer = "stdDev",
#'                                   bands = "NDVI")
#'
#' ## Extract spatial minimum land surface temperature for each pentad
#' lst_min <- addVarEEcollection(ee_pentads = pentads,
#'                                  collection = "MODIS/061/MOD11A1",
#'                                  dates = c("2010-01-01", "2011-01-01"),
#'                                  temp_reducer = "mean",
#'                                  spt_reducer = "min",
#'                                  bands = "LST_Day_1km")
#'
#' ## Extract spatial maximum land surface temperature for each pentad
#' lst_max <- addVarEEcollection(ee_pentads = pentads,
#'                                  collection = "MODIS/061/MOD11A1",
#'                                  dates = c("2010-01-01", "2011-01-01"),
#'                                  temp_reducer = "mean",
#'                                  spt_reducer = "max",
#'                                  bands = "LST_Day_1km")
#'
#' ## Create a site covariate data frame for input into addEEtoUnmarked_single().
#' ## Note the first column is called "pentad" which is a requirement for the function to work properly.
#' my_ee_data <- bind_cols(pentad = ndvi_mean$pentad,
#'                      ndvi_SD = ndvi_sd$NDVI_stdDev,
#'                      ndvi_MEAN = ndvi_mean$NDVI_mean,
#'                      temp_MIN = lst_min$LST_Day_1km_min,
#'                      temp_MAX = lst_max$LST_Day_1km_max)
#'
#' ## Add GEE covariates to unmarked frame
#' umf_single_ee <- addEEtoUnmarked_single(umf = umf_single,
#'                                        ee_data = my_ee_data)
#' summary(umf_single_ee)
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
#' ## Select the variables to transfer to the unmarked frame, making sure
#' ## 'pentad' is amongst them
#' my_ee_data <- pentads_tc %>%
#'     select(pentad, tmmx_mean, tmmn_mean)
#'
#' ## Add GEE covariates to unmarked frame
#' umf_single_ee <- addEEtoUnmarked_single(umf = umf_single,
#'                                         ee_data = my_ee_data)
#'
#' summary(umf_single_ee)
#'
#' }

addEEtoUnmarked_single <- function(umf, ee_data) {

    if(isFALSE("pentad" %in% names(ee_data))){
        stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
    }

    umf_pentads <- dimnames(umf@y)[[1]]

    site_cov <- ee_data %>%
        as.data.frame() %>%
        dplyr::select(-any_of("geometry")) %>%
        dplyr::filter(pentad %in% umf_pentads) %>%
        dplyr::arrange(match(pentad, umf_pentads)) %>%
        dplyr::select(-pentad)

    if (is.null(umf@siteCovs)) {
        umf@siteCovs <- site_cov
    } else {
        umf@siteCovs <- dplyr::bind_cols(umf@siteCovs, site_cov)
    }

    return(umf)
}
