#' Add Google Earth Engine covariate data to a multi-season spOccupancy data list
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted
#' to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}}
#' functions. `addEEtoSpOcc_multi` can then be used to add these types of data
#' to a list in order to fit single-species multi-season occupancy models using
#' \code{\link[spOccupancy]{stPGOcc}} (spatial) or \code{\link[spOccupancy]{tPGOcc}}
#' (non-spatial) from the `spOccupancy` package.
#'
#' @param spOcc a list containing ABAP detection/non-detection data returned by
#' \code{\link{abapToSpOcc_multi}}.
#' @param ee_data a data frame with GEE data extracted using \code{\link{addVarEEcollection}}
#' or \code{\link{addVarEEimage}}. The data frame needs to contain a column called
#' `pentads` with the pentad ID from data extracted using the GEE functions.
#' The remaining columns should only contain the GEE covariate values that are
#' intended to be added to the `spOccupancy` list. For ease of use in occupancy
#' model formula notation it's recommended that the variable names in the data
#' frame are concise and informative, and don't contain spaces. See the example
#' below for how to create this data frame after extracting GEE data.
#' @param type either 'occ' or 'det' specifying whether the covariate to process
#' is affecting occupancy or detection, respectively.
#' @param seasons string indicating the name of the variable in `ee_data` that should
#' be used to define seasons in multi-season models.
#'
#' @note The numeric ranges of various GEE data can be vastly different so it
#' is advised that you scale your covariate data before running an occupancy
#' model. See the example below for how to do this.
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @seealso \code{\link{abapToSpOcc_multi}}, \code{\link[spOccupancy]{stPGOcc}},
#' \code{\link[spOccupancy]{tPGOcc}}, \code{\link{addVarEEimage}},
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
#'                                 .region = "Eastern Cape")
#'
#' ## Extract single season ABAP bird data
#' abap_single <- getAbapData(.spp_code = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = 2012)
#'
#' ## Create spOcc list (with X & Y co-ordinates as site covariates)
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
#' ## Create a site covariate data frame for input into addEEtoSpOcc_single().
#' ## Note the first column is called "pentad" which is a requirement for the
#' ## function to work properly.
#' my_ee_data <- bind_cols(pentad = ndvi_mean$pentad,
#'                      ndvi_SD = ndvi_sd$NDVI_stdDev,
#'                      ndvi_MEAN = ndvi_mean$NDVI_mean,
#'                      temp_MIN = lst_min$LST_Day_1km_min,
#'                      temp_MAX = lst_max$LST_Day_1km_max)
#'
#' ## Add GEE covariates to spOcc list
#' spOcc_single_ee <- addEEtoSpOcc_single(spOcc = spOcc_single,
#'                                      ee_data = my_ee_data)
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
#'                                      ee_data = my_ee_data)
#'
#' str(spOcc_single_ee)
#'
#' }

addEEtoSpOcc_multi <- function(spOcc, ee_data, type = c("occ", "det"), seasons = NULL) {

    if(!requireNamespace("spOccupancy", quietly = TRUE)) {
        warning("Package spOccupancy doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

    if(isFALSE("pentad" %in% names(ee_data))){
        stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
    }

    if(is.null(seasons)){
        stop("No seasons have been specified, for single-season model use addEEtoSpOcc_single()")
    }

    if(is.null(type)){
        type <- "occ"
    } else if(!type %in% c("occ", "det")){
        stop("Covariate type must be either 'occ' or 'det'")
    }

    spOcc_pentads <- dimnames(spOcc$y)[[1]]

    site_cov <- ee_data %>%
        as.data.frame() %>%
        dplyr::select(-dplyr::any_of("geometry")) %>%
        dplyr::filter(pentad %in% spOcc_pentads) %>%
        dplyr::mutate(season = as.character(!!as.name(seasons))) %>%
        dplyr::arrange(match(pentad, spOcc_pentads), seasons)

    # check that pentads in spOccupancy data and covariate data are the same
    if(!identical(unique(site_cov$pentad), spOcc_pentads)){
        stop("Pentads in spOccupancy data and covariate data don't match")
    }


    # Number of covariates (all columns, minus the pentad and season columns)
    n_covts <- ncol(site_cov) - 2

    # Covariate names
    covt_vec <- names(site_cov)
    covt_vec <- covt_vec[!covt_vec %in% c("pentad", "season")]

    # Create a complete data set for pentad and seasons
    full_cov <- expand.grid(pentad = spOcc_pentads, season = dimnames(spOcc$y)[[2]],
                            stringsAsFactors = FALSE) %>%
        dplyr::arrange(match(pentad, spOcc_pentads), season)

    full_cov <- full_cov %>%
        dplyr::left_join(site_cov, by = c("pentad", "season"))

    # Prepare spOccupancy covariates
    out <- vector("list", length = n_covts)

    for(k in seq_along(covt_vec)){

        covt_sel <- covt_vec[[k]]

        covt <- full_cov %>%
            dplyr::select(pentad, season, dplyr::all_of(covt_sel)) %>%
            tidyr::pivot_wider(names_from = season,
                               values_from = dplyr::all_of(covt_sel))

        # Save pentads for dimnames
        pt <- covt$pentad

        out[[k]] <- covt %>%
            dplyr::select(-pentad) %>%
            as.matrix()

        # Fix names
        dimnames(out[[k]])[[1]] <- pt
        names(out)[k] <- covt_sel

    }


    if(type == "occ"){
        if(is.null(spOcc$occ.covs)) {
            spOcc$occ.covs <- out
        } else {
            spOcc$occ.covs <- c(spOcc$occ.covs, out)
        }
    } else {
        if(is.null(spOcc$det.covs)) {
            spOcc$det.covs <- out
        } else {
            spOcc$det.covs <- c(spOcc$det.covs, out)
        }
    }

    return(spOcc)
}
