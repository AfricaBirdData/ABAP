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
#' @param ee_assign  "site" if the covariate only varies between sites or "site-season"
#' if the covariate varies across sites and between seasons.
#' @param process either 'occ' or 'det' specifying whether the covariate
#' is affecting occupancy or detection processes, respectively.
#' @param seasons string indicating the name of the variable in `ee_data` that should
#' be used to define seasons in multi-season models.
#'
#' @details If ee_assign = "site-year" ee_data must be a data frame-like object (sf could work)
#' with each covariate in one column, the pentad identifier in another column named `pentad`,
#' and the season identifier in another column (with the same name specified in \code{season}).
#' if ee_assign = "site" ee_data must be a data frame-like object
#' with each covariate in one column, and the pentad identifier in another column named 'pentad'.
#' @note The numeric ranges of various GEE data can be vastly different so it
#' is advised that you scale your covariate data before running an occupancy
#' model.
#' @returns The data list \code{spOcc} with the additional covariates.
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @seealso \code{\link{abapToSpOcc_multi}}, \code{\link[spOccupancy]{stPGOcc}},
#' \code{\link[spOccupancy]{tPGOcc}}, \code{\link[ABDtools]{addVarEEimage}},
#' \code{\link[ABDtools]{addVarEEcollection}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'  ## rgee and ABDtools are required to annotate data with Google Earth Engine
#' library(rgee)
#' library(ABDtools)
#' library(dplyr)
#'
#' ## Extract ABAP pentad data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#'
#' ## Extract multi-season ABAP bird data
#' abap_multi <- getAbapData(.spp = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = c(2009,2010,2011,2012))
#'
#' ## We will use years as occupancy reference seasons
#' abap_multi$year <- format(abap_multi$StartDate, "%Y")
#'
#' ## Create unmarked frame (with X & Y coords as site covariates)
#' spOcc_multi <- abapToSpOcc_multi(abap_multi, abap_pentads, seasons = "year")
#' str(spOcc_multi)
#'
#' ## Start up GEE
#' ee_check()
#' ee_Initialize(drive = TRUE)
#'
#' ## Create assetId for pentads of interest
#' assetId <- file.path(ee_get_assethome(), 'EC_pentads')
#'
#' ## Upload to pentads to GEE (only run this once per asset) and
#' ## load the remote asset into R session
#' pentads <- uploadFeaturesToEE(feats = abap_pentads,
#'                               asset_id = assetId,
#'                               load = TRUE)
#'
#' ### ADD SITE-YEAR VARIABLES ###
#'
#' ## Create a multi-band image with mean NDVI for each year
#' ndvi_multiband <- EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
#'                                           dates = c("2009-01-01", "2012-12-31"),
#'                                           band = "NDVI",
#'                                           group_type = "year",
#'                                           groups = 2009:2012,
#'                                           reducer = "mean",
#'                                           unmask = FALSE)
#'
#' ## Find mean and sd NDVI value for each pentad and year from the multi-band image
#' ndvi_mean <- addVarEEimage(pentads, ndvi_multiband, "mean")
#' ndvi_sd <- addVarEEimage(pentads, ndvi_multiband, "stdDev")
#'
#' ## Format the data to include the pentad column and GEE values for each year
#' ndvi_mean <- ndvi_mean %>%
#'     select(pentad, as.character(2009:2012))
#'
#' ndvi_sd <- ndvi_sd %>%
#'     select(pentad, as.character(2009:2012))
#'
#' ## Create a data frame with covariate columns, pentad and season.
#' ee_siteyear <- ndvi_mean %>%
#'     as.data.frame() %>%
#'     dplyr::select(-geometry) %>%
#'     tidyr::pivot_longer(-pentad, names_to = "year", values_to = "ndvi_mean")
#'
#' ## Add GEE site-year data to spOccupancy data list. We will add these covariates
#' ## to the detection process.
#' spOcc_multi_ee1 <- addEEtoSpOcc_multi(spOcc_multi,
#'                                       ee_data = ee_siteyear,
#'                                       ee_assign = "site-season",
#'                                       process = "det",
#'                                       seasons = "year")
#'
#' str(spOcc_multi_ee1)
#'
#' ### ADD SITE VARIABLES ###
#'
#' ## Annotate data with mean elevation per pentad
#' elev_mean <- addVarEEimage(ee_feats = pentads,
#'                            image = "MERIT/DEM/v1_0_3",
#'                            reducer = "mean",
#'                            bands = "dem",
#'                            unmask = FALSE)
#'
#' ## Keep only the columns that we need
#' ee_site <- elev_mean %>%
#'     as.data.frame() %>%
#'     dplyr::select(pentad, dem_mean)
#'
#' ## Add GEE site data to spOccupancy data list. We will now add it to the
#' ## occupancy process.
#' spOcc_multi_ee2 <- addEEtoSpOcc_multi(spOcc_multi,
#'                                       ee_data = ee_site,
#'                                       ee_assign = "site",
#'                                       process = "occ")
#' }

addEEtoSpOcc_multi <- function(spOcc, ee_data, ee_assign, process = c("occ", "det"), seasons = NULL) {

    # For compatibility
    if(ee_assign == "site-year") ee_assign = "site_season"

    if(!requireNamespace("spOccupancy", quietly = TRUE)) {
        warning("Package spOccupancy doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

    if(isFALSE("pentad" %in% names(ee_data))){
        stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
    }

    if(is.null(seasons) && ee_assign == "site-season"){
        stop('No seasons have been specified, if covariates only change by site set ee_assign = "site"')
    }

    if(!is.null(seasons) && ee_assign == "site"){
        stop('Seasons have been specified, if covariates change by site and season set ee_assign = "site-season",
             otherwise do not specify any seasons')
    }

    if(is.null(process)){
        process <- "occ"
    } else if(!process %in% c("occ", "det")){
        stop("Covariate 'process' must be either 'occ' or 'det'")
    }

    spOcc_pentads <- dimnames(spOcc$y)[[1]]

    site_cov <- ee_data %>%
        as.data.frame() %>%
        dplyr::select(-dplyr::any_of("geometry")) %>%
        dplyr::filter(pentad %in% spOcc_pentads)

    if(is.null(seasons)){
        site_cov <- site_cov %>%
            dplyr::arrange(match(pentad, spOcc_pentads))
    } else {
        site_cov <- site_cov %>%
            dplyr::mutate(season = as.character(!!as.name(seasons))) %>%
            dplyr::arrange(match(pentad, spOcc_pentads), seasons)
    }

    # check that pentads in spOccupancy data and covariate data are the same
    if(!identical(unique(site_cov$pentad), spOcc_pentads)){
        stop("Pentads in spOccupancy data and covariate data don't match")
    }

    # Number of covariates (all columns, minus the pentad and season columns)
    n_covts <- ncol(site_cov) - (2 - is.null(seasons))

    # Covariate names
    covt_vec <- names(site_cov)
    covt_vec <- covt_vec[!covt_vec %in% c("pentad", "season")]

    # Create a complete data set for pentad and seasons (if specified)
    if(is.null(seasons)){
        full_cov <- data.frame(pentad = spOcc_pentads) %>%
            dplyr::arrange(match(pentad, spOcc_pentads))

        full_cov <- full_cov %>%
            dplyr::left_join(site_cov, by = c("pentad"))
    } else {
        full_cov <- expand.grid(pentad = spOcc_pentads, season = dimnames(spOcc$y)[[2]],
                                stringsAsFactors = FALSE) %>%
            dplyr::arrange(match(pentad, spOcc_pentads), season)

        full_cov <- full_cov %>%
            dplyr::left_join(site_cov, by = c("pentad", "season"))
    }

    # Prepare spOccupancy covariates
    if(ee_assign == "site-season"){

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

    } else if(ee_assign == "site"){

        out <- vector("list", length = n_covts)

        # Save pentads for names
        pt <- full_cov$pentad

        for(k in seq_along(covt_vec)){

            covt_sel <- covt_vec[[k]]

            out[[k]] <- full_cov %>%
                dplyr::pull(dplyr::all_of(covt_sel))

            # Fix names
            names(out[[k]]) <- pt
            names(out)[k] <- covt_sel

        }

    }


    if(process == "occ"){
        if(is.null(spOcc$occ.covs)) {
            spOcc$occ.covs <- out
        } else {
            spOcc$occ.covs <- c(spOcc$occ.covs, out)
        }
    } else if(process == "det"){
        if(is.null(spOcc$det.covs)) {
            spOcc$det.covs <- out
        } else {
            spOcc$det.covs <- c(spOcc$det.covs, out)
        }
    }

    return(spOcc)
}
