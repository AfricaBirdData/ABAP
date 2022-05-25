#' Add Google Earth Engine covariate data to a multi-season Unmarked frame
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}} functions. `addEEtoUnmarked_multi` can then be used to add these types of data to a multi-season Unmarked frame. In multi-season (dynamic) occupancy models, the GEE data can either be used as covariates that vary at the site (`siteCovs`) level or covariates that vary at the site-year level (`yearlySiteCovs`). More information on this data structure can be found by looking at \code{\link[unmarked]{unmarkedMultFrame}}.
#'
#' @param umf a multi-season Unmarked frame containing ABAP detection/non-detection data returned by \code{\link{abapToUnmarked_multi}}.
#' @param ee_data a data frame with GEE data extracted using \code{\link{addVarEEcollection}} or \code{\link{addVarEEimage}}.
#' @param ee_name a user-defined character string giving the desired name of the GEE site or site-year covariate.
#' @param ee_assign "site" if the covariate only varies between sites or "site-year" if the covariate varies across sites and between years.
#'
#' @return an object of class \code{\link[unmarked]{unmarkedMultFrame}} with survey, site, and site-year covariates.
#'
#' @note The numeric ranges of various GEE data can be vastly different so it is advised that you scale your covariate data before running an occupancy model. This can be done within the formula notation of the \code{\link[unmarked]{colext}} function. Note that `year` is automatically added as a site-year covariate in order to model changes in colonisation/extinction purely as a function of year (in terms of notation, the first year is coded as `01`, the second year as `02`, etc).
#'
#' @seealso \code{\link{abapToUnmarked_multi}}, \code{\link{addVarEEimage}}, \code{\link{addVarEEcollection}}
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
#'
#' ## Extract ABAP pentad data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                 .region = "Eastern Cape")
#'
#' ## Extract single season ABAP bird data
#' abap_multi <- getAbapData(.spp = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = c(2009,2010,2011,2012))
#'
#' ## Create unmarked frame
#' umf_multi <- abapToUnmarked_multi(abap_multi)
#' summary(umf_multi)
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
#' ## Create a multi-band image with mean NDVI for each year
#' ndvi_multiband <- EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
#'                                           dates = c("2009-01-01", "2012-12-31"),
#'                                           band = "NDVI",
#'                                           group_type = "year",
#'                                           groups = 2009:2012,
#'                                           reducer = "mean",
#'                                           unmask = FALSE)
#'
#' ## Extract mean and max NDVI for each pentad and year
#' ndvi_multi_mean <- addVarEEimage(pentads, ndvi_multiband, "mean")
#' ndvi_multi_max <- addVarEEimage(pentads, ndvi_multiband, "max")
#'
#' ## Extract mean estimate of surface water occurrence for each pentad (doesn't vary by year)
#' water_mean <- addVarEEimage(ee_pentads = pentads,
#'                            image = "JRC/GSW1_3/GlobalSurfaceWater",
#'                            reducer = "mean",
#'                            bands = "occurrence")
#'
#' ## Add to NDVI covariate to multi-season Unmarked frame
#' umf_multi_ee <- addEEtoUnmarked_multi(umf = umf_multi,
#'                                        ee_data = ndvi_multi_mean,
#'                                        ee_name = "NDVI_mean",
#'                                        ee_assign = "site-year")
#' summary(umf_multi_ee)
#'
#' ## Add surface water covariate to unmarked frame
#' umf_multi_ee <- addEEtoUnmarked_multi(umf = umf_multi,
#'                                        ee_data = water_mean,
#'                                        ee_name = "water_mean",
#'                                        ee_assign = "site")
#' summary(umf_multi_ee)
#'
#' ## Add all covariates simultaneously using the pipe operator
#' umf_multi_ee <- umf_multi %>%
#'     addEEtoUnmarked_multi(., water_mean, "water_mean", "site") %>%
#'     addEEtoUnmarked_multi(., ndvi_multi_mean, "ndvi_mean", "site-year") %>%
#'     addEEtoUnmarked_multi(., ndvi_multi_max, "ndvi_max", "site-year")
#'
#' summary(umf_multi_ee)
#' }
addEEtoUnmarked_multi <- function(umf, ee_data, ee_name, ee_assign){

    if(ee_assign == "site"){

        reducer_names <- c("mean","max", "min", "count", "median",
                           "sum", "stdDev", "variance")

        builtin_names <- c("id","Name","country", "full_qdgc",
                           "pentad","province","qdgc", "geometry")

        cols_keep <- reducer_names[reducer_names %in% names(ee_data)]

        if(length(cols_keep) == 0){

            cols_keep <- names(ee_data)[!names(ee_data) %in% builtin_names]

        }

        umf_pentads <- dimnames(umf@y)[[1]]

        site_cov <- ee_data %>%
            sf::st_drop_geometry() %>%
            dplyr::filter(pentad %in% umf_pentads) %>%
            dplyr::arrange(match(pentad, umf_pentads)) %>%
            dplyr::select(dplyr::all_of(cols_keep)) %>%
            dplyr::rename_with(.cols = 1, ~{{ee_name}})

        if(is.null(umf@siteCovs)){

            umf@siteCovs <- site_cov

        } else {

            umf@siteCovs <- dplyr::bind_cols(umf@siteCovs,site_cov)
        }

        return(umf)

    } else if (ee_assign == "site-year"){

        umf_pentads <- dimnames(umf@y)[[1]]

        year_cov <- ee_data %>%
            sf::st_drop_geometry() %>%
            dplyr::filter(pentad %in% umf_pentads) %>%
            dplyr::arrange(match(pentad, umf_pentads)) %>%
            dplyr::select(dplyr::starts_with("X"))

        umf@yearlySiteCovs$new_year_cov <- as.vector(t(as.matrix(year_cov)))
        name_ref <- length(names(umf@yearlySiteCovs))
        names(umf@yearlySiteCovs)[[name_ref]] <- ee_name

        return(umf)

    }

}