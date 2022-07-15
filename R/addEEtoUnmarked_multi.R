#' Add Google Earth Engine covariate data to a multi-season Unmarked frame
#'
#' @description Google Earth Engine (GEE) data for each pentad can be extracted to a data frame using ABAP's \code{\link{addVarEEcollection}} and \code{\link{addVarEEimage}} functions. `addEEtoUnmarked_multi` can then be used to add these types of data to a multi-season Unmarked frame. In multi-season (dynamic) occupancy models, the GEE data can either be used as covariates that vary at the site (`siteCovs`) level or covariates that vary at the site-year level (`yearlySiteCovs`). More information on this data structure can be found by looking at \code{\link[unmarked]{unmarkedMultFrame}}.
#'
#' @param umf a multi-season `unmarked` frame containing ABAP detection/non-detection data returned by \code{\link{abapToUnmarked_multi}}.
#' @param ee_assign  "site" if the covariate only varies between sites or "site-year" if the covariate varies across sites and between years.
#' @param ee_data if `ee_assign` = "site" then a user-formatted data frame with GEE data extracted using \code{\link{addVarEEcollection}} or \code{\link{addVarEEimage}}. If `ee_assign` = "site-year" then the input is user-formatted list where each element contains a GEE variable that varies across years. See details below for the formatting requirements of `ee_data`.
#'
#' @details The data frame used when `ee_assign` = "site" needs to contain a column called `pentads` with the pentad ID from data extracted using the GEE functions. The remaining columns should only contain the GEE covariate values that are intended to be added to the `unmarked` frame. For ease of use in occupancy model formula notation it's recommended that the variable names in the data frame are concise and informative, and don't contain spaces.  \cr
#'
#' Each element in the list used when `ee_assign` = "site-year" is a data frame that needs to have a column called `pentads` with the pentad ID followed by a GEE variable column for each year (primary survey period) defined in the \code{\link{abapToUnmarked_multi}} call. \cr
#'
#'See the examples below for how to create these  data frame and list `ee_data` objects after extracting GEE data.
#'
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
#'
#' library(rgee)
#' library(unmarked)
#' library(dplyr)
#'
#' ## Extract ABAP pentad data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                 .region = "Eastern Cape")
#'
#' ## Extract multi-season ABAP bird data
#' abap_multi <- getAbapData(.spp = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = c(2009,2010,2011,2012))
#'
#' ## Create unmarked frame (with X & Y coords as site covariates)
#' umf_multi <- abapToUnmarked_multi(abap_multi, abap_pentads)
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
#' ## Find mean and max NDVI value for each pentad and year from the multi-band image
#' ndvi_mean <- addVarEEimage(pentads, ndvi_multiband, "mean")
#' ndvi_max <- addVarEEimage(pentads, ndvi_multiband, "max")
#'
#' ## Format the data to include the pentad column and GEE values for each year
#' ndvi_max <- ndvi_max %>%
#'    select(pentad, X2009:X2012)
#'
#' ndvi_min <- ndvi_mean %>%
#'   select(pentad, X2009:X2012)
#'
#' ## Create a list where each named element is a multi-season GEE variable which can then be used in addEEtoUnmarked_multi()
#' ee_siteyear <- list(ndvi_MAX = ndvi_max,
#'                     ndvi_MIN = ndvi_min)
#'
#' ## Add GEE site-year data to unmarked frame
#' umf_multi_ee1 <- addEEtoUnmarked_multi(umf_multi,
#'                                        ee_assign = "site-year",
#'                                        ee_siteyear)
#'
#' summary(umf_multi_ee1)
#'
#' ### ADD SITE VARIABLES ###
#'
#' ## Extract spatial minimum land surface temperature for each pentad
#' lst_min <- addVarEEcollection(ee_pentads = pentads,
#'                                  collection = "MODIS/061/MOD11A1",
#'                                  dates = c("2010-01-01", "2012-12-31"),
#'                                  temp_reducer = "mean",
#'                                  spt_reducer = "min",
#'                                  bands = "LST_Day_1km")
#'
#' ## Extract spatial maximum land surface temperature for each pentad
#' lst_max <- addVarEEcollection(ee_pentads = pentads,
#'                                  collection = "MODIS/061/MOD11A1",
#'                                  dates = c("2010-01-01", "2012-12-31"),
#'                                  temp_reducer = "mean",
#'                                  spt_reducer = "max",
#'                                  bands = "LST_Day_1km")
#'
#' ## Create a site covariate data frame for input into addEEtoUnmarked_multi().
#' ## Again, note the first column is called "pentad" which is a requirement for the function to work properly.
#' ee_site <- bind_cols(pentad = lst_min$pentad,
#'                      temp_MIN = lst_min$LST_Day_1km_min,
#'                      temp_MAX = lst_max$LST_Day_1km_max)
#'
#' ## Add GEE site data to unmarked frame
#' umf_multi_ee2 <- addEEtoUnmarked_multi(umf_multi,
#'                                        ee_assign = "site",
#'                                        ee_site)
#'
#' summary(umf_multi_ee2)
#'
#' ## Add GEE site-year data to the unmarked frame that already has site data (using addEEtoUnmarked_multi, site-year or site only data can be added sequentially).
#' umf_multi_ee3 <- addEEtoUnmarked_multi(umf_multi_ee2,
#'                                        ee_assign = "site-year",
#'                                        ee_siteyear)
#'
#' summary(umf_multi_ee3)
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
#' ee_site_tc <- pentads_tc %>%
#'     select(pentad, tmmx_mean, tmmn_mean)
#'
#' ## Add GEE site data to unmarked frame
#' umf_multi_ee4 <- addEEtoUnmarked_multi(umf_multi,
#'                                        ee_assign = "site",
#'                                        ee_site_tc)
#'
#' summary(umf_multi_ee4)
#'
#'  }
addEEtoUnmarked_multi <- function(umf, ee_assign, ee_data) {

    umf_pentads <- dimnames(umf@y)[[1]]

    if (ee_assign == "site-year") {
        for (i in seq_along(ee_data)) {
            if (isFALSE("pentad" %in% names(ee_data[[i]]))) {
                stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
            }

            attr(ee_data[[i]], "metadata") <- NULL

            year_cov <- ee_data[[i]] %>%
                as.data.frame() %>%
                dplyr::select(-any_of("geometry")) %>%
                dplyr::filter(pentad %in% umf_pentads) %>%
                dplyr::arrange(match(pentad, umf_pentads)) %>%
                dplyr::select(-pentad)

            umf@yearlySiteCovs$new_year_cov <- as.vector(t(as.matrix(year_cov)))
            name_ref <- length(names(umf@yearlySiteCovs))
            names(umf@yearlySiteCovs)[[name_ref]] <- names(ee_data)[[i]]
        }
    } else if (ee_assign == "site") {
        if (isFALSE("pentad" %in% names(ee_data))) {
            stop("ee_data does not contain the required 'pentad' variable. See function documentation.")
        }

        attr(ee_data, "metadata") <- NULL

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
    }


    return(umf)
}
