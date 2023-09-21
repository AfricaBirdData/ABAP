#' ABAP to occuR
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an list which can be used to fit single-species occupancy models using
#' the package \href{https://github.com/r-glennie/occuR}{occuR}. This package
#' can fit non-linear effects, including spatial, temporal and spatio-temporal effects
#' using splines.
#' @param abap_data ABAP data downloaded using \code{\link{getAbapData}}.
#' @param occasion A character string indicating the variable in `abap_data`
#' that informs about the season (or occasion, as referred to in occuR). This must always
#' be supplied, although it is really only important in the case of multi-season data.
#' It is typically "year" but could be something else.
#' @param pentads Optional, An `sf` object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#' @param proj_coords logical value indicating whether pentad coordinates are
#' projected (`TRUE`) or kept in decimal degree format (`FALSE`). Defaults to `TRUE`.
#' See details below for coordinate reference system used during transformation.
#'
#' @return A list containing data necessary for model fitting in `occuR`.
#' List elements are: visit_data, a data frame containing information about individual
#' visits; site_data, a data frame containing information about sites and seasons.
#' If `pentads` are given then the coordinates of the centroid of the pentads will be
#' included in site_data.
#'
#' @details The \href{https://github.com/r-glennie/occuR}{occuR} package can
#' fit spatial effects, for which we need the spatial location of our sites. Within the context
#' of ABAP, these locations are the centroid of each sampling pentad. In order to
#' provide these spatial data to this function, simply use \code{\link{getRegionPentads}}
#' and provide the same inputs for `.region_type` and `.region` that are specified
#' in corresponding \code{\link{getAbapData}} call. If proj_coords is set to `TRUE`
#' then the coordinates will be transformed using the African Albers Equal Area
#' coordinate system (see \href{https://spatialreference.org/ref/esri/africa-albers-equal-area-conic/}{here}
#' for details). This projection is best suited for land masses extending in an
#' east-to-west orientation at mid-latitudes making it suitable for projecting
#' pentads in Southern Africa. \cr
#'
#' In addition to reformatting the detection/non-detection ABAP data for use in
#' `occuR` occupancy models, this function also extracts two survey-level
#' covariates and adds them to the output list: ` hours` and `jday`. The `hours`
#' variable is the total number of hours spent atlassing which is recorded on the
#' pentad card and `jday` is the Julian day corresponding to the first day of
#' atlassing for that card.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ABAP)
#' library(rgee)
#' library(ABDtools)
#' library(dplyr)
#'
#' ## Download single-season ABAP data
#' abap_data <- getAbapData(.spp_code = 212,
#'                          .region_type = "province",
#'                          .region = "Eastern Cape",
#'                          .years = 2012)
#'
#' # Download ABAP site spatial data
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#'
#' # We will use years as occupancy reference seasons
#' abap_data$year <- format(abap_data$StartDate, "%Y")
#'
#' ## Return list for spatial occupancy model
#' occur_data <- abapToOccuR(abap_data, occasion = "year", abap_pentads)
#' str(occur_data)
#'
#' ## Return list for spatial occupancy model (without coordinate projection)
#' occur_data <- abapToOccuR(abap_data, "year", abap_pentads, proj_coords = FALSE)
#' str(occur_data)
#'
#' ## List for non-spatial occupancy model
#' occur_data <- abapToOccuR(abap_data, "year")
#' str(occur_data)
#'
#' ## Transform multi-season ABAP data into occuR data is just as easy
#' abap_data <- getAbapData(.spp_code = 212,
#'                          .region_type = "province",
#'                          .region = "Eastern Cape",
#'                          .years = 2012:2015)
#'
#' # We will use years as occupancy reference seasons
#' abap_data$year <- format(abap_data$StartDate, "%Y")
#'
#' ## Return list for spatial occupancy model
#' occur_data <- abapToOccuR(abap_data, occasion = "year", abap_pentads)
#' str(occur_data)
#'
#' ### ADD SITE-YEAR VARIABLES ###
#'
#' ## Start up GEE
#' ee_check()
#' ee_Initialize(drive = TRUE)
#'
#' ## Create assetId for pentads of interest
#' assetId <- file.path(ee_get_assethome(), 'EC_pentads')
#'
#' ## Upload to pentads to GEE (only run this once per asset)
#' uploadFeaturesToEE(pentads = abap_pentads,
#'                    asset_id = assetId,
#'                    load = FALSE)
#'
#' ## Load the remote asset into R session
#' pentads <- ee$FeatureCollection(assetId)
#'
#' ## Create a multi-band image with mean NDVI for each year
#' ndvi_multiband <- EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
#'                                           dates = c("2012-01-01", "2015-12-31"),
#'                                           band = "NDVI",
#'                                           group_type = "year",
#'                                           groups = 2012:2015,
#'                                           reducer = "mean",
#'                                           unmask = FALSE)
#'
#' ## Find mean and standard deviation of NDVI value for each pentad and year from the multi-band image
#' ndvi_mean <- addVarEEimage(pentads, ndvi_multiband, "mean")
#' ndvi_sd <- addVarEEimage(pentads, ndvi_multiband, "stdDev")
#'
#' ## Format the data to include the pentad column and GEE values for each year
#' ndvi_mean <- ndvi_mean %>%
#'     select(pentad, paste0("NDVI_", as.character(2012:2015)))
#'
#' ndvi_sd <- ndvi_sd %>%
#'     select(pentad, paste0("NDVI_", as.character(2012:2015)))
#'
#' ## occuR data have the structure of regular data frames (they are data tables, but
#' ## behave very similarly), so there are many ways we can transfer covariate values from
#' ## other data frame-like objects. Here, we will transfer the value of these variables
#' ## with dplyr::left_join(), but first we need to give covariates a long format with tidyr
#' ndvi_mean_long <- ndvi_mean %>%
#'     sf::st_drop_geometry() %>%
#'     tidyr::pivot_longer(-pentad, names_to = "year", values_to = "NDVI_mean") %>%
#'     mutate(year = gsub("NDVI_", "", year))
#'
#' ndvi_sd_long <- ndvi_sd %>%
#'     sf::st_drop_geometry() %>%
#'     tidyr::pivot_longer(-pentad, names_to = "year", values_to = "NDVI_sd") %>%
#'     mutate(year = gsub("NDVI_", "", year))
#'
#' # Transfer variables via join
#' occur_data_ee <- occur_data$site %>%
#'     dplyr::left_join(ndvi_mean_long, by = c("pentad", "year")) %>%
#'     dplyr::left_join(ndvi_sd_long, by = c("pentad", "year"))
#'
#' summary(occur_data_ee)
#'
#' }

abapToOccuR <- function(abap_data, occasion, pentads = NULL, proj_coords = TRUE){

    # if(!requireNamespace("occuR", quietly = TRUE)) {
    #     warning("Package occuR doesn't seem to be installed. We recommend installing it if you are using this function.")
    # }

    # Create visit data
    visit_data <- abap_data %>%
        dplyr::arrange(Pentad, StartDate) %>%
        dplyr::group_by(Pentad) %>%
        dplyr::mutate(site = dplyr::cur_group_id()) %>%
        dplyr::ungroup()

    visit_data <- visit_data %>%
        dplyr::group_by(!!rlang::sym(occasion)) %>%
        dplyr::group_by(occasion = dplyr::cur_group_id()) %>%
        dplyr::ungroup()

    visit_data <- visit_data %>%
        dplyr::group_by(site) %>%
        dplyr::mutate(visit = dplyr::row_number()) %>%
        dplyr::ungroup()

    visit_data <- visit_data %>%
        dplyr::mutate(obs = ifelse(Spp == "-", 0L, 1L))


    # Create additional covariates (total observation hours and day of year)
    visit_data <- visit_data %>%
        dplyr::rename(hours = TotalHours) %>%
        dplyr::mutate(jday = lubridate::yday(StartDate))

    # Select columns
    visit_data <- visit_data %>%
        dplyr::select(pentad = Pentad, !!rlang::sym(occasion), site, occasion, visit, obs, hours, jday)

    # Create site data
    site_data <- visit_data %>%
        dplyr::distinct(pentad, year, site, occasion)

    # Extract spatial data
    if(!is.null(pentads)){

        pentad_id <- unique(abap_data$Pentad)

        sf::st_agr(pentads) = "constant"
        aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

        if(isTRUE(proj_coords)){
            pentads <- sf::st_transform(pentads, aeaproj)
        }

        ids <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            dplyr::arrange(match(pentad, site_data$pentad)) %>%
            dplyr::pull(pentad)

        pentad_xy <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            dplyr::arrange(match(pentad, site_data$pentad)) %>%
            sf::st_centroid() %>%
            sf::st_coordinates() %>%
            as.data.frame() %>%
            dplyr::mutate(pentad = ids)

        site_data <- site_data %>%
            dplyr::left_join(pentad_xy, by = "pentad")

    }

    return(list(site = data.table::as.data.table(site_data),
                visit = data.table::as.data.table(visit_data)))

}
