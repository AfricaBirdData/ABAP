#' ABAP to occuR
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an list which can be used to fit single-species occupancy models using
#' the package \code{\href{https://github.com/r-glennie/occuR}{occuR}}. This package
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
#' @details The \code{\href{https://github.com/r-glennie/occuR}{occuR}} package can
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
#' abap_data <- getAbapData(.spp_code = 212,
#'                          .region_type = "province",
#'                          .region = "Eastern Cape",
#'                          .years = 2012)
#'
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
abapToOccuR <- function(abap_data, occasion, pentads = NULL, proj_coords = TRUE){

    if(!requireNamespace("occuR", quietly = TRUE)) {
        warning("Package occuR doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

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
        dplyr::mutate(obs = ifelse(Spp == "-", 0, 1))


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

        pentad_xy <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            dplyr::arrange(match(pentad, site_data$pentad)) %>%
            sf::st_centroid() %>%
            sf::st_coordinates()

        site_data <- site_data %>%
            dplyr::mutate(X = pentad_xy[,1],
                          Y = pentad_xy[,2])

    }

    return(list(site = data.table::as.data.table(site_data),
                visit = data.table::as.data.table(visit_data)))

}
