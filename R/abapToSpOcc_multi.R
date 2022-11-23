#' ABAP to spOccupancy (multi-season)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an list which can be used to fit single-species occupancy models using
#' \code{\link[spOccupancy]{spPGOcc}} (spatial) or \code{\link[spOccupancy]{PGOcc}}
#' (non-spatial). The `spOccupancy` package fits single-species, multi-species,
#' and integrated non-spatial and spatial occupancy models using Markov Chain Monte Carlo (MCMC).
#'
#' @param abap_data multi-season ABAP data downloaded using \code{\link{getAbapData}}.
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#' @param proj_coords logical value indicating whether pentad coordinates are
#' projected (`TRUE`) or kept in decimal degree format (`FALSE`). Defaults to `TRUE`.
#' See details below for coordinate reference system used during transformation.
#' @param seasons string indicating the name of the variable in `abap_data` that should
#' be used to define seasons in multi-season models.
#'
#' @return A list containing data necessary for model fitting in `spOccupancy`.
#' List elements are `y` (detection/non-detection data), `det.covs` (survey-level covariates),
#' and `coords` (x and y centroids of pentads if spatial data are supplied).
#'
#' @details The \code{\link[spOccupancy]{spPGOcc}} function takes `coords` as an
#' argument with X and Y coordinates included for each site. Within the context
#' of ABAP, these `coords` are the centroid of each sampling pentad. In order to
#' provide these spatial data to this function, simply use \code{\link{getRegionPentads}}
#' and provide the same inputs for `.region_type` and `.region` that are specified
#' in corresponding \code{\link{getAbapData}} call. If proj_coords is set to `TRUE`
#' then the coordinates will be transformed using the African Albers Equal Area
#' coordinate system (see \href{https://spatialreference.org/ref/esri/africa-albers-equal-area-conic/}{here}
#' for details). This projection is best suited for land masses extending in an
#' east-to-west orientation at mid-latitudes making it suitable for projecting
#' pentads in Southern Africa. The functions in the `spOccupancy` package assume
#' that coordinates are projected so for best results it is recommended to always
#' project the data. \cr
#'
#' If `pentads` are specified then the output can be automatically used in
#' \code{\link[spOccupancy]{stPGOcc}}. If no spatial data are provided the output
#' can be used in \code{\link[spOccupancy]{tPGOcc}}.\cr
#'
#' In addition to reformatting the detection/non-detection ABAP data for use in
#' `spOccupancy` occupancy models, this function also extracts two survey-level
#' covariates and adds them to the output list: ` hours` and `jday`. The `hours`
#' variable is the total number of hours spent atlassing which is recorded on the
#' pentad card and `jday` is the Julian day corresponding to the first day of
#' atlassing for that card.
#'
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#' @seealso \code{\link[spOccupancy]{stPGOcc}}, \code{\link[spOccupancy]{tPGOcc}}
#' @export
#'
#' @examples
#' abap_data <- getAbapData(.spp_code = 212,
#'                          .region_type = "province",
#'                          .region = "Eastern Cape",
#'                          .years = 2012:2014)
#'
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#' # We will use years as occupancy reference seasons
#' abap_data$year <- format(abap_data$StartDate, "%Y")
#'
#' ## Return list for spatial occupancy model
#' stOcc <- abapToSpOcc_multi(abap_data, abap_pentads, seasons = "year")
#' str(stOcc)
#'
#' ## Return list for spatial occupancy model (without coordinate projection)
#' stOcc <- abapToSpOcc_multi(abap_data, abap_pentads, proj_coords = FALSE, "year")
#' str(stOcc)
#'
#' ## List for non-spatial occupancy model
#' tOcc <- abapToSpOcc_multi(abap_data, seasons = "year")
#' str(tOcc)
#'
#'
abapToSpOcc_multi <- function(abap_data, pentads = NULL, proj_coords = TRUE, seasons = NULL){

    if(!requireNamespace("spOccupancy", quietly = TRUE)) {
        warning("Package spOccupancy doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

    # Sort data frame for consistency with other functions
    abap_data <- abap_data %>%
        dplyr::arrange(Pentad, StartDate)

    # Extract unique pentads
    pentad_id <- sort(unique(abap_data$Pentad))
    n_sites <- length(pentad_id)

    # Extract maximum number of visits in a single season
    if(is.null(seasons)){

        max_visits <- abap_data %>%
            dplyr::count(Pentad) %>%
            dplyr::pull(n) %>%
            max()

        n_seasons <- 1
        season_vec <- NULL

    } else {

        # Extract seasons and calculate max number of visits per season
        abap_data <- abap_data %>%
            dplyr::mutate(season = !!as.name(seasons))

        season_vec <- abap_data %>%
            dplyr::pull(season) %>%
            unique() %>%
            sort()

        n_seasons <- length(season_vec)

        max_visits <- abap_data %>%
            dplyr::count(Pentad, season) %>%
            dplyr::pull(n) %>%
            max()

    }


    ## Create empty arrays
    Y <- array(NA, dim = c(n_sites, n_seasons, max_visits),
               dimnames = list(pentad_id, season_vec, seq_len(max_visits)))

    obs_hours <- array(NA, dim = c(n_sites, n_seasons, max_visits),
                       dimnames = list(pentad_id, season_vec, seq_len(max_visits)))

    obs_jday <-  array(NA, dim = c(n_sites, n_seasons, max_visits),
                       dimnames = list(pentad_id, season_vec, seq_len(max_visits)))


    # Aux padding vector
    vpad <- rep(NA, max_visits)

    # Aux pentad column to join yearly results
    pentad_col <- abap_data %>%
        dplyr::distinct(Pentad) %>%
        dplyr::arrange(Pentad)

    for(k in seq_along(season_vec)){

        ## Create dataframe to format
        season_data <- abap_data %>%
            dplyr::filter(season == season_vec[k]) %>%
            dplyr::select(Pentad, Spp, TotalHours, StartDate) %>%
            dplyr::mutate(Spp = ifelse(Spp == "-", 0L, 1L),
                          julian_day = lubridate::yday(StartDate)) %>%
            dplyr::right_join(pentad_col, by = "Pentad") %>%      # Join in Pentads missing for the year
            dplyr::nest_by(Pentad) %>%
            dplyr::arrange(Pentad) %>%
            dplyr::mutate(hourpad = list(head(c(data$TotalHours, vpad), max_visits)),
                          jdaypad = list(head(c(data$julian_day, vpad), max_visits)))

        # Join in Pentads missing for the year
        # season_data <- pentad_col %>%
        #     dplyr::left_join(season_data, by = "Pentad")

        ## Extract detection histories
        det_hist <- season_data %>%
            dplyr::mutate(dets = list(head(c(data$Spp, vpad), max_visits))) %>%
            dplyr::select(Pentad, dets)

        Y[,k,] <- do.call("rbind", det_hist$dets)

        ## Extract total hours
        obs_hours[,k,] <- do.call("rbind", season_data$hourpad)

        ## Extract Julian day
        obs_jday[,k,] <- do.call("rbind", season_data$jdaypad)

    }

    # Make data list
    if(is.null(seasons)){
        spocc_data <- list(y = Y[,1,],
                           det.covs = list(hours = obs_hours[,1,], jday = obs_jday[,1,]))
    } else {
        spocc_data <- list(y = Y,
                           det.covs = list(hours = obs_hours, jday = obs_jday))
    }


    ## Add spatial data if necessary
    if(!is.null(pentads)){

        sf::st_agr(pentads) = "constant"
        aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

        if(isTRUE(proj_coords)){
            pentads <- sf::st_transform(pentads, aeaproj)
        }

        pentad_xy <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            dplyr::arrange(pentad) %>%
            sf::st_centroid() %>%
            sf::st_coordinates()

        spocc_data <- list(y = Y,
                           coords = pentad_xy,
                           det.covs = list(hours = obs_hours, jday = obs_jday))

        spocc_data <- c(spocc_data, list(coords = pentad_xy))

    }

    return(spocc_data)

}
