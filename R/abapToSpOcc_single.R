#' ABAP to spOccupancy (single-species)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an list which can be used to fit single-species occupancy models using
#' \code{\link[spOccupancy]{spPGOcc}} (spatial) or \code{\link[spOccupancy]{PGOcc}}
#' (non-spatial). The `spOccupancy` package fits single-species, multi-species,
#' and integrated non-spatial and spatial occupancy models using Markov Chain Monte Carlo (MCMC).
#'
#' @param abap_data single-season ABAP data downloaded using \code{\link{getAbapData}}.
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#' @param proj_coords logical value indicating whether pentad coordinates are
#' projected (`TRUE`) or kept in decimal degree format (`FALSE`). Defaults to `TRUE`.
#' See details below for coordinate reference system used during transformation.
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
#' \code{\link[spOccupancy]{spPGOcc}}. If no spatial data are provided the output
#' can be used in \code{\link[spOccupancy]{PGOcc}}.\cr
#'
#' In addition to reformatting the detection/non-detection ABAP data for use in
#' `spOccupancy` occupancy models, this function also extracts two survey-level
#' covariates and adds them to the output list: ` hours` and `jday`. The `hours`
#' variable is the total number of hours spent atlassing which is recorded on the
#' pentad card and `jday` is the Julian day corresponding to the first day of
#' atlassing for that card.
#'
#' @note The processing time of `abapToSpOcc_single` can be considerably long if
#' the number of cards and pentads of the focal species is high, so patience may
#' be required.
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#' @seealso \code{\link[spOccupancy]{spPGOcc}}, \code{\link[spOccupancy]{PGOcc}}
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
#'
#' ## Return list for spatial occupancy model
#' spOcc <- abapToSpOcc_single(abap_data, abap_pentads)
#' str(spOcc)
#'
#' ## Return list for spatial occupancy model (without coordinate projection)
#' spOcc <- abapToSpOcc_single(abap_data, abap_pentads, proj_coords = FALSE)
#' str(spOcc)
#'
#' ## List for non-spatial occupancy model
#' spOcc <- abapToSpOcc_single(abap_data)
#' str(spOcc)
#'
abapToSpOcc_single <- function(abap_data, pentads = NULL, proj_coords = TRUE){

    if(!requireNamespace("spOccupancy", quietly = TRUE)) {
        warning("Package spOccupancy doesn't seem to be installed. We recommend installing it if you are using this function.")
    }

    pentad_id <- unique(abap_data$Pentad)

    n_sites <- length(pentad_id)

    max_visits <- abap_data %>%
        dplyr::count(Pentad) %>%
        dplyr::pull(n) %>%
        max()

    ## Extract spatial data
    if(!is.null(pentads)){

        sf::st_agr(pentads) = "constant"
        aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

        if(isTRUE(proj_coords)){
            pentads <- sf::st_transform(pentads, aeaproj)
        }

        pentad_xy <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            sf::st_centroid() %>%
            sf::st_coordinates()
    }

    ## Create empty arrays
    Y <-  array(NA, dim = c(n_sites, max_visits),
                dimnames = list(pentad_id, NULL))

    obs_hours <-  array(NA, dim = c(n_sites, max_visits),
                        dimnames = list(pentad_id, NULL))

    obs_jday <-  array(NA, dim = c(n_sites, max_visits),
                       dimnames = list(pentad_id, NULL))

    ## Populate detection/non-detection array (Y) and survey covariate arrays
    for(i in seq_len(n_sites)){

        dnd_vec <- abap_data %>%
            dplyr::filter(Pentad == pentad_id[i]) %>%
            dplyr::pull(Spp)

        dnd_vec <- dplyr::case_when(dnd_vec == "-" ~ 0,
                                    TRUE ~ 1)

        Y[i, seq_along(dnd_vec)] <- dnd_vec

        total_hours <- abap_data %>%
            dplyr::filter(Pentad == pentad_id[i]) %>%
            dplyr::pull(TotalHours)

        obs_hours[i, seq_along(total_hours)] <- total_hours

        jday <- abap_data %>%
            dplyr::filter(Pentad == pentad_id[i]) %>%
            dplyr::mutate(julian_day = lubridate::yday(StartDate)) %>%
            dplyr::pull(julian_day)

        obs_jday[i, seq_along(jday)] <- jday

    }

    if(!is.null(pentads)){

        spocc_data <- list(y = Y,
                           coords = pentad_xy,
                           det.covs = list(hours = obs_hours, jday = obs_jday))

    } else if (is.null(pentads)){

        spocc_data <- list(y = Y,
                           det.covs = list(hours = obs_hours, jday = obs_jday))
    }

    return(spocc_data)

}
