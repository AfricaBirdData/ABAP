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

    # Aux padding vector
    vpad <- rep(NA, max_visits)

    ## Create detection histories
    det_hist <- abap_data %>%
        dplyr::select(Pentad, Spp) %>%
        dplyr::mutate(Spp = ifelse(Spp == "-", 0L, 1L)) %>%
        dplyr::nest_by(Pentad)

    det_hist <- det_hist %>%
        dplyr::mutate(dets = list(head(c(data$Spp, vpad), max_visits)))

    Y <- do.call("rbind", det_hist$dets)
    rownames(Y) <- pentad_id

    ## Extract total hours
    obs_hours <- abap_data %>%
        dplyr::select(Pentad, TotalHours) %>%
        dplyr::nest_by(Pentad) %>%
        dplyr::mutate(hourpad = list(head(c(data$TotalHours, vpad), max_visits)))

    obs_hours <- do.call("rbind", obs_hours$hourpad)
    rownames(obs_hours) <- pentad_id

    ## Extract Julian day
    obs_jday <- abap_data %>%
        dplyr::mutate(julian_day = lubridate::yday(StartDate)) %>%
        dplyr::select(Pentad, julian_day) %>%
        dplyr::nest_by(Pentad) %>%
        dplyr::mutate(jdaypad = list(head(c(data$julian_day, vpad), max_visits)))

    obs_jday <- do.call("rbind", obs_jday$jdaypad)
    rownames(obs_jday) <- pentad_id

    # Need to add dimnames


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
