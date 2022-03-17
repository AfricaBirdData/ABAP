#' ABAP to spOccupancy (single-species)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}}) into an list which can be used to fit single-species occupancy models using \code{\link[spOccupancy]{spPGOcc}}(spatial) or \code{\link[spOccupancy]{PGOcc}}(non-spatial). The `spOccupancy` package fits single-species, multi-species, and integrated non-spatial and spatial occupancy models using Markov Chain Monte Carlo (MCMC).
#'
#' @param abap_data single-season ABAP data downloaded using \code{\link{getAbapData}}.
#' @param pentads an sf object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#'
#' @return A list containing data necessary for model fitting in `spOccupancy`. List elements are `y` (detection/non-detection data), `det.covs` (survey-level covariates), and `coords` (x and y centroids of pentads if spatial data are supplied).
#'
#' @details The \code{\link[spOccupancy]{spPGOcc}} function takes `coords` as an argument with X and Y coordinates included for each site. Within the context of ABAP these `coords` are the centroid of each sampling pentad. In order to provide these spatial data to this function, simply use \code{\link{getRegionPentads}} and provide the same inputs for `.region_type` and `.region` that are specified in corresponding \code{\link{getAbapData}} call.\cr
#'
#' If `pentads` are specified then the output can be automatically used in \code{\link[spOccupancy]{spPGOcc}}. If no spatial data are provided the output can be used in \code{\link[spOccupancy]{PGOcc}}.\cr
#'
#' In addition to reformatting the detection/non-detection ABAP data for use in `spOccupancy` occupancy models, this function also extracts two survey-level covariates and adds them to the output list: ` hours` and `jday`. The `hours` variable is the total number of hours spent atlassing which is recorded on the pentad card and `jday` is the Julian day corresponding to the first day of atlassing for that card.
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
#' abapToSpOcc_single(abap_data, abap_pentads)
#'
#' ## List for non-spatial occupancy model
#' abapToSpOcc_single(abap_data)
abapToSpOcc_single <- function(abap_data, pentads = NULL){

    pentad_id <- unique(abap_data$Pentad)

    n_sites <- length(pentad_id)

    max_visits <- abap_data %>%
        dplyr::count(Pentad) %>%
        dplyr::pull(n) %>%
        max()

    ## Extract spatial data
    if(!is.null(pentads)){

        sf::st_agr(pentads) = "constant"

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
