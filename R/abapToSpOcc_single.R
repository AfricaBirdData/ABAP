#' ABAP to spOccupancy single species
#'
#' @description Transforms a raw ABAP data frame into a list formatted for use in a single-species spOccupancy model
#'
#' @param abap_data ABAP data downloaded using \code{\link{getAbapData}}
#' @param pentads sf object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#'
#' @note If pentads are specified then output can be used in `spOccupancy::spPGOcc`. If no spatial data are provided the output can be used in `spOccupancy::spPGOcc`
#' @return A list containing data necessary for model fitting in `spOccupancy`. List elements are `y`,  `det.covs`, and `coords` (if pentad data supplied)
#' @export
#'
#' @examples
#' abap_single <- getAbapData(.spp_code = 212, .region_type = "province",
#'                            .region = "Eastern Cape", .years = 2012)
#'
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#' ## List for spatial model
#' abapToSpOcc_single(abap_single, abap_pentads)
#'
#' ## List for non-spatial model
#' abapToSpOcc_single(abap_single)
abapToSpOcc_single <- function(abap_data, pentads = NULL){

    pentad_id <- unique(abap_data$Pentad)

    n_sites <- length(pentad_id)

    max_visits <- abap_data %>%
        dplyr::count(Pentad) %>%
        dplyr::pull(n) %>%
        max()

    ## Extract spatial data
    if(!is.null(pentads)){

        pentads <- pentads %>%
            dplyr::filter(pentad %in% pentad_id)

        pentad_xy <- sf::st_coordinates(
            sf::st_centroid(pentads)
        )
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
