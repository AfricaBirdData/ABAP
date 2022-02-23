#' ABAP to unmarked (single season)
#'
#' @description Transforms a raw ABAP data frame into an unmarked-ready data frame
#' @param abap_data ABAP data downloaded using \code{\link{getAbapData}}
#'
#' @return An object in the same format as that returned by \code{\link[unmarked]{unmarkedFrameOccu}}
#' @note Total hours and Julian day are automatically added as observation-level covariates
#' @export
#'
#' @examples
#' abap_single <- getAbapData(.spp_code = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = 2012)
#'
#' um_df <- abapToUnmarked_single(abap_single)
abapToUnmarked_single <- function(abap_data){

    pentad_id <- unique(abap_data$Pentad)

    n_sites <- length(pentad_id)

    max_visits <- abap_data %>%
        dplyr::count(Pentad) %>%
        dplyr::pull(n) %>%
        max()

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

    ## Create named list of observation covariates
    obs_covs <- list(hours = as.data.frame(obs_hours),
                     jday = as.data.frame(obs_jday))

    ## Create unmarked data frame
    umf_abap <- unmarked::unmarkedFrameOccu(y = Y,
                                            obsCovs = obs_covs,
                                            mapInfo = NULL)

    return(umf_abap)
}
