#' ABAP to unmarked (single-season)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}}) into an \code{\link[unmarked]{unmarkedFrameOccu}} object which can be used to fit single-season occupancy models using \code{\link[unmarked]{occu}} (MacKenzie et. al 2002) or \code{\link[unmarked]{occuRN}} (Royle and Nichols, 2003) in `unmarked`.
#'
#' \code{\link[unmarked]{unmarkedFrameOccu}} objects can also be used to fit \code{\link[ubms]{stan_occu}} or \code{\link[ubms]{stan_occuRN}} models from the `ubms` package (which fits Unmarked Bayesian Models with Stan).
#'
#'
#' @param abap_data single-season ABAP data downloaded using \code{\link{getAbapData}}
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}. Defaults to `NULL`.
#'
#' @details In addition to reformatting the detection/non-detection ABAP data for use in `unmarked`  and `ubms` occupancy models, this function also extracts two survey-level covariates: ` hours` and `jday`. The `hours` variable is the total number of hours spent atlassing which is recorded on the pentad card and `jday` is the Julian day corresponding to the first day of atlassing for that card. If `pentads` are provided the `unmarked` frame will add the x and y coordinates as site covariates (`siteCovs`). The spatial data can be used as random effects to run a restricted spatial regression (RSR) occupancy model in `ubms`. See \code{\link[ubms]{RSR}} for more details.
#'
#' @return an object of class  \code{\link[unmarked]{unmarkedFrameOccu}}
#'
#' @note It's important to make sure that the `.region_type` and `.region` are the same when extracting `abap_data` and `pentads` (see example below).
#'
#' @seealso \code{\link[unmarked]{occu}}, \code{\link[unmarked]{occuRN}}, \code{\link[ubms]{stan_occu}}, \code{\link[ubms]{stan_occuRN}}
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @references
#' MacKenzie, D. I., J. D. Nichols, G. B. Lachman, S. Droege, J. Andrew Royle, and C. A. Langtimm. 2002. Estimating Site Occupancy Rates When Detection Probabilities Are Less Than One. Ecology 83: 2248-2255.
#'
#' Royle, J. A. and Nichols, J. D. (2003) Estimating Abundance from Repeated Presence-Absence
#' Data or Point Counts. Ecology, 84(3) pp. 777-790.
#' @export
#'
#' @examples
#' abap_single <- getAbapData(.spp_code = 212,
#'                            .region_type = "province",
#'                            .region = "Eastern Cape",
#'                            .years = 2012)
#'
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                  .region = "Eastern Cape")
#'
#' ## Return unmarked frame no site covariates
#' um_df <- abapToUnmarked_single(abap_single)
#'
#' ## Return unmarked frame with Pentad coordinates as site covariates
#' um_df <- abapToUnmarked_single(abap_single, abap_pentads)
#'
abapToUnmarked_single <- function(abap_data, pentads = NULL){

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
            sf::st_coordinates() %>%
            as.data.frame() %>%
            dplyr::rename_all(tolower)

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

    ## Create named list of observation covariates
    obs_covs <- list(hours = as.data.frame(obs_hours),
                     jday = as.data.frame(obs_jday))

    ## Create unmarked data frame
    if(!is.null(pentads)){

        umf_abap <- unmarked::unmarkedFrameOccu(y = Y,
                                                siteCovs = pentad_xy,
                                                obsCovs = obs_covs,
                                                mapInfo = NULL)

    } else if (is.null(pentads)){

        umf_abap <- unmarked::unmarkedFrameOccu(y = Y,
                                                obsCovs = obs_covs,
                                                mapInfo = NULL)
    }

    return(umf_abap)
}
