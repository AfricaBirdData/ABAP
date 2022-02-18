#' ABAP to unmarked
#'
#' @description Transforms a raw ABAP data into multiseason unmarked-ready data
#' @param abap_data ABAP data dowloaded using \link{getAbapData}.
#'
#' @return An \link{unmarkedMultFrame}
#' @export
#'
#' @examples
#' abap_multi <- getAbapData(.spp = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = c(2009,2010,2011,2012))
#'
#' um_df <- abapToUnmarked_multi(abap_multi)
abapToUnmarked_multi <- function(abap_data){

    abap_data <- abap_data %>%
        dplyr::mutate(year = lubridate::year(StartDate))

    years_vec <- abap_data %>%
        dplyr::pull(year) %>%
        unique() %>%
        sort()

    n_occasions <- length(years_vec)

    occasion_vec <- paste0(" 0", 1:n_occasions)

    pentad_id <- unique(abap_data$Pentad)

    n_sites <- length(pentad_id)

    max_visits <- abap_data %>%
        dplyr::count(Pentad, year) %>%
        dplyr::pull(n) %>%
        max()

    ## Create empty arrays
    Y <- array(NA, dim = c(n_sites, max_visits, n_occasions),
               dimnames = list(pentad_id, NULL, occasion_vec))

    obs_hours <- array(NA, dim = c(n_sites, max_visits, n_occasions),
                       dimnames = list(pentad_id, NULL, occasion_vec))

    obs_jday <-  array(NA, dim = c(n_sites, max_visits, n_occasions),
                       dimnames = list(pentad_id, NULL, occasion_vec))

    ## Populate detection/non-detection array (Y) and survey covariate arrays
    # Y[sites, visits, years]

    for(k in seq_along(years_vec)){

        year_data <- abap_data %>%
            dplyr::filter(year == years_vec[k])

        for(i in seq_len(n_sites)){

            dnd_vec <- year_data %>%
                dplyr::filter(Pentad == pentad_id[i]) %>%
                dplyr::pull(Spp)

            if(length(dnd_vec) == 0){
                next
            }

            dnd_vec <- dplyr::case_when(dnd_vec == "-" ~ 0,
                                        TRUE ~ 1)

            Y[i, seq_along(dnd_vec), k] <- dnd_vec

            # Add number of hours and Julian day
            total_hours <- year_data %>%
                dplyr::filter(Pentad == pentad_id[i]) %>%
                dplyr::pull(TotalHours)

            obs_hours[i, seq_along(total_hours), k] <- total_hours

            jday <- year_data %>%
                dplyr::filter(Pentad == pentad_id[i]) %>%
                dplyr::mutate(julian_day = lubridate::yday(StartDate)) %>%
                dplyr::pull(julian_day)

            obs_jday[i, seq_along(jday), k] <- jday

        }
    }

    ## Convert 3 dimensional array to matrix format
    Y <- matrix(Y, n_sites, max_visits*n_occasions)
    obs_hours <- matrix(obs_hours, n_sites, max_visits*n_occasions)
    obs_jday <- matrix(obs_jday, n_sites, max_visits*n_occasions)

    ## Create year occasion variable
    year <- matrix(occasion_vec, nrow(Y), n_occasions, byrow = TRUE)

    ## Create multi-season unmarked data frame
    umf_abap <- unmarked::unmarkedMultFrame(y = Y,
                                            obsCovs = list(hours = obs_hours,
                                                           jday = obs_jday),
                                            yearlySiteCovs = list(year = year),
                                            numPrimary = n_occasions)

    return(umf_abap)
}
