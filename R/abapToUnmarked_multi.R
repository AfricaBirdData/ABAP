#' ABAP to unmarked (multi-season)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an \code{\link[unmarked]{unmarkedMultFrame}} object which can be used to
#' fit dynamic occupancy models using \code{\link[unmarked]{colext}} (MacKenzie et. al 2003)
#' and \code{\link[ubms]{stan_colext}} from the `ubms` package (which fits
#' Unmarked Bayesian Models with Stan).
#'
#' @param abap_data multi-season ABAP data downloaded using \code{\link{getAbapData}}.
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}.
#' Defaults to `NULL`.
#'
#' @return an object of class \code{\link[unmarked]{unmarkedMultFrame}}
#'
#' @details In addition to reformatting the detection/non-detection ABAP data for
#' use in `unmarked` and `ubms` occupancy models, this function also extracts two
#' survey-level covariates: `hours` and `jday`. The `hours` variable is the total
#' number of hours spent atlassing which is recorded on the pentad card and `jday`
#' is the Julian day corresponding to the first day of atlassing for that card.\cr
#'
#' The function also adds the sampling year as a `yearlySiteCovs` which allows
#' year to be used in the formula for colonization, extinction and detection
#' probability.\cr
#'
#' If `pentads` are provided the `unmarked` frame will add the X and Y coordinates
#' as site-level covariates (`siteCovs`).
#'
#' @note The processing time of `abapToUnmarked_multi` can be considerably long
#' if the number of cards and pentads of the focal species is high, so patience
#' may be required.

#'
#' @seealso \code{\link[unmarked]{colext}}, \code{\link[ubms]{stan_colext}}
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @references
#' MacKenzie, D. I., J. D. Nichols, J. E. Hines, M. G. Knutson, and A. B.
#' Franklin. 2003. Estimating site occupancy, colonization, and local extinction
#' when a species is detected imperfectly. Ecology 84:2200-2207.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(unmarked)
#' abap_multi <- getAbapData(.spp = 212,
#'                           .region_type = "province",
#'                           .region = "Eastern Cape",
#'                           .years = c(2009,2010,2011,2012))
#'
#' abap_pentads <- getRegionPentads(.region_type = "province",
#'                                   .region = "Eastern Cape")
#'
#' ## Return unmarked frame no site covariates
#' um_df <- abapToUnmarked_multi(abap_multi)
#' summary(um_df)
#'
#' ## Return unmarked frame with Pentad coordinates as site covariates
#' um_df <- abapToUnmarked_multi(abap_data = abap_multi, pentads = abap_pentads)
#' summary(um_df)
#' }
abapToUnmarked_multi <- function(abap_data, pentads = NULL){

    if(!requireNamespace("unmarked", quietly = TRUE)) {
        stop("Package unmarked doesn't seem to be installed. Please install before using this function.",
            call. = FALSE)
    }

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

    ## Extract spatial data
    if(!is.null(pentads)){

        sf::st_agr(pentads) = "constant"

        pentad_xy <- pentads %>%
            dplyr::filter(pentad %in% pentad_id) %>%
            dplyr::arrange(pentad) %>%
            sf::st_centroid() %>%
            sf::st_coordinates() %>%
            as.data.frame() %>%
            dplyr::rename_all(tolower)

    } else {

        pentad_xy <- NULL
    }

    ## Create empty arrays
    Y <- array(NA, dim = c(n_sites, max_visits, n_occasions),
               dimnames = list(pentad_id, NULL, occasion_vec))

    obs_hours <- array(NA, dim = c(n_sites, max_visits, n_occasions),
                       dimnames = list(pentad_id, NULL, occasion_vec))

    obs_jday <-  array(NA, dim = c(n_sites, max_visits, n_occasions),
                       dimnames = list(pentad_id, NULL, occasion_vec))

    ## Populate detection/non-detection array (Y) and survey covariate arrays
    # Y[sites, visits, years]

    # Aux pentad column to join yearly results
    pentad_col <- abap_data %>%
        dplyr::group_by(Pentad) %>%
        dplyr::summarise(Pentad = unique(Pentad)) %>%
        dplyr::arrange(Pentad)


    # Aux padding vector
    vpad <- rep(NA, max_visits)

    for(k in seq_along(years_vec)){

        ## Create dataframe to format
        year_data <- abap_data %>%
            dplyr::filter(year == years_vec[k]) %>%
            dplyr::select(Pentad, Spp, TotalHours, StartDate) %>%
            dplyr::mutate(Spp = ifelse(Spp == "-", 0L, 1L),
                          julian_day = lubridate::yday(StartDate)) %>%
            dplyr::right_join(pentad_col, by = "Pentad") %>%      # Join in Pentads missing for the year
            dplyr::nest_by(Pentad) %>%
            dplyr::arrange(Pentad) %>%
            dplyr::mutate(hourpad = list(head(c(data$TotalHours, vpad), max_visits)),
                          jdaypad = list(head(c(data$julian_day, vpad), max_visits)))

        # Join in Pentads missing for the year
        # year_data <- pentad_col %>%
        #     dplyr::left_join(year_data, by = "Pentad")

        ## Extract detection histories
        det_hist <- year_data %>%
            dplyr::mutate(dets = list(head(c(data$Spp, vpad), max_visits))) %>%
            dplyr::select(Pentad, dets)

        Y[,,k] <- do.call("rbind", det_hist$dets)

        ## Extract total hours
        obs_hours[,,k] <- do.call("rbind", year_data$hourpad)

        ## Extract Julian day
        obs_jday[,,k] <- do.call("rbind", year_data$jdaypad)

    }

    ## Convert 3 dimensional array to matrix format
    Y <- matrix(Y, n_sites, max_visits*n_occasions,
                dimnames = list(pentad_col$Pentad, NULL))

    obs_hours <- matrix(obs_hours, n_sites, max_visits*n_occasions,
                        dimnames = list(pentad_col$Pentad, NULL))

    obs_jday <- matrix(obs_jday, n_sites, max_visits*n_occasions,
                       dimnames = list(pentad_col$Pentad, NULL))

    ## Create year occasion variable
    year <- matrix(occasion_vec, nrow(Y), n_occasions, byrow = TRUE)

    ## Create multi-season unmarked data frame
    umf_abap <- unmarked::unmarkedMultFrame(y = Y,
                                            obsCovs = list(hours = obs_hours,
                                                           jday = obs_jday),
                                            siteCovs = pentad_xy,
                                            yearlySiteCovs = list(year = year),
                                            numPrimary = n_occasions)

    return(umf_abap)
}
