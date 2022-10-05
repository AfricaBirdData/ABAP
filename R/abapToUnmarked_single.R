#' ABAP to unmarked (single-season)
#'
#' @description This function transforms a raw ABAP data frame (returned by \code{\link{getAbapData}})
#' into an \code{\link[unmarked]{unmarkedFrameOccu}} object which can be used to
#' fit single-season occupancy models using \code{\link[unmarked]{occu}} (MacKenzie et. al 2002)
#' or \code{\link[unmarked]{occuRN}} (Royle and Nichols, 2003) in `unmarked`.
#'
#' \code{\link[unmarked]{unmarkedFrameOccu}} objects can also be used to fit \code{\link[ubms]{stan_occu}}
#' or \code{\link[ubms]{stan_occuRN}} models from the `ubms` package (which fits
#' Unmarked Bayesian Models with Stan).
#'
#'
#' @param abap_data single-season ABAP data downloaded using \code{\link{getAbapData}}
#' @param pentads an `sf` object returned by \code{\link{getRegionPentads}}.
#' Defaults to `NULL`.
#'
#' @details In addition to reformatting the detection/non-detection ABAP data
#' for use in `unmarked`  and `ubms` occupancy models, this function also extracts
#' two survey-level covariates: ` hours` and `jday`. The `hours` variable is the
#' total number of hours spent atlassing which is recorded on the pentad card and
#' `jday` is the Julian day corresponding to the first day of atlassing for that
#' card. If `pentads` are provided the `unmarked` frame will add the x and y
#' coordinates as site covariates (`siteCovs`). The spatial data can be used as
#' random effects to run a restricted spatial regression (RSR) occupancy model
#' in `ubms`. See \code{\link[ubms]{RSR}} for more details.
#'
#' @return an object of class  \code{\link[unmarked]{unmarkedFrameOccu}}
#'
#' @note It's important to make sure that the `.region_type` and `.region` are
#' the same when extracting `abap_data` and `pentads` (see example below). The
#' processing time of `abapToUnmarked_single` can be considerably long if the
#' number of cards and pentads of the focal species is high, so patience may be
#' required.

#'
#' @seealso \code{\link[unmarked]{occu}}, \code{\link[unmarked]{occuRN}},
#' \code{\link[ubms]{stan_occu}}, \code{\link[ubms]{stan_occuRN}}
#'
#' @author Dominic Henry <dominic.henry@gmail.com> \cr
#' Pachi Cervantes
#'
#' @references
#' MacKenzie, D. I., J. D. Nichols, G. B. Lachman, S. Droege, J. Andrew Royle,
#' and C. A. Langtimm. 2002. Estimating Site Occupancy Rates When Detection
#' Probabilities Are Less Than One. Ecology 83: 2248-2255.
#'
#' Royle, J. A. and Nichols, J. D. (2003) Estimating Abundance from Repeated
#' Presence-Absence
#' Data or Point Counts. Ecology, 84(3) pp. 777-790.
#' @export
#'
#' @examples
#' \dontrun{
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
#'}
abapToUnmarked_single <- function(abap_data, pentads = NULL){

    if(!requireNamespace("unmarked", quietly = TRUE)) {
        stop("Package unmarked doesn't seem to be installed. Please install before using this function.",
             call. = FALSE)
    }

    pentad_id <- unique(abap_data$Pentad)

    max_visits <- abap_data %>%
        dplyr::count(Pentad) %>%
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

    }

    # Aux padding vector
    vpad <- rep(NA, max_visits)

    ## Create dataframe to format
    format_df <- abap_data %>%
        dplyr::select(Pentad, Spp, TotalHours, StartDate) %>%
        dplyr::mutate(Spp = ifelse(Spp == "-", 0L, 1L),
                      julian_day = lubridate::yday(StartDate)) %>%
        dplyr::nest_by(Pentad) %>%
        dplyr::arrange(Pentad) %>%
        dplyr::mutate(hourpad = list(head(c(data$TotalHours, vpad), max_visits)),
                      jdaypad = list(head(c(data$julian_day, vpad), max_visits)))

    ## Extract detection histories
    det_hist <- format_df %>%
        dplyr::mutate(dets = list(head(c(data$Spp, vpad), max_visits)))

    Y <- do.call("rbind", det_hist$dets)
    rownames(Y) <- format_df$Pentad

    ## Extract total hours
    obs_hours <- do.call("rbind", format_df$hourpad)
    rownames(obs_hours) <- format_df$Pentad

    ## Extract Julian day
    obs_jday <- do.call("rbind", format_df$jdaypad)
    rownames(obs_jday) <- format_df$Pentad

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
