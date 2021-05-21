#' Clean raw SABAP data just extracted from server
#'
#' @param rawdata tibble with SABAP data extracted from server
#' @param minhours a double that gives the threshold (in hours) for filtering out years with too little effort
#'
#' @return a tibble with clean names and variables
#' @export
#'
#' @examples
#' getSabapData(123, region_type = "pentad", region = "2505_2850") %>%
#' cleanSabapData()
#' getSabapData(169, region_type = "province", region = "Eastern Cape", years = 2008) %>%
#' cleanSabapData(minhours = 3)
cleanSabapData <- function(rawdata, minhours = 0){

  cleandata <- rawdata %>%
    # One if detected / 0 if not detected
    dplyr::mutate(spp = dplyr::case_when(Spp == "-" ~ 0,
                                         TRUE ~ 1)) %>%
    # Filter out those pentads with little effort
    dplyr::filter(TotalHours >= minhours) %>%
    # dplyr::mutate(totalhours = ifelse(totalhours > 50, 50, totalhours)) %>% why??
    # Fix dates and times
    dplyr::mutate(log_hours = log(TotalHours),
                  date = lubridate::ymd(StartDate),
                  month = lubridate::month(date),
                  date = lubridate::yday(date),
                  year = lubridate::year(StartDate),
                  # Fix card numbers and pentads
                  CardNo = stringr::str_replace_all(CardNo,"c", "_"),
                  Pentad = stringr::str_replace_all(Pentad, "c", "_"),
                  # Fix latitude / longitude
                  lat = -(as.numeric(substr(Pentad,1,2)) + (as.numeric(substr(Pentad,3,4)) + 2.5)/60),
                  lon = as.numeric(substr(Pentad,6,7)) + (as.numeric(substr(Pentad,8,9)) + 2.5)/60,
                  latmin = as.numeric(substr(Pentad,3,4)),
                  lonmin = as.numeric(substr(Pentad,8,9)),
                  let1 = ifelse(latmin<30 & lonmin<30, "A", ifelse(latmin<30 & lonmin>29,"B", ifelse(latmin>29 & lonmin<30,"C", "D"))),
                  let2 = ifelse(latmin %in% c(0,5,10,30,35,40) & lonmin %in% c(0,5,10,30,35,40), "A",
                                ifelse(latmin %in% c(0,5,10,30,35,40) & lonmin %in% c(15,20,25,45,50,55),"B",
                                       ifelse(latmin %in% c(15,20,25,45,50,55) & lonmin %in% c(0,5,10,30,35,40),"C", "D"))),
                  QDGC = paste(substr(Pentad,1,2), substr(Pentad,6,7), let1, let2, sep=""))

  return(cleandata)

}
