library(tidyverse)
library(sf)

rm(list = ls())

# Load pentad data from kml. This was downloaded from the website
pentads_sabap <- sf::st_read("../gis/gis_data/project_sabap2.kml")

# Remove description because it is empty
pentads_sabap <- dplyr::select(pentads_sabap, Name)

# Remove Z dimension
pentads_sabap <- sf::st_zm(pentads_sabap)

# Save as data
usethis::use_data(pentads_sabap, overwrite = TRUE)

