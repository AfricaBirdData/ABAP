library(ABAP)

# Load data from a random species
abap_visit_vars <- ABAP::getAbapData(151, "country", "south africa", .years = 2010:2020)

abap_visit_vars <- abap_visit_vars %>%
  dplyr::slice(0)

# Save data ---------------------------------------------------------------

# Save as data
usethis::use_data(abap_visit_vars, overwrite = TRUE)
