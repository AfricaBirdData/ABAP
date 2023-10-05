test_that("returns a deprecation message", {
  lifecycle::expect_deprecated(EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
                                                       dates = c("2008-01-01", "2020-01-01"),
                                                       band = "NDVI",                       # You can find what bands are available from GEE catalog
                                                       group_type = "year",
                                                       groups = 2008:2019,
                                                       reducer = "mean",
                                                       unmask = FALSE))
})


test_that("informs about an alternative", {
  expect_message(EEcollectionToMultiband(collection = "MODIS/006/MOD13A2",
                                         dates = c("2008-01-01", "2020-01-01"),
                                         band = "NDVI",                       # You can find what bands are available from GEE catalog
                                         group_type = "year",
                                         groups = 2008:2019,
                                         reducer = "mean",
                                         unmask = FALSE),
                 regexp = "This function has been discontinued. Please, use ABDtools::EEcollectionToMultiband() instead",
                 fixed = TRUE)
})
