test_that("returns a deprecation message", {
  lifecycle::expect_deprecated(addVarEEcollection(ee_pentads = ee_data,
                                                  collection = "IDAHO_EPSCOR/TERRACLIMATE",
                                                  dates = c("2010-01-01", "2011-01-01"),
                                                  temp_reducer = "mean",
                                                  spt_reducer = "mean",
                                                  bands = "tmmn"))
})


test_that("informs about an alternative", {
  expect_message(addVarEEcollection(ee_pentads = ee_data,
                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
                                    dates = c("2010-01-01", "2011-01-01"),
                                    temp_reducer = "mean",
                                    spt_reducer = "mean",
                                    bands = "tmmn"),
                 regexp = "This function has been discontinued. Please, use ABDtools::addVarEEcollection() instead",
                 fixed = TRUE)
})
