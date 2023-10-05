test_that("returns a deprecation message", {
  lifecycle::expect_deprecated(addVarEEclosestImage(ee_pentads = ee_data,
                                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
                                                    reducer = "mean",
                                                    maxdiff = 15,
                                                    bands = c("tmmn")))
})


test_that("informs about an alternative", {
  expect_message(addVarEEclosestImage(ee_pentads = ee_data,
                                      collection = "IDAHO_EPSCOR/TERRACLIMATE",
                                      reducer = "mean",
                                      maxdiff = 15,
                                      bands = c("tmmn")),
                 regexp = "This function has been discontinued. Please, use ABDtools::addVarEEclosestImage() instead",
                 fixed = TRUE)
})


