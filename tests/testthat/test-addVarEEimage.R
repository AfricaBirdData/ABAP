test_that("returns a deprecation message", {
  lifecycle::expect_deprecated(addVarEEimage(ee_pentads = ee_data,
                                             image = "JRC/GSW1_3/GlobalSurfaceWater",
                                             reducer = "mean",
                                             bands = "occurrence"))
})


test_that("informs about an alternative", {
  expect_message(addVarEEimage(ee_pentads = ee_data,
                               image = "JRC/GSW1_3/GlobalSurfaceWater",
                               reducer = "mean",
                               bands = "occurrence"),
                 regexp = "This function has been discontinued. Please, use ABDtools::addVarEEimage() instead",
                 fixed = TRUE)
})
