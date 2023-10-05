test_that("returns a deprecation message", {
  lifecycle::expect_deprecated(uploadPentadsToEE(pentads = pentads,
                                                 asset_id = assetId,
                                                 load = TRUE))
})


test_that("informs about an alternative", {
  expect_message(uploadPentadsToEE(pentads = pentads,
                                   asset_id = assetId,
                                   load = TRUE),
                 regexp = "This function has been discontinued. Please, use ABDtools::uploadFeaturesToEE() instead",
                 fixed = TRUE)
})
