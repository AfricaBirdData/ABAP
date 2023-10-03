# For the selection of species and years that we can use for testing see
# tests/testthat/helper.R

test_that("we get an sf object", {

  my_data <- getRegionPentads(.region_type = "province",
                              .region = "Western Cape")

  expect_s3_class(my_data, "sf")

})

# For the next tests we need a standard to match the outputs of getRegionPentads()
# against. This is found at tests/testthat/fixtures/empty_pentad_df_yyyymmdd.rds
# where yyyymmdd is the data of the last file to use as reference. If the
# standard changed, we would need to generate a new file with the new standard
# and the new date. It would be good to keep all standards for future comparison

test_that("we get the correct number of columns", {

  my_data <- getRegionPentads(.region_type = "province",
                              .region = "Western Cape")

  ref_data <- readRDS(test_path("fixtures", "empty_pentad_df_20231003.rds"))

  expect_equal(ncol(my_data), ncol(ref_data))

})


test_that("we get the correct types of columns", {

  my_data <- getRegionPentads(.region_type = "province",
                              .region = "Western Cape")

  ref_data <- readRDS(test_path("fixtures", "empty_pentad_df_20231003.rds"))

  expect_equal(sapply(my_data, class), sapply(ref_data, class))

})
