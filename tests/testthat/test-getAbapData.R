# For the selection of species and years that we can use for testing see
# tests/testthat/helper.R

test_that("we get a tibble", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  expect_s3_class(my_data, "tbl_df")

})

# For the next tests we need a standard to match the outputs of getAbapData()
# against. This is found at tests/testthat/fixtures/empty_abap_df_yyyymmdd.rds
# where yyyymmdd is the data of the last file to use as referece. If the
# standard changed, we would need to generate a new file with the new standard
# and the new date. It would be good to keep all standards for future comparison

test_that("we get the correct number of columns", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  ref_data <- readRDS(test_path("fixtures", "empty_abap_df_20231003.rds"))

  expect_equal(ncol(my_data), ncol(ref_data))

})


test_that("we get the correct types of columns", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  ref_data <- readRDS(test_path("fixtures", "empty_abap_df_20231003.rds"))

  expect_equal(sapply(my_data, class), sapply(ref_data, class))

})


test_that("different species only differ in the last four columns", {

  species <- sample(spp, 2, replace = FALSE)
  year <- sample(years, 1)

  data1 <- getAbapData(species[1],
                       .region_type = "province",
                       .region = "Western Cape",
                       .years = year)

  data2 <- getAbapData(species[2],
                       .region_type = "province",
                       .region = "Western Cape",
                       .years = year)

  expect_equal(data1[,1:20], data2[,1:20])

})
