# For the selection of species and years that we can use for testing see
# tests/testthat/helper.R

test_that("we get a tibble", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  my_card <- my_data$CardNo[1]

  card_data <- getCardRecords(my_card)

  expect_s3_class(card_data, "tbl_df")

})


# For the next tests we need a standard to match the outputs of getCardRecords()
# against. This is found at tests/testthat/fixtures/empty_card_df_yyyymmdd.rds
# where yyyymmdd is the data of the last file to use as reference. If the
# standard changed, we would need to generate a new file with the new standard
# and the new date. It would be good to keep all standards for future comparison

test_that("we get the correct number of columns", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  my_card <- my_data$CardNo[1]

  card_data <- getCardRecords(my_card)

  ref_data <- readRDS(test_path("fixtures", "empty_card_df_20231003.rds"))

  expect_equal(ncol(card_data), ncol(ref_data))

})


test_that("we get the correct types of columns", {

  my_data <- getAbapData(sample(spp, 1),
                         .region_type = "province",
                         .region = "Western Cape",
                         .years = sample(years, 1))

  my_card <- my_data$CardNo[1]

  card_data <- getCardRecords(my_card)

  ref_data <- readRDS(test_path("fixtures", "empty_card_df_20231003.rds"))

  expect_equal(sapply(card_data, class), sapply(ref_data, class))

})
