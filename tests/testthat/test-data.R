# tests/testthat/test-data.R
library(testthat)

test_that("catch_full.rds file exists", {
  expect_true(file.exists(here::here("catch_full.rds")))
})

# Load data once for all tests
catch_full <- readRDS(here::here("catch_full.rds"))

test_that("catch_full has expected columns", {
  expected_cols <- c("date", "species", "catch_kg", "latitude", 
                     "longitude", "country", "season", "crew_size",
                     "vessel_name", "port")
  expect_true(all(expected_cols %in% names(catch_full)))
})

test_that("catch_full has no NA in required columns", {
  expect_false(any(is.na(catch_full$country)))
  expect_false(any(is.na(catch_full$species)))
  expect_false(any(is.na(catch_full$catch_kg)))
})

test_that("catch_kg values are positive", {
  expect_true(all(catch_full$catch_kg > 0))
})