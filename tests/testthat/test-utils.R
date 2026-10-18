library(testthat)

source("../../R/utils.R")

test_that("calculate_alcohol calculates pure alcohol volume", {

  # 12 oz drink at 5% ABV
  result <- calculate_alcohol(
    abv = 5,
    volume = 12
  )

  expect_equal(
    result,
    0.05 * (12 * 29.574)
  )
})

test_that("calculate_alcohol is vectorized", {

  result <- calculate_alcohol(
    abv = c(5, 10),
    volume = c(12, 6)
  )

  expected <- c(
    0.05 * (12 * 29.574),
    0.10 * (6 * 29.574)
  )

  expect_equal(result, expected)
})

test_that("calculate_bac calculates BAC", {

  result <- calculate_bac(
    alcohol = 30,
    weight_g = 80000,
    r = 0.68,
    elapsed_hours = 2
  )

  expected <- (30 / (80000 * 0.68) * 100) - (2 * 0.015)

  expect_equal(result, expected)
})

test_that("calculate_bac does not return negative BAC", {

  result <- calculate_bac(
    alcohol = 10,
    weight_g = 80000,
    r = 0.68,
    elapsed_hours = 10
  )

  expect_equal(result, 0)
})

test_that("format_duration formats durations at appropriate scales", {

  expect_equal(
    format_duration(9000),
    "1 years 10 days"
  )

  expect_equal(
    format_duration(50),
    "2 days 2 hours"
  )

  expect_equal(
    format_duration(2.5),
    "2 hours 30 minutes"
  )

  expect_equal(
    format_duration(0.5),
    "30 minutes"
  )
})

test_that("format_duration handles unit boundaries", {

  expect_equal(
    format_duration(8760),
    "1 years 0 days"
  )

  expect_equal(
    format_duration(24),
    "1 days 0 hours"
  )

  expect_equal(
    format_duration(1),
    "1 hours 0 minutes"
  )
})