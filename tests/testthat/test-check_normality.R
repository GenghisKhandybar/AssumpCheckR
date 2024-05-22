library(testthat)

test_that("check_normality handles non-numeric input with an error", {
  expect_error(check_normality(as.factor(mtcars$cyl)), "Input 'x' must be numeric.")
})

test_that("check_normality handles invalid test type with an error", {
  expect_error(check_normality(mtcars$mpg, test_type = "INVALID"), "Invalid test type.")
})

test_that("check_normality handles invalid significance level with an error", {
  expect_error(check_normality(mtcars$mpg, sig_level = 1.5), "Input 'sig_level' must be numeric and between 0 and 1.")
  expect_error(check_normality(mtcars$mpg, sig_level = -0.5), "Input 'sig_level' must be numeric and between 0 and 1.")
})

# Example to test output without the list structure
test_that("check_normality prints expected results and interpretation", {
  output <- capture.output(check_normality(mtcars$mpg, include_graph = FALSE, include_interpretation = TRUE))
  expect_true(any(grepl("Test type: Shapiro-Wilk", output)))
  expect_true(any(grepl("P-value:", output)))
  expect_true(any(grepl("According to the Shapiro-Wilk test for normality", output)))
})
