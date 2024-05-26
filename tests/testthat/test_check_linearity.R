
test_that("check_linearity rejects null properly", {
  set.seed(0)
  x <- rnorm(10000, 0, 1)
  y <- (x)^2 + rnorm(10000,0,1)

  check_results <- check_linearity(x, y, include_graph = FALSE)
  expect_equal(check_results[["test.result"]]$p.value, 0.04574, tolerance = 0.0001)
  expect_equal(check_results[["interpretation"]], "With p < 0.05, we are confident that removing data points from this model improves the fit more than what would be expected if there were a linear relationship. Therefore, using a Rainobow Test, we are 95% confident that the relationship between x and y is not linear.")
})

test_that("check_linearity accepts null properly", {
  set.seed(0)
  x <- rnorm(10000, 0, 1)
  y <- (x) + rnorm(10000,0,1)

  check_results <- check_linearity(x, y, include_graph = FALSE)
  expect_equal(check_results[["test.result"]]$p.value, 0.4829, tolerance = 0.0001)
  expect_equal(check_results[["interpretation"]], "With p > 0.05, we are have insufficient evidence to conclude that removing data points from this model improves the fit more than what would be expected if there were a linear relationship. Therefore, using a Rainobow Test, we do not have evidence to suggest thatthat the relationship between x and y is not linear.")
})

test_that("check_linearity handles non-numeric input with an error", {
  expect_error(check_linearity(as.factor(mtcars$cyl), mtcars$gear), "Input 'x' must be numeric")
})
