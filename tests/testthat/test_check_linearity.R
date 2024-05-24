
test_that("Rejects null properly", {
  set.seed(0)
  x <- rnorm(10000, 0, 1)
  y <- (x)^2 + rnorm(10000,0,1)

  check_results <- check_linearity(x, y, include_graph = FALSE)
  expect_equal(check_results[[1]]$p.value, 0.04574, tolerance = 0.0001)
  expect_equal(check_results[[2]], "With p < 0.05, we are confident that removing data points from this model improves the fit more than what would be expected if there were a linear relationship. Therefore, we are 95% confident that the relationship between x and y is not linear.")

})
library(AssumpCheckR)
test_that("Accepts null properly", {
  set.seed(0)
  x <- rnorm(10000, 0, 1)
  y <- (x) + rnorm(10000,0,1)

  check_results <- check_linearity(x, y, include_graph = TRUE)
  expect_equal(check_results[[1]]$p.value, 0.4829, tolerance = 0.0001)
  expect_equal(check_results[[2]], "With p > 0.05, we are have insufficient evidence to conclude that removing data points from this model improves the fit more than what would be expected if there were a linear relationship. Therefore, we do not have evidence to suggest thatthat the relationship between x and y is not linear.")

})


