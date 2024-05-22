
test_that("Rejects null properly", {
  set.seed(42)
  x <- rnorm(500, 0,1)
  y <- (0.1*x)^2 + rnorm(500,0,2)
  check_linearity(x, y)

  check_results <- check_linearity(x, y, include_graph = FALSE)

  expect_equal(check_results[[1]]$p.value, 0.00063179, tolerance = 0.00001)
  expect_equal(check_results[[2]], "With p < 0.05, we are confident that removing data points from this model improves the fit more than what would be expected if there were a linear relationship. Therefore, we are 95% confident that the relationship between x and y is not linear.")

})




