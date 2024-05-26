test_that("check_variance basic", {
  my_result <- check_variance(mtcars$mpg, mtcars$disp, include_graph = FALSE)


  model = lm(mtcars$mpg ~ mtcars$disp)

  correct <- list(test.result = lmtest::bptest(model),
                  interpretation = "According to the Breusch-Pagan test for equal variance, at the 0.05 significance level, we do not have evidence to conclude that equal variance is violated.")

  expect_equal(my_result, correct)
})

test_that("check_variance string x", {

  expect_error(check_variance(x = "test", mtcars$disp), "Input 'x' must be numeric")

})

test_that("check_variance number include_interpretation", {

  expect_error(check_variance(mtcars$mpg, mtcars$disp, include_interpretation = 3), "Input 'interp' must be boolean")

})
