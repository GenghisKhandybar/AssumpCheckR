# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(AssumpCheckR)

test_check("AssumpCheckR")

data(iris)
library(lmtest)
library(tidyverse)
check_linearity(iris$Sepal.Length, iris$Sepal.Width)

model <- lm(iris$Sepal.Length ~ iris$Sepal.Width)

test_result <- raintest(model)

if(include_graph){
  graph <- make_residual_plot(model$fitted.values, model$residuals)
}
list(test_result, graph)
