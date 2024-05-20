#' Checks the linearity assumption of an x and y variable.
#'
#' @param x A single predictor variable.
#' @param y A numeric response variable.
#' @param sig_level The significance level for the test. 0.05 by default.
#' @param include_graph Whether to include the residual plot. TRUE by default.
#'
#' @return A test, interpretation, and plot (optional)
#'
#' @importFrom lmtest resettest
#' @importFrom ggplot2 ggplot, geom_point, geom_smooth, labs
#'
#' @export

install.packages("lmtest")
library(lmtest)
library(ggplot2)
library(magrittr)

check_linearity <- function(x, y, sig_level = 0.05, include_graph = TRUE){
  model <- lm(y ~ x)

  test_result <- resettest(model)

  if(include_graph){
    graph <- make_residual_plot(model$fitted.values, model$residuals)
  }
  list(test_result, graph)
}

# also try rainbow test?

# helper function

make_residual_plot <- function(preds, residuals){
  data.frame(preds = preds, residuals = residuals) %>%
    ggplot(aes(x = preds, y = residuals)) +
      geom_point() +
      geom_smooth() +
      labs(x = "Predicted",
           y = "Residual")
}
