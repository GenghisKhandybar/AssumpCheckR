#' Checks the linearity assumption of an x and y variable.
#'
#' @param x A single predictor variable.
#' @param y A numeric response variable.
#' @param sig_level The significance level for the test. 0.05 by default.
#' @param include_graph Whether to include the residual plot. TRUE by default.
#'
#' @return A test, interpretation, and plot (optional)
#'
#' @importFrom lmtest raintest
#' @importFrom ggplot2 ggplot, geom_point, geom_smooth, labs
#'
#' @export

check_linearity <- function(x, y, sig_level = 0.05, include_graph = TRUE){
  model <- lm(y ~ x)

  test_result <- lmtest::raintest(model)

  # Write interpretations based on the p-value and significance level
  if(test_result$p.value < sig_level){
    interpretation <- paste0("With p < ", sig_level,
                             ", we are confident that removing data points from this model ",
                             "improves the fit more than what would be expected if there were a ",
                             "linear relationship. Therefore, we are ", 100*(1-sig_level),
                             "% confident that the relationship between x and y is not linear.")
  } else {
    interpretation <- paste0("With p > ", sig_level,
                             ", we are have insufficient evidence to conclude that removing data points from this model ",
                             "improves the fit more than what would be expected if there were a ",
                             "linear relationship. Therefore, we do not have evidence to suggest that",
                             "that the relationship between x and y is not linear.")
  }

  # Generate graph if desired
  if(include_graph){
    graph <- make_residual_plot(model$fitted.values, model$residuals)
    list(test_result, interpretation, graph)
  }
  list(test_result, interpretation)
}

#' Helper function: Creates the predicted vs. residuals plot
#'
#' @param preds A single predictor variable.
#' @param residuals A numeric residual in a response variable.
#'
#' @return A residual vs. predicted plot
#'
#' @importFrom ggplot2 ggplot, geom_point, geom_smooth, labs

make_residual_plot <- function(preds, residuals){
  data.frame(preds = preds, residuals = residuals) %>%
    ggplot2::ggplot(ggplot2::aes(x = preds, y = residuals)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = loess, formula = y ~ x) +
      ggplot2::labs(x = "Predicted",
           y = "",
           title = "Predicted vs. Residuals Plot") +
      ggplot2::theme_bw()
}
