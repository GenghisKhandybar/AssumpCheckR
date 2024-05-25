#' Checks the linearity assumption of an x and y variable.
#'
#' @param x A single predictor variable.
#' @param y A numeric response variable.
#' @param sig_level The significance level for the test. 0.05 by default.
#' @param include_graph Whether to include the residual plot. TRUE by default.
#' @param include_interpretation Whether to include an interpretation of the results.
#'
#' @return A test, interpretation, and plot (optional)
#'
#' @importFrom lmtest raintest
#' @importFrom ggplot2 ggplot geom_point geom_smooth labs
#' @importFrom rapportools is.boolean
#'
#' @export

check_linearity <- function(x, y, sig_level = 0.05, include_graph = TRUE,
                            include_interpretation = TRUE){
  # Check if inputs are of the correct type
  if(!is.numeric(x)){
    stop("Input 'x' must be numeric")
  }
  if(!is.numeric(y)){
    stop("Input 'y' must be numeric")
  }
  if(!is.numeric(sig_level)){
    stop("Input 'sig_level' must be numeric and between 0 and 1.")
  }
  if(!rapportools::is.boolean(include_graph)){
    stop("Input 'include_graph' must be boolean")
  }
  if(!rapportools::is.boolean(include_interpretation)){
    stop("Input 'include_interpretation' must be boolean")
  }

  model <- lm(y ~ x)
  test <- lmtest::raintest(model)

  # Write interpretations based on the p-value and significance level
  if(test$p.value < sig_level){
    interpretation <- paste0("With p < ", sig_level,
                             ", we are confident that removing data points from this model ",
                             "improves the fit more than what would be expected if there were a ",
                             "linear relationship. Therefore, using a Rainobow Test, we are ", 100*(1-sig_level),
                             "% confident that the relationship between x and y is not linear.")
  } else {
    interpretation <- paste0("With p > ", sig_level,
                             ", we are have insufficient evidence to conclude that removing data points from this model ",
                             "improves the fit more than what would be expected if there were a ",
                             "linear relationship. Therefore, using a Rainobow Test, we do not have evidence to suggest that",
                             "that the relationship between x and y is not linear.")
  }

  # Generate graph if desired
  if(include_graph){
    plot_result <- make_residual_plot(model$fitted.values, model$residuals)
  }

  # Returns what is requested
  if (include_interpretation && include_graph){
    return(list(test.result = test, plot = plot_result, interpretation = interpretation))
  } else if (include_interpretation) {
    return(list(test.result = test, interpretation = interpretation))
  } else if (include_graph) {
    return(list(test.result = test, plot = plot_result))
  } else {
    return(list(test.result = test))
  }
}


#' Helper function: Creates the predicted vs. residuals plot
#'
#' @param preds A single predictor variable.
#' @param residuals A numeric residual in a response variable.
#'
#' @return A residual vs. predicted plot
#'
#' @importFrom ggplot2 ggplot geom_point geom_smooth labs

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
