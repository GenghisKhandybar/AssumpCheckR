#' Checks the variance for a simple linear mode with 1 x and 1 y variable.
#'
#' @param y The response variable of interest
#' @param x The explanatory variable of interest
#' @param data The data from which the y and x variable belong to
#' @param test The type of variance test used
#' Defaults to "Breusch"
#' Option to change to "White"
#' @param sig_level Significance level for the variance test
#' @param plot Option to return a plot of the fitted residuals
#' Defaults to `TRUE`
#' @param interp Option to return an interpretation of the test
#' Defaults to `TRUE`
#'
#' @return A candy gram announcement
#'
#' @importFrom lmtest bptest
#' @importFrom rapportools is.boolean
#'
#' @export
check_variance <- function(y, x, test_type = "Breusch", sig_level = 0.05,
                           plot=TRUE, interp=TRUE) {

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
  if(!rapportools::is.boolean(plot)){
    stop("Input 'plot' must be boolean")
  }
  if(!rapportools::is.boolean(interp)){
    stop("Input 'interp' must be boolean")
  }

  model = lm(y ~ x)

  # Runs a test based on the test_type
  if(test_type == "Breusch"){
    test = lmtest::bptest(model)
  }else if(test_type == "White"){
    test = whitestrap::white_test(model)
  }else{
    stop("Test type is not valid")
  }

  # If requested, makes a plot
  if(plot){
    plot_result = make_plot(model)
  }


  # Check if 'sig_level' is between 0 and 1
  if (!(sig_level > 0 && sig_level < 1)) {
      stop("Input 'sig_level' must be numeric and between 0 and 1.")
  }

  # Creates a p value, the two tests uses different syntax
  if(test_type == "Breusch"){
    pval = test$p.value
  }else if (test_type == "White") {
    pval = test$p_value
  }

  # If requested, makes an interpretation to the test result
  if (interp){
    if (pval >= sig_level) {
      interpretation = make_interpretation(test_type, sig_level, TRUE)
    } else {
      interpretation = make_interpretation(test_type, sig_level, FALSE)
    }
  }

  # Returns what is requested
  if (interp && plot){
    return(list(test.result = test, plot = plot_result, interpretation = interpretation))
  } else if (interp) {
    return(list(test.result = test, interpretation = interpretation))
  } else if (plot) {
    return(list(test.result = test, plot = plot_result))
  } else {
    return(list(test.result = test))
  }

}

#' Makes a residual vs fitted plot for check_variance
#'
#' @param model The linear model created by the provided y and x variables
#'
#' @return A plot
make_plot <- function(model) {

  plot(model, which = 1)
  res = recordPlot()

}

#' Makes an interpretation for check_Variance
#'
#' @param test_type The type of test ran
#' @param sig_level Significance level of the test
#' @param sig Whether or not the result is significant
#'
#' @return A string
make_interpretation <- function(test_type, sig_level, sig){

  if (sig && test_type == "Breusch") {
    interp = paste("According to the Breusch-Pagan test for equal variance, at the",
                     sig_level, "significance level, we do not have evidence to conclude that equal variance is violated.")
  } else if (!sig && test_type == "Breusch"){
    interp = paste("According to the Breusch-Pagan test for equal variance, at the",
                   sig_level, "significance level, we have evidence to conclude that equal variance is violated.")
  } else if (sig && test_type == "White"){
    interp = paste("According to the White test for equal variance, at the",
                   sig_level, "significance level, we have evidence to conclude that equal variance is violated.")
  } else if (!sig && test_type == "White"){
    interp = paste("According to the White test for equal variance, at the",
                   sig_level, "significance level, we have evidence to conclude that equal variance is violated.")
  } else {
    interp = "Error"
  }

  interp

}
