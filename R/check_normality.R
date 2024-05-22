#' Checks the normality assumption of a variable.
#'
#' @param x A single variable.
#' @param test_type The type of normality test to be used. Shapiro-Wilk by default
#' @param sig_level The significance level for the test. 0.05 by default.
#' @param include_graph Whether to include histogram with normal distribution overlay. TRUE by default.
#' @param include_interpretation Whether to include interpretation. TRUE by default.
#'
#' @return NULL
#'
#' @importFrom nortest ad.test
#' @importFrom rcompanion plotNormalHistogram
#'
#' @export
check_normality <- function(x, test_type = "SW", sig_level = 0.05, include_graph = TRUE,
                            include_interpretation = TRUE) {

  # Extract variable name
  var_name <- deparse(substitute(x))

  # Check if 'x' is numeric
  if (!is.numeric(x)) {
    stop("Input 'x' must be numeric.")
  }

  # Check test type argument
  if (test_type %in% c("SW", "AD")) {
    if (test_type == "SW") {
      result <- shapiro.test(x)
    } else {
      result <- nortest::ad.test(x)
    }
  } else {
    stop("Invalid test type. Use 'SW' for Shapiro-Wilk test or 'AD' for Anderson-Darling test.")
  }

  # Check if 'sig_level' is numeric and between 0 and 1
  if (!(sig_level > 0 && sig_level < 1)) {
    stop("Input 'sig_level' must be numeric and between 0 and 1.")
  }

  # Print the test result
  cat(sprintf("Test type: %s\n", if (test_type == "SW") "Shapiro-Wilk"
              else "Anderson-Darling"))
  cat(sprintf("Null Hypothesis: Data is normally distributed\n"))
  cat(sprintf("P-value: %f\n", result$p.value))

  # Generate histogram with normal distribution overlay if include_graph is TRUE
  if (include_graph) {
    make_histogram(x, var_name)
  }

  # Include interpretation
  if (include_interpretation) {
    cat(interpreter(x, var_name, test_type, sig_level, result), "\n")
  }

  return(invisible(NULL))
}

#' Helper function: Creates histogram of data with a normal distribution overlay
#'
#' @param x A single variable.
#' @param var_name The name of x to be used the histogram title
#'
#' @return NULL
#'
#' @importFrom rcompanion plotNormalHistogram
make_histogram <- function(x, var_name){
  rcompanion::plotNormalHistogram(x, prob = FALSE,
                                  main = paste("Histogram of", var_name, "with Normal Distribution overlay"),
                                  xlab = var_name,
                                  length = 1000,
                                  cex.main = .9)
  return(NULL)
}

#' Helper function: Creates interpretation of results based on p-value and inputted significance level
#'
#' @param x A single variable.
#' @param var_name The name of x to be used in the interpretation
#' @param test_type The type of normality test to be used. Shapiro-Wilk by default
#' @param sig_level The significance level for the normality test
#' @param result The output of the normality test used
#'
#' @return Interpretation text as a string
interpreter <- function(x, var_name, test_type, sig_level, result){
  test_name <- if (test_type == "SW") "Shapiro-Wilk" else "Anderson-Darling"

  interpretation <- if (result$p.value > sig_level) {
    sprintf("\nAccording to the %s test for normality, at the %.2f significance level, \nwe do not have evidence to conclude that %s is non-normal.", test_name, sig_level, var_name)
  } else {
    sprintf("\nAccording to the %s test for normality, at the %.2f significance level, \nwe have evidence to conclude that %s is non-normal.", test_name, sig_level, var_name)
  }

  return(interpretation)
}
