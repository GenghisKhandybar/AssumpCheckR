#install.packages("nortest")
#library(nortest)

#install.packages("rcompanion")
#library(rcompanion)


check_normality <- function(x, test_type = "SW", include_graph = TRUE, sig_level = 0.05, include_interpretation = TRUE) {

  # Extract variable name
  var_name <- deparse(substitute(x))

  # Check if 'x' is numeric
  if (!is.numeric(x)) {
    stop("Input 'x' must be numeric.")
  }

  # Check test type argument
  if (!missing(test_type)) {
    if (test_type %in% c("SW", "AD")) {
      if (test_type == "SW") {
        result <- shapiro.test(x)
      } else {
        result <- nortest::ad.test(x)
      }
    } else {
      stop("Invalid test type. Use 'SW' for Shapiro-Wilk test or 'AD' for Anderson-Darling test.")
    }
  } else {
    test_type <- "SW" # Default to Shapiro-Wilk test
    result <- shapiro.test(x)
  }

  # Check include_graph argument
  if (!is.logical(include_graph)) {
    stop("Include graph argument must be logical (TRUE/FALSE).")
  }

  # Generate histogram with normal distribution overlay if include_graph is TRUE
  if (include_graph) {
    plotNormalHistogram(x, prob = FALSE,
                        main = paste("Normal Distribution overlay on Histogram of", var_name),
                        xlab = var_name,
                        length = 1000)
  }

  # Check if 'sig_level' is numeric and between 0 and 1
  if (!(sig_level > 0 && sig_level < 1)) {
    stop("Input 'sig_level' must be numeric and between 0 and 1.")
  }

  # Include interpretation
  if (include_interpretation) {
    if (test_type == "SW") {
      test_name <- "Shapiro-Wilk"
    } else if (test_type == "AD") {
      test_name <- "Anderson-Darling"
    }

    if (result$p.value > sig_level) {
      cat(sprintf("According to the %s test for normality, at the %.2f significance level,\n we do not have evidence to conclude that %s is non-normal.\n", test_name, sig_level, var_name))
    } else {
      cat(sprintf("According to the %s test for normality, at the %.2f significance level,\n we have evidence to conclude that %s is non-normal.\n", test_name, sig_level, var_name))
    }
  }

  # Return the test result
  return(result)
}


