#install.packages("nortest")
#library(nortest)

#install.packages("rcompanion")
#library(rcompanion)


check_normality <- function(x, test_type = "SW", include_graph = TRUE, sig_level = 0.05, include_interpretation = TRUE) {

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
    plotNormalHistogram(mtcars$mpg, prob = FALSE,
                        main = "Normal Distribution overlay on Histogram of",
                        length = 1000)
  }

  # Check if 'sig_level' is numeric and between 0 and 1
  if (!is.numeric(sig_level)) {
    stop("Input 'sig_level' must be numeric and between 0 and 1.")
  if (!(sig_level > 0 && sig_level < 1))
    stop("Input 'sig_level' must be numeric and between 0 and 1.")
  }

  # Include interpretation
  if (include_interpretation && test_type == "SW") {
    if (result$p.value > sig_level) {
      cat("According to the Shapiro-Wilk test for normality, at the", sig_level, "significance level, we do not have evidence to conclude that x is non-normal.\n")
    } else {
      cat("According to the Shapiro-Wilk test for normality, at the", sig_level, "significance level, we have evidence to conclude that x is non-normal.\n")
    }
  }
  if (include_interpretation && test_type == "AD") {
    if (result$p.value > sig_level) {
      cat("According to the Anderson-Darling test for normality, at the", sig_level, "significance level, we do not have evidence to conclude that x is non-normal.\n")
    } else {
      cat("According to the Anderson-Darling test for normality, at the", sig_level, "significance level, we have evidence to conclude that x is non-normal.\n")
    }
  }

  # Return the test result
  return(result)
}

