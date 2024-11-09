
##' Demonstrate the Central Limit Theorem
##'
##' The `demo_clt()` function generates plots to illustrate the
##' Central Limit Theorem (CLT) using a specified random number
##' generator. The function displays the sampling distributions
##' for different sample sizes, standardized and compared against
##' the standard normal distribution.
##'
##' @param rng A random number generator function (e.g., `runif`,
##' `rnorm`, `rgamma`).
##' @param n A numeric vector of sample sizes (e.g., `c(5, 10,
##' 20, 40)`).
##' @param nrep The number of repetitions for generating sample
##' means (default is 10000).
##' @param ... Additional arguments passed to the random number
##' generator (e.g., `shape` and `rate` for `rgamma`).
##' @param mean The theoretical mean of the distribution. If not
##' provided, it is estimated from a large sample.
##' @param sd The theoretical standard deviation of the
##' distribution. If not provided, it is estimated from a large
##' sample.
##'
##' @return A `ggplot2` plot showing the standardized sampling
##' distributions for different sample sizes, compared against
##' the standard normal curve.
##' @examples
##' set.seed(123)
##' demo_clt(runif, n = c(5, 10, 20, 40), nrep = 10000, min = 0,
##' max = 1)
##' demo_clt(rgamma, n = c(5, 10, 20, 40), nrep = 10000, shape = 2,
##' rate = 1, mean = 2, sd = sqrt(2))
##'
##' @importFrom ggplot2 ggplot geom_histogram geom_line aes
##' @importFrom ggplot2 facet_wrap labs theme_minimal after_stat
##' @export
demo_clt <- function(rng, n, nrep = 10000, ..., mean = NULL,
                     sd = NULL) {
  ## Validate that 'rng' is a function
  if (!is.function(rng)) {
    stop("The argument 'rng' must be a function.")
  }

  ## Validate that 'n' is a numeric vector of positive values
  if (!is.numeric(n) || any(n <= 0)) {
    stop("The argument 'n' must be a numeric vector of positive values.")
  }

  ## Validate that 'nrep' is a positive integer
  if (!is.numeric(nrep) || nrep <= 0) {
    stop("The argument 'nrep' must be a positive integer.")
  }

  ## Estimate the theoretical mean and standard deviation if not provided
  if (is.null(mean) || is.null(sd)) {
    sample_data <- rng(100000, ...)
    mean <- mean(sample_data)
    sd <- sd(sample_data)
  }

  ## Helper function to generate random samples
  generate_sample <- function(size) {
    rng(size, ...)
  }

  ## Initialize an empty list to store standardized sample means
  results <- list()

  ## Loop through each sample size and generate standardized sample means
  for (size in n) {
    ## Generate 'nrep' sample means for the current sample size
    sample_means <- replicate(nrep, mean(generate_sample(size)))

    ## Standardize the sample means using the theoretical mean and standard deviation
    standardized_means <- (sample_means - mean) / (sd / sqrt(size))

    ## Store the standardized means in a data frame
    results[[as.character(size)]] <- data.frame(
      StandardizedMean = standardized_means,
      SampleSize = factor(size)
    )
  }

  ## Combine all results into a single data frame
  data <- do.call(rbind, results)

  ## Create data for the standard normal curve overlay
  x_vals <- seq(-4, 4, length.out = 100)
  normal_data <- data.frame(
    x = x_vals,
    y = dnorm(x_vals)
  )

  ## Create the ggplot object
  plot <- ggplot2::ggplot(data, aes(x = StandardizedMean)) +
    ## Plot the histogram in density mode
    ggplot2::geom_histogram(aes(y = after_stat(density)),
                            bins = 30, color = "black",
                            fill = "skyblue") +
    ## Overlay the standard normal density curve
    ggplot2::geom_line(data = normal_data,
                       aes(x = x, y = y),
                       color = "red",
                       linetype = "dashed",
                       linewidth = 0.8) +
    ## Facet the plot by sample size
    ggplot2::facet_wrap(~ SampleSize) +
    ## Add labels and theme
    ggplot2::labs(
      title = "Demonstrating the Central Limit Theorem",
      x = "Standardized Sample Mean",
      y = "Density"
    ) +
    ggplot2::theme_minimal()

  ## Return the ggplot object
  return(plot)
}
