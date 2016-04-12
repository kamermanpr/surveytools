#' Calculate the sample size of a survey.
#'
#' \code{survey_size} calculates the sample size of a survey based on user
#' imputs of margin of error, confidence level, response distribution, and
#' population size.
#'
#' @param error_margin The amount of error that can be tolerated, where the
#'   margin of error is how close the calculated value is to the “true
#'   value” for the population. The smaller the margin of error, the closer
#'   the calculated value will be to the "true value" at the specified
#'   confidence level. Defaults to 0.05 (5\%).
#' @param conf_level The amount of uncertainty that can be tolerated, where
#'   the confidence level is the certainty with which the sample accurately
#'   reflects the population, within the specified margin of error.
#'   Defaults to 0.95 (95\%).
#' @param response_distr The expected distribution of responses to a binary
#'   question. Defaults to 0.5: either answer is equally likely to be
#'   chosen.
#' @param population_size Estimated size of the population being surveyed.
#'   Defaults to 20,000
#'
#' @return None
#'
#' @keywords sample size
#'
#' @export
#'
#' @examples
#' # Calculate sample size using the defaults
#' survey_size() # sample size = 377
#'
#' # Calculate the sample size required for a survey of a group of
#' # specialist physicians (population = 450), with a margin of error of
#' # 10%, and a confidence level of 99%. Assume the response distribution
#' # is 50%.
#' survey_size(error_margin = 0.1, conf_level = 0.99, population_size = 450) # sample size = 121

survey_size <- function(error_margin = 0.05,
                        conf_level = 0.95,
                        response_distr = 0.5,
                        population_size = 20000) {
    z <- qnorm(1 - ((1 - conf_level) / 2)) # critical z-score for confidence level
    e <- error_margin
    r <- response_distr
    N <- population_size

    x <- z^2 * r * (1- r)
    numerator <- N * x
    denominator <- (N - 1) * e^2 + x
    sample_size <- round(numerator / denominator)
    Values <- c(100 * e,
               100 * conf_level,
               100 * r,
               N,
               sample_size)
    Variable <- c('Margin of error (%)',
               'Confidence level (%)',
               'Response distribution (%)',
               'Population size (count)',
               'Survey sample (count)')
    tab.3 <- as.table(cbind(Variable, Values))
   print(tab.3)
}
