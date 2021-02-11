#' simulator
#'
#' @param numer integer length of distribution
#' @param bins integer size of bins for histogram
#'
#' @return a histigram showing CI at the 95% confidence level
#' @examples
#' simulator()
#' @import infer
#' @importFrom dplyr tibble
#' @importFrom stats cor rbeta
#' @export
simulator <- function (numer=25, bins=50){
set.seed(123)
transcript <- tibble(
    name   = sample(letters,numer),
    cor   = rbeta(numer,1,3))

cor_mean <- transcript %>%
    specify(response = cor) %>%
    calculate(stat = "mean")

null_distn <- transcript %>%
    specify(response = cor) %>%
    hypothesize(null = "point", mu = 0.5) %>%
    generate(reps = 1000, type = "bootstrap") %>%
    calculate(stat = "mean")

percentile_ci <- get_confidence_interval(null_distn, level = 0.95)

visualize(null_distn,bins = bins) +
    shade_confidence_interval(endpoints = percentile_ci)

}


