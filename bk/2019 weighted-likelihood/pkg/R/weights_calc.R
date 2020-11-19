#' Calculate model weights from population numbers for weighted-likelihood model
#'
#' @param numerator1 Vector of population sizes to use as weights
#' @param denominator Denominator to scale weights
#' @param numerator2 Vector of population sizes for unweighted (random) samples to combine with weighted samples (numerator1)
#' @param inverse Logical indicating if inverse weights should be returned
#'
#' @return Vector of weights
#'
#' @export

weights_calc <- function(numerator1, denominator, numerator2=NULL, inverse=F){

  weights <- numerator1 / denominator

  if(inverse){

    weights <- 1 / weights

    if(!is.null(numerator2)) {
      n2 <- length(numerator2)
      weights <- c(weights, rep( sum(weights) / n2, n2 ) )
    }

  }
  weights <- weights / sum(weights)
  return(weights)
}
