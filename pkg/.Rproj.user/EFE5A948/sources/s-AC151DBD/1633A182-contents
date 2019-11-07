#' Convert an mcmc list to a data frame
#'
#' @param x MCMC list object
#'
#' @return Data frame with a column for each parameter and a row for each mcmc sample
#'
#' @export

mcmc_to_df <- function(x){
  d <- as.data.frame(x[[1]])
  for(i in 2:length(x)){
    d <- rbind(d, x[[i]])
  }
  return(d)
}
