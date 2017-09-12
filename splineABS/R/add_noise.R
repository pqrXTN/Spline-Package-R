#' @title Add Gaussian noise
#'
#' @description  Add Guassian noise to response variable based on SNR(signal to noise ratio).
#'   \eqn{y = v + e}.
#'   This is an inner function, not exposed outside.
#'
#' @param v    A vector of response value without noise.
#' @param SNR  A number of "signal to noise" ratio: the ratio of standard deviation of signal to noise.
#' @param seed A number of random seed. Default is NULL, do not set seed.
#'
#' @return A vector of response varible with noise. \eqn{y = v + e}.
#'
#' @import stats
#'
add.noise <- function(v, SNR = 7, seed = NULL){

  # set seed for random numbers if required
  if(!is.null(seed)){
    set.seed(seed + 200)
  }

  # calculate standard deviation of signals
  nobs <- length(v)
  ssig <- sd(v)

  # generate noise based on sd of sinal and SNR
  e <- rnorm(nobs, 0, ssig/SNR)
  y <- v + e

  return(y)
}
