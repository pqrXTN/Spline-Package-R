#' @title Generate 1D Fan-Gijbels simulation data
#'
#' @description  Return a list of x and y value generating by 1D FanGijbels
#'   function.
#'
#' @details
#'   In \code{genMethod}: "DJ" stands for functions in Donoho and
#'   Johnstone(1994).
#'
#'   The return value of function is a list of \code{x, y, z}.
#' \itemize{
#'   \item x: independent varible.
#'   \item y: response varible with noise.
#'   \item v: the true value of response varible without noise.
#' }
#'
#' @param nobs   A number of observations in simulation.
#' @param genMethod A string of data generating method among "1dfan",
#'   "DJ.blocks", "DJ.bumps", "DJ.heavi", "DJ.doppler".
#' @param SNR    A number of "signal to noise" ratio: the ratio of standard
#'   deviation of signal to noise.
#' @param signal A number about amplitude in response value of "DJ" method.
#' @param seed   A number of base of random seed.
#'
#'
#' @return       A list contains generated \code{x, y, v}.
#'
#' @examples
#' # generate data of "DJ.blocks".
#' ## numbers of observation: 1024, signal to noise ratio is 7.
#' data.xyv <- gen.simu.data(nobs = 1024, genMethod = "DJ.blocks", SNR = 7)
#'
#' @seealso
#'   4 kinds of functions of in Donoho and Johnstone(1994) in function:
#'   \code{\link{DJ.EX.rand}}.
#'   Add Guassian noise to response varible in function:
#'   \code{\link{add.noise}}.
#' @import stats
#' @export
#'
gen.simu.data <- function(nobs = 1024, genMethod = "1dfan", SNR = 7, signal = 7,
                          seed = NULL){

  # if not test, set random seed.
  if(is.null(seed) == FALSE){
    set.seed(seed)
  }

  # generate independent varible: x
  # generate original dependent varible: v (without noise)
  if(genMethod == "1dfan"){
    x <- c(0,sort(runif(nobs-2)),1)
    v <- sin(2*(4*x-2))+2*exp(-16*(4*x-2)^2)
  }else if(genMethod == "DJ.blocks"){
    x <- c(0,sort(runif(nobs-2)),1)
    v <- DJ.EX.rand(x = x, signal = signal)$blocks
  }else if(genMethod == "DJ.bumps"){
    x <- c(0,sort(runif(nobs-2)),1)
    v <- DJ.EX.rand(x = x, signal = signal)$bumps
  }else if(genMethod == "DJ.heavi"){
    x <- c(0,sort(runif(nobs-2)),1)
    v <- DJ.EX.rand(x = x, signal = signal)$heavi
  }else if(genMethod == "DJ.doppler"){
    x <- c(0,sort(rbeta(nobs-2,2,2)),1)
    v <- DJ.EX.rand(x = x, signal = signal)$doppler
  }

  # generate y: add Gussian noise into signal, y = v + err
  if(is.null(seed) == TRUE){
    y <- add.noise(v, SNR = SNR, seed = 123)
  }else{
    y <- add.noise(v, SNR = SNR, seed = seed)
  }

  # return x, y(withnoise), v(without noise)
  return(list(x=x, y=y, v=v))
}
