#' @title Generate the all functions in Donoho and Johnstone(1994)
#'
#' @description generate 4 kinds of responsive varible based on \code{x}:
#'   "blocks", "bumps", "heavi", "dopper".
#'   We use the return values for function:\code{\link{gen.simu.data}}.
#'
#' @param x      A vector of independent varible.
#' @param signal A number about amplitude in response value in "DJ" method.
#' @param rsnr   A number of relative signal to noise ration.
#'   The standard deviation of noise is \eqn{signal / rsnr}.
#'   If \code{noisy == FALSE}, \code{rsnr} does not work.
#' @param noisy  A boolean whether add noise to model value.
#' @param plotfn A boolean whether plot 4 plots of data.
#'
#' @return       A list of 4 vectors of response varibles named "blocks", "bumps", "heavi", "dopper".
#'
#' @examples
#' # genertae data based on x. Amplitude is 7, do not add noise.
#' x <- runif(100, 0, 1)
#' data.list <- DJ.EX.rand(x, signal = 7, nosiy = FALSE)
#'
#' @import stats
#'
DJ.EX.rand <- function (x, signal = 7, rsnr = 7, noisy = FALSE, plotfn = FALSE)
{

  n <- length(x)

  # Generate 4 signals based on x.
  ## 1. "blocks"
  t <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76,
         0.78, 0.81)
  h1 <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
  blocks <- rep(0, n)
  for (i in seq(1, length(h1))) {
    blocks <- blocks + (h1[i] * (1 + sign(x - t[i])))/2
  }

  ## 2. "bumps"
  h2 <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
  w <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01,
         0.005, 0.008, 0.005)
  bumps <- rep(0, n)
  for (i in seq(1, length(h2))) {
    bumps <- bumps + h2[i] * pmax(0, (1 - abs((x - t[i])/w[i])))^4
  }

  ## 3. "heavi"
  heavi <- 4 * sin(4 * pi * x) - sign(x - 0.3) - sign(0.72 - x)

  ## 4. "doppler"
  eps <- 0.05
  doppler <- sqrt(x * (1 - x)) * sin((2 * pi * (1 - eps))/(x + eps))

  # standardize each signal and multiply amplitude.
  blocks <- blocks/sqrt(var(blocks)) * signal
  bumps <- bumps/sqrt(var(bumps)) * signal
  heavi <- heavi/sqrt(var(heavi)) * signal
  doppler <- doppler/sqrt(var(doppler)) * signal

  # if required, add Gaussian noise.
  if (noisy == TRUE){
    values <- list(blocks = blocks + rnorm(n, 0, signal/rsnr),
                   bumps = bumps + rnorm(n, 0, signal/rsnr),
                   heavi = heavi + rnorm(n, 0, signal/rsnr),
                   doppler = doppler + rnorm(n, 0, signal/rsnr))
  }else{
    values <- list(blocks = blocks, bumps = bumps, heavi = heavi,
                   doppler = doppler)
  }

  # plot 4 signals in one figure if required.
  if(plotfn == TRUE){
    par(mfrow = c(2, 2))
    plot(x, values$blocks, type = "l", ylab = "(a) Blocks")
    plot(x, values$bumps, type = "l", ylab = "(b) Bumps")
    plot(x, values$heavi, type = "l", ylab = "(c) HeaviSine")
    plot(x, values$doppler, type = "l", ylab = "(d) Doppler")
  }


  return(values)
}
