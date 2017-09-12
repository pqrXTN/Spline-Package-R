#' @title Plot fitted model
#'
#' @description
#' \itemize{
#'   \item Plot in one graph of original data without noise(if there is), data with noise, fitted data.
#'   \item When fitted model is given, generate 10 times unif points to plot a smoothing curve.
#'   \item \strong{Attetion}: x and y shoule be vector(Just plot a 2D figure).
#'     The varible name in model must exact be "x" and "y", or it would be error in \code{predict}.
#'     (generate 10 times points by \code{predict} to plot a smoothing curve)
#' }
#'
#'
#' @param x           A vector (or a matrix) of independent varibles. (But now, we just realize vector part)
#' @param y           A vector of response varibles with noise.
#' @param v           A vector of response varibles of true value (without noise).
#' @param sampleIndex A vector of index of selected sample.
#' @param fitModel    A list of fitted model of \code{gss::ssanova}.
#'                    (result of other fitting function like \code{lm} is also acceptable).
#' @param figTitle       A string of figure' s title.
#'
#' @return NULL
#'
#' @examples
#' # do linear regression of y~x, and plot the scatter with fitted line on one figure.
#' # all x and y are selected into regression, so sampleIndex is a vector of all indices.
#' x <- 1:10
#' y <- 3 * x + rnorm(10,0,1)
#' lm.model <- lm(y ~ x)
#' fitplot(x = x, y= y, sampleIndex = 1:10, fitModel = lm.model,
#'         figTitle = "an example of lm")
#'
#' @import stats
#' @export
#'
fit.plot <- function(x, y, v=NULL, sampleIndex=NULL, fitModel=NULL, figTitle="Figure"){

  # plot a scatter of (x, y).
  plot(x, y, pch=4, col="grey", main = figTitle)

  # plot signal on the figure
  if(!is.null(v)){
    lines(x, v, lwd=2)
  }

  # plot a smooth curve based on result of "predict"
  if(!is.null(fitModel)){

    # prepare for a smmothing curve
    ## add 10 times points to for plot
    nPlot <- 10 * length(x)
    xPlot <- seq(min(x), max(x), length=nPlot)
    yPlot <- predict(fitModel,  newdata = data.frame(x=xPlot), se.fit = TRUE)$fit

    # add points & smmothing curve.
    # add marks of samples on x-axis
    lines(xPlot, yPlot, lwd=2)
    rug(x[sampleIndex])
  }
}
