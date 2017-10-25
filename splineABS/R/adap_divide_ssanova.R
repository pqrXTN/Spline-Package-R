#' @title Do adaptive sampling smoothing spline with method of "divide and conquer".
#'
#' @description  Return a list of x and y value generating by 1D FanGijbels function.
#'
#' @param susetNumber  A number of subsets. (Denote as m).
#' @param ratio         A vector of the ratio of size of each subset. Default is equal.
#' @param moreSampling A boolean whether sample more basis function in adaptive sampling.
#'                     If \code{TRUE}, nbasis = max(30*m, 10*m*(N/m)^(2/9)).
#' @param pred         A boolean whether return predition value or list of models.
#' @param predAverage  A boolean whether predition an average of all subsets models.
#'                     If \code{FALSE}, do not avearage, return the each predition.
#' @param calculateTime A boolean whether calculate time of fitting model.
#' @inheritParams adap.ssanova
#'
#' @return       A list contains one or two items as below (depends on parameters):
#' \itemize{
#'   \item A number of fitting time of all subset models.
#'   \item An N*1 vector of preditions of average model (the average value of all subset models.)
#'   \item An N*m matrix of preditions of each subset model.
#'   \item An list of m fitted models on each subset.
#' }
#'
#' @import stats
#' @export
#'
adap.divide.ssanova <- function(x, y, subsetNumber = 1, ratio = NULL, alpha=NULL, nbasis=NULL,
                                nslice=10, sliceMethod=NULL,
                                splineFormula = NULL, moreSample = TRUE, pred = TRUE,
                                predAverage = TRUE, calculateTime = FALSE){

  if(moreSample == TRUE) {
    samples.index <- adap.sample(x, y, nbasis=nbasis, nslice=nslice,
                                 sliceMethod=sliceMethod, subsetNumber )
  }else{
    samples.index <- adap.sample(x, y, nbasis=nbasis, nslice=nslice, sliceMethod=sliceMethod)
  }


  # In case the x is a matirx or data.frame(>=2D) instead of a vector(1D)
  # generate a string of spline formula and a data.frame containing x and y
  #
  ## x.dim is the number of columns of matrix x
  ## xy.df is a data.frame combined x and y, renamed cols as x1, x2, ..., y
  ## spline.formula is like "y ~ x1 * x2 * ... * xn"
  formula.and.df <- gen.formula.and.df(x, y)
  x.dim <- formula.and.df$x.dim
  xy.df <- formula.and.df$xy.df
  if(is.null(splineFormula)){
    spline.formula <- formula.and.df$spline.formula
  }
  rm(formula.and.df)

  # get index of elements in each subset and the sampling baisi in each subset
  subset.sampleAndIndex <- generate.subset(length(y), samples.index, subsetNumber, ratio = ratio)

  subset.index <- subset.sampleAndIndex[[1]]
  subset.sample.index <- subset.sampleAndIndex[[2]]


  # if calculateTime == FALSE, print the value instead of return it.
  if(calculateTime == FALSE){
    cat("The number of samples ", length(samples.index), "\n")
  }


  rm(subset.sampleAndIndex)

  # fit model in each subset and get predict value
  # do fit model by ssanova in package: "gss"
  fitModel <- list()
  est <- matrix(0, nrow=length(y), ncol=subsetNumber)
  t0 <- proc.time()

  for(i in 1:subsetNumber){

    if(is.null(alpha)){
      fitModel[[i]] <- gss::ssanova(spline.formula, data = xy.df,
                                    id.basis = subset.sample.index[[i]])
    }else{
      fitModel[[i]] <- gss::ssanova(spline.formula, data = xy.df,
                                    id.basis = subset.sample.index[[i]], alpha=alpha)
    }
  }

  ## record time for fitting model
  fitTime <- proc.time()[3] - t0[3]
  # cat("time spent on fitting model: ", fitTime, '\n')

  # if there is a need for prediction,
  # predict the average of fitted value of all subset models
  # if "predSubsetModel == TRUE",
  if(pred == TRUE){
    for(i in 1:subsetNumber){
      est[,i] <- predict(fitModel[[i]],  newdata = xy.df[,1:x.dim, drop=FALSE],
                         se.fit = TRUE)$fit
      est.ave <- apply(est, 1, mean)
    }
    if(predAverage == TRUE){
      if(calculateTime == TRUE){
        return(list(est.ave=est.ave, fitTime=fitTime))
      }else{
        return(est.ave)
      }

    }else{
      if(calculateTime == TRUE){
        return(list(est=est, fitTime=fitTime))
      }else{
        return(est)
      }
      return(est)
    }

  }else{
    return(fitModel)
  }
}
