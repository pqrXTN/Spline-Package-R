#' @title Do adaptive sampling smoothing spline with method of "divide and conquer".
#'
#' @description  Return a list of x and y value generating by 1D FanGijbels function.
#'
#' @param susetNumber  A number of subsets. (Denote as m).
#' @param subsetElementIndex A list of indices of each subset's element in whole data.
#'                     Default = NULL, randomly generate subsets.
#' @param ratio        A vector of the ratio of size of each subset. Default is equal.
#' @param pred         A boolean whether return predition value or list of models.
#' @param predAverage  A boolean whether predition an average of all subsets models.
#'                     If \code{FALSE}, do not avearage, return the each predition.
#' @inheritParams adap.ssanova
#'
#' @return       A list contains two items as below :
#' \itemize{
#'   \item (all)A number of fitting time of all subset models.
#'   \item (predAverage = TRUE)An N*1 vector of preditions of average model
#'     (the average value of all subset models.)
#'   \item (pred = TRUE)An N*m matrix of preditions of each subset model.
#'   \item (pred = FALSE)An list of m fitted models on each subset.
#' }
#'
#' @import stats
#' @export
#'
adap.divide.ssanova <- function(x, y, subsetNumber = 1, subsetElementIndex = NULL,
                                ratio = NULL, alpha = NULL, nbasis=NULL,
                                nslice = 10, sliceMethod = NULL,
                                splineFormula = NULL,  pred = TRUE,
                                predAverage = TRUE){

  # generate random ABS basis
  basisIndex <- adap.sample(x, y, nbasis=nbasis, nslice=nslice,
                                 sliceMethod=sliceMethod)

  # If x is a matirx or data.frame(>=2D) instead of a vector(1D)
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

  # get index of elements in each subset and the sampling basis in each subset.
  # if subsetElementIndex != NULL, firstly generate random subsets according to ratio.
  # copy basis into each subsets to garantee that each subsets shares the same basis.
  subsetElementAndBasis <- generate.subset(length(y), basisIndex, subsetNumber,
                                           ratio = ratio,
                                           subset.element.index = subsetElementIndex)
  subsetElementIndex <- subsetElementAndBasis[[1]]
  basisIndexInSubset <- subsetElementAndBasis[[2]]
  rm(subsetElementAndBasis)

  # fit model in each subset and get predict value
  # do fit model by ssanova in package: "gss"
  fitModel <- list()
  est <- matrix(0, nrow=length(y), ncol=subsetNumber)
  t0 <- proc.time()

  for(i in 1:subsetNumber){

    # fit each subset model with global basis(but local basis indices)
    # and local data
    if(is.null(alpha)){
      fitModel[[i]] <- gss::ssanova(spline.formula,
                                    data = xy.df[subsetElementIndex[[i]], ],
                                    id.basis = basisIndexInSubset[[i]])
    }else{
      fitModel[[i]] <- gss::ssanova(spline.formula,
                                    data = xy.df[subsetElementIndex[[i]], ],
                                    id.basis = basisIndexInSubset[[i]],
                                    alpha=alpha)
    }
  }

  # record time for fitting model
  fitTime <- proc.time()[3] - t0[3]

  # if there is a need for prediction,
  # predict the average of fitted value of all subset models
  # if "predSubsetModel == TRUE",
  if(pred == TRUE){
    for(i in 1:subsetNumber){
      est[,i] <- predict(fitModel[[i]],  newdata = xy.df[ ,1:x.dim, drop=FALSE],
                         se.fit = TRUE)$fit
      est.ave <- apply(est, 1, mean)
    }

    if(predAverage == TRUE){
      return(list(est.ave=est.ave, fitTime=fitTime))
    }else{
      return(list(est=est, fitTime=fitTime))
    }

  # do not predict, just return a list of subset models.
  }else{
    return(list(fitModel=fitModel, fitTime=fitTime))
  }
}
