#' @title Do adaptive sampling smoothing spline with method of "divide and conquer".
#'
#' @description  Return a list of x and y value generating by 1D FanGijbels function.
#'
#' @param susetNumber  A number of subsets. (Denote as m).
#' @param subsetElementIndex A list of indices of each subset's element in whole data.
#'                     Default = NULL, randomly generate subsets.
#' @param ratio        A vector of the ratio of size of each subset. Default is equal.
#' @param pred         A boolean whether return predition value or list of models.
#' @param shareBasis   A boolean whether each subset shares the same basis.
#'                     Default = TRUE: all subsets uses the basis in whole data;
#'                     if FALSE: all subsets do adaptive or uniform sampling
#'                     themselves.
#' @param uniformSampling A boolean whether use uniform sampling basis in each
#'                        subset. If FALSE, use adptive sampling.
#'                        Require \code{subsetElementIndex != NULL}.
#' @inheritParams adap.ssanova
#'
#' @return       A list:
#' \itemize{
#'   \item (all)A number of fitting time of all subset models.
#'   \item (pred = TRUE)An N*1 vector of preditions of average model
#'     (the average value of all subset models.)
#'   \item (pred = TRUE)An N*m matrix of preditions of each subset model.
#'   \item (pred = FALSE)An list of m fitted models on each subset.
#'   \item subsetElementIndex: the indices of each subset's elements in whole data
#'   \item basisIndexInSubset: the indices of basis in each subset
#' }
#'
#' @import stats
#' @export
#'
adap.divide.ssanova <- function(x, y, subsetNumber = 1, subsetElementIndex = NULL,
                                ratio = NULL, alpha = NULL, nbasis=NULL,
                                nslice = 10, sliceMethod = NULL,
                                splineFormula = NULL,  pred = TRUE,
                                shareBasis = TRUE, uniformSampling = FALSE){

  # If x is a matirx or data.frame(>=2D) instead of a vector(1D)
  # generate a string of spline formula and a data.frame containing x and y
  ##
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

  # fit model in each subset and get predictted value
  fitModel <- list()
  est <- matrix(0, nrow=length(y), ncol=subsetNumber)
  fitTime <- 0

  # distributed computing based on the same basis
  if(shareBasis == TRUE){

    # generate random ABS basis on global data
    basisIndex <- adap.sample(y = y, nbasis=nbasis, nslice=nslice,
                              sliceMethod=sliceMethod)
    nbasis <- length(basisIndex)
    # if we need uniform sampling. Re-sample basis of equal amount.
    if(uniformSampling == TRUE){
      basisIndex <- sample(1:length(y), nbasis)
    }


    # get index of elements in each subset and the sampling basis in each subset.
    # if subsetElementIndex != NULL, firstly generate random subsets according to ratio.
    # if not, subsets are based on param 'subsetElementIndex'
    # copy basis into each subsets to garantee that each subsets shares the same basis.
    subsetElementAndBasis <- generate.subset(length(y), basisIndex, subsetNumber,
                                             ratio = ratio,
                                             subset.element.index = subsetElementIndex)
    subsetElementIndex <- subsetElementAndBasis[[1]]
    basisIndexInSubset <- subsetElementAndBasis[[2]]
    rm(subsetElementAndBasis)

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

  }else{
  # distributed computing based on the different basis

    basisIndexInSubset <- list()

    # compared to sharing global basis, now generatelocal basis in each
    #  'for loop'
    for(i in 1:subsetNumber){

      x <- xy.df[subsetElementIndex[[i]], ]$x
      y <- xy.df[subsetElementIndex[[i]], ]$y

      # get adaptive sampling inner subset
      # i.e do not share the same basis
      if(uniformSampling == FALSE){
        # adaptive sampling
        localBasisIndex <- adap.sample(y = y, nbasis=nbasis, nslice=nslice,
                                       sliceMethod=sliceMethod)
      }else{
        # uniform sampling
        localBasisIndex <- sort(sample(1:length(y), nbasis))
      }

      # record basisIndex in each subset
      basisIndexInSubset[[i]] <- localBasisIndex

      # record time to start fitting model
      t0 <- proc.time()

      # do smoothing spline
      if(is.null(alpha)){
        fitModel[[i]] <- gss::ssanova(spline.formula,
                                      data = xy.df[subsetElementIndex[[i]], ],
                                      id.basis = localBasisIndex)
      }else{
        fitModel[[i]] <- gss::ssanova(spline.formula,
                                      data = xy.df[subsetElementIndex[[i]], ],
                                      id.basis = localBasisIndex,
                                      alpha=alpha)
      }
      # record time for each model
      fitTime <- fitTime + proc.time()[3] - t0[3]

    }
  }


  # if there is a need for prediction,
  # predict the average of fitted value of all subset models
  # if "predSubsetModel == TRUE",
  if(pred == TRUE){
    for(i in 1:subsetNumber){
      est[,i] <- predict(fitModel[[i]],  newdata = xy.df[ ,1:x.dim, drop=FALSE],
                         se.fit = TRUE)$fit
      est.ave <- apply(est, 1, mean)
    }
    return(list(est=est, est.ave=est.ave, fitTime=fitTime,
                subsetElementIndex=subsetElementIndex,
                basisIndexInSubset=basisIndexInSubset))

  }else{
    # do not predict, just return a list of subset models.
    return(list(fitModel=fitModel, fitTime=fitTime,
                subsetElementIndex=subsetElementIndex,
                basisIndexInSubset=basisIndexInSubset))
  }

}
