#' @title  Compare mse and running time.
#'
#' @description
#'   Compare mse and running time of spline via Full, UBS and ABS method.
#'   If required, plot box.plot of MSE of all iteration or fited cure in last
#'   iteration.
#'
#' @details
#'   Run n times of spline model based on Full samples, unif sampling and
#'   adaptive sampling. Compare the mse, time consuming of each method;
#'   if need, plot a boxplot or plot 4 fitted plot.
#'
#'   The fitting of smoothing spline model with Gaussian noise is based on
#'   package \code{gss}.
#'
#'   If x is a matrix, generate a data.frame of x & y;
#'   generate string of formular for high demesion like(if \code{splineFormula
#'   == NULL}): \code{"y ~ x1 * x2 * ... * xn"}.
#'   Do not plot if x is a matrix, since we cannot plot when dimension is more
#'   than 2.
#'
#'   In parameter \code{splineFormula}, default is \code{NULL}.
#'   \itemize{
#'     \item If x is 1D, default is \code{"y~x"};
#'     \item If x is mD, default is \code{"y ~ x1*x2*... *xm"}.
#'     \item \code{"1 +x1 + x2"} stands for: c + f1(x1) + f2(x2);
#'     \item \code{"1 + x1 + x1:x2"} stands for: c + f1(x1) + f12(x1,x2);
#'     \item \code{"1 + x1*x2"} stands for: c + f1(x1) + f2(x2) + f12(x1,x2);
#'     \item etc.
#'   }
#'
#' @param x         A vector (or a matrix or data.frame) of independent varibles.
#' @param y         A vector of response varibles with noise
#' @param v         A vector of response varibles of true value (without noise).
#' @param alpha     A number of modifying minimizing GCV in \code{ssanova} of
#'                  package \code{[gss]{ssanova}}. alpha = 1 indicates using
#'                  unmodified cross validation to determine lambda.
#' @param splineFormula A string of formula of spline function. See more description
#'   in details.
#'
#' @inheritParams adap.sample
#' @inheritParams gen.simu.data
#'
#' @param times     An integer number of how many times we repeat sampling.
#' @param plotBox   A boolean of whether plot a box-plot of MSE.
#' @param calTime   A boolean of whether returns the average fitting time of
#'                  splines of each method.
#' @param seed      A number of random seed. Default is NULL.
#' @param printIter A boolean if print out the current iteration when testing.
#' @param plotFit   A boolean if plot the original data, noise data, and
#'                  fitted splines in last iteration.
#'
#'
#' @return A data.frame of MSE of three methods.
#'
#'         If required output time for fitting, return a list of a data.frame
#'         of MSE and a data.frame of consuming time.
#'
#' @examples
#' # use simulation data of "1dfan", get slice by "Scott" method.
#' # run 10 times in iteration.
#' compare.mse(genMethod = "1dfan", nobs = 1024, sliceMethod = "Scott",
#'             times = 10)
#'
#' @seealso \code{\link[gss]{ssanova}} for smoothing spline,
#'
#'          \code{\link{adap.sample}} for adaptive sampling,
#'
#'          \code{\link{gen.simu.data}} for generating simulation data,
#'
#'          \code{\link{fit.plot}} for plot fitted figures.
#'
#' @import stats gss
#' @export
#'
#'
compare.mse <- function(x=NULL, y=NULL, v=NULL, genMethod=NULL, signal=7,
                        nobs=1024, SNR=7, alpha= -1, nbasis=NULL, nslice=10,
                        sliceMethod=NULL, splineFormula = NULL,
                        times=1, plotBox=FALSE, calTime=FALSE, seed=NULL,
                        printIter=FALSE, plotFit=FALSE){

  # create a matrix of mse and a matirx of time
  mse.m <- matrix(data = NA, nrow = times, ncol = 3)
  colnames(mse.m) <- c('Full', 'UBS','ABS')
  time.m <- matrix(data = NA, nrow = times, ncol = 3)
  colnames(time.m) <- c('Full', 'UBS','ABS')

    seed <- seed

    for(iter in 1:times){

      # print the number of iteration when testing if required
      ## print 10 numbers on on line and then switch to another line
      if(printIter == TRUE){
        if(iter %% 10 == 1){
          if(iter == 1){
            cat("iter =  ", iter, "\t")
          }else{
            cat("\t", iter, "\t")
          }
        }else if(iter %% 10 == 0){
          cat(iter, "\n")
        }else{
          cat(iter, "\t")
        }
      }

      # set seeds for every iteration if required
      if(!is.null(seed)){
        seed <- seed + 1
        set.seed(seed)
      }

      # genearte data(genMethod != NULL) or use given data
      if(!is.null(genMethod)){
        data.xyv <- gen.simu.data(nobs = nobs, genMethod = genMethod,
                                  signal = signal, SNR = SNR, seed = seed)
        x <- data.xyv$x
        y <- data.xyv$y
        v <- data.xyv$v
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


      # get samples of ABS and UBS.
      ## UBS is based on the sample size of ABS, but sampling method is simple
      ## random sampling.
      sampleIndex.abs <- adap.sample(y=y, nbasis=nbasis, nslice=nslice,
                                     sliceMethod=sliceMethod)
      sampleIndex.ubs <- sample(1:length(y), length(sampleIndex.abs))

      # apply gss::ssanova to 1) full samples; 2)uniform samples; 3) adaptive
      #  samples
      ## record time for spline model
      t0 <- proc.time()
      model.full <- gss::ssanova0(spline.formula, data = xy.df)
      time.m[iter,1] <- proc.time()[3] - t0[3]

      t0 <- proc.time()
      model.ubs <- gss::ssanova(spline.formula, data = xy.df, id.basis =
                                  sampleIndex.ubs, alpha = alpha)
      time.m[iter,2] <- proc.time()[3] - t0[3]

      t0 <- proc.time()
      model.abs <- gss::ssanova(spline.formula, data = xy.df, id.basis =
                                  sampleIndex.abs, alpha = alpha)
      time.m[iter,3] <- proc.time()[3] - t0[3]


      # calcualte mse, use data.frame generated before.
      ## in case the data.frame degrades into vector, use parameter
      ## "drop == FALSE".
      est.full <- predict(model.full, newdata = xy.df[,1:x.dim, drop=FALSE],
                          se.fit = TRUE)
      est.ubs  <- predict(model.ubs,  newdata = xy.df[,1:x.dim, drop=FALSE],
                          se.fit = TRUE)
      est.abs  <- predict(model.abs,  newdata = xy.df[,1:x.dim, drop=FALSE],
                          se.fit = TRUE)

      mse.full <- mean((est.full$fit- v)^2)
      mse.ubs  <- mean((est.ubs$fit - v)^2)
      mse.abs  <- mean((est.abs$fit - v)^2)


      mse.m[iter,] <- c(mse.full, mse.ubs, mse.abs)

    }

    # convert matrix of mse into data.frame for convience
    mse.df  <- data.frame(mse.m)
    time.df <- data.frame(time.m)

    # plot a boxplox if necessary required
    if(plotBox == TRUE){
      boxplot(mse.df)
    }

    # if required, plot model in last iteration.
    ## plot only when x is a vector. i.e. we just plot 2D figure.
    if((plotFit == TRUE) & (x.dim == 1)){
      par(mfrow=c(2,2))
      titles <- c("Truth", "Full", "UBS", "ABS")

      fit.plot(x = x, y = y, v = v, figTitle = titles[1])
      fit.plot(x = x, y = y, v = NULL, sampleIndex = 1:length(x),
               fitModel=model.full, figTitle = titles[2])
      fit.plot(x = x, y = y, v = NULL, sampleIndex = sampleIndex.ubs,
               fitModel=model.ubs, figTitle = titles[3])
      fit.plot(x = x, y = y, v = NULL, sampleIndex = sampleIndex.abs,
               fitModel=model.abs, figTitle = titles[4])
    }

    # return a list including MSE matrix data.frame and time data.frame
    ## if calTime == FALSE, just return mse.datafram
    if(calTime == TRUE){
      return(list(MSE=mse.df, TIME=time.df))
    }else{
      return(mse.df)
    }

}
