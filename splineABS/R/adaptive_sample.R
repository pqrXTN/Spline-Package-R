#' @title Adaptive sampling
#'
#' @description  Do adaptive sampling depending on y value
#'
#' @param y       A vecotr of response value.
#' @param nbasis  A number of approximate sample size. If it is \code{NULL}, the number would be
#'                \code{max(30, 10*n^(2/9))}
#' @param nslice  A number of slices numbers. If \code{sliceMethod} is not \code{NULL}, it will be invaid.
#' @param sliceMethod A string of slice method among "Scott", "Sturges", "FD"(Freddman and Diaconis).
#'                    The default is \code{NULL} (NULL is not a string).
#'                    The number of slices depends on the method in function: \code{\link[stats]{hist}}.
#'
#' @return A vector of index of selected samples in \code{y}.
#'
#' @examples
#' # do adaptive sampling based on the value of y
#' ## number of samples is default as max(30, 10*n^(2/9))
#' ## slices are divided by rule of "Scoot" of hist().
#' adap.sample(y=1:100, sliceMethod="Scott")
#'
#' @import stats
#' @export
#'
#' @seealso
#' The methods of divided slices in \code{\link[stats]{hist}}.
#'
adap.sample <- function(y, nbasis=NULL, nslice=11, sliceMethod=NULL){

  # initiate a set of sampling index, it automatically includes the minimum and maximum.
  nobs <- length(y)
  sample.index <- c(which.min(y), which.max(y))

  # decide the sampling size and break point of each slice.
  # it depends on `sliceMethod` and `nslice``
  if(!is.null(sliceMethod)){
    y.slice <- hist(y, breaks = sliceMethod, plot = FALSE)$breaks
    nslice <- length(y.slice)-1
  }else{
    nslice <- nslice
    y.slice <- c(seq(min(y), max(y), length = nslice+1))
  }

  # decide numbers of obsevation in each slice
  if(is.null(nbasis)){
    nbasis <- max(30, ceiling(10 * nobs^(2/9)))
  }

  nobs.slice <- ceiling(nbasis/nslice)

  # sample in each slice (exclude the maximum since it is inculded initially)
  for(i in 1:(nslice)){

    # a rough operation garantee that the last slice includes the upper bounder
    if(i<nslice){
      slice.index <- which(y.slice[i] <= y & y <  y.slice[i+1])
    }else{
      slice.index <- which(y.slice[i] <= y & y <= y.slice[i+1])
    }


    # If the size of a slice is less than nobs.slice, we take all members in rather than sampling.
    if(length(slice.index) >= nobs.slice){
      sample.index <- union(sample.index, sample(slice.index, nobs.slice))
    }else{
      sample.index <- union(sample.index, slice.index)
    }
  }

  sample.index <- sort(sample.index)

  return(sample.index)
}
