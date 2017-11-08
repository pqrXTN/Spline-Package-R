#' @title Adaptive sampling
#'
#' @description  Do adaptive sampling depending on y value
#'
#' @param y       A vecotr of response value.
#' @param nbasis  A number of approximate sample size. If it is \code{NULL},
#'                the number would be \code{max(30, 10*(n)^(2/9))}.
#' @param nslice  A number of slices numbers. If \code{sliceMethod} is not
#'                \code{NULL}, it will be invaid.
#' @param sliceMethod A string of slice method among "Scott", "Sturges",
#'                    "FD"(Freddman and Diaconis).
#'                    The default is \code{NULL} (NULL is not a string).
#'                    The number of slices depends on the method in function:
#'                    \code{\link[stats]{hist}}.
#'
#' @return A vector of index of selected samples in \code{y}.
#'
#' @examples
#' # do adaptive sampling based on the value of y.
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
#' Get the boundary of x data. \code{\link{find.boundary}}.
#'
adap.sample <- function(y, nbasis=NULL, nslice=10, sliceMethod=NULL){

  # initiate a set of sampling index
  # sampling set initially includes the minimum and maximum
  nobs <- length(y)

  # try not to find bounds becasue it is not plausible.
  # sample.index <- c(find.boundary(x), which.min(y), which.max(y))
  sample.index <- c(which.min(y), which.max(y))

  # decide the sampling size and break point of each slice.
  # it depends on `sliceMethod` and `nslice``
  if(!is.null(sliceMethod)){
    y.slice <- hist(y, breaks = sliceMethod, plot = FALSE)$breaks
    nslice  <- length(y.slice) - 1
  }else{
    nslice  <- nslice
    y.slice <- c(seq(min(y), max(y), length = nslice+1))
  }

  # decide numbers of obsevation in each slice
  if(is.null(nbasis)){
    nbasis <- max(30, ceiling(10 * (nobs)^(2/9)))
  }



  # sample in each slice
  ## record the index of slice which has less elemets than required samples.

  residue <- nbasis - 2
  slice.index <- list()

  for(i in 1:(nslice)){

    # a rough operation garantee that the last slice includes the upper bounder
    if(i < nslice){
      slice.index[[i]] <- which(y.slice[i] <= y & y <  y.slice[i+1])
    }else{
      slice.index[[i]] <- which(y.slice[i] <= y & y <= y.slice[i+1])
    }
  }

  # record which slice has availble space for new sampling
  large.slice <- rep(TRUE, nslice)


  # deal with min and max
  slice.index[[1]] <- setdiff(slice.index[[1]], which.min(y))
  slice.index[[nslice]] <- setdiff(slice.index[[nslice]], which.max(y))
  if(length(slice.index[[1]]) ==0){
    large.slice[1] <- FALSE
  }else if(length(slice.index[[nslice]]) ==0){
    large.slice[nslice] <- FALSE
  }

  # record which slice has availble space for new sampling
  large.slice.index <- c(1:nslice)[large.slice]

  iter <- 1

  # conduct iteration
  # if could not converge in 10 iterations, stop
  while(residue > 0 && iter <= 10){

    nslice.new <- sum(large.slice)

    # set the number of basis in each slice as even as possible
    # if indivisible, choose some "luck" slice to get extra one base.
    ## use function allocateToSlice in this package
    nbasis.slice <- allocateToSlice(nelement = residue, nslice = nslice.new)

    for(i in large.slice.index){
      # If the size of a slice is less than nbasis.slice, we take all members in
      # rather than sampling.
      if(length(slice.index[[i]]) >= nbasis.slice[i]){
        selected.index <- sample(slice.index[[i]], nbasis.slice[i])

      }else{
        selected.index <- slice.index[[i]]
        large.slice[i] <- FALSE
      }

      # record remain space in each slice;
      #  add select.index into whole samples
      slice.index[[i]] <- setdiff(slice.index[[i]], selected.index)
      sample.index <- union(sample.index, selected.index)
    }

    # update varibles in iteration
    residue <- nbasis - length(sample.index)
    large.slice.index <- c(1:nslice)[large.slice]
    iter <- iter + 1
  }


  sample.index <- sort(sample.index)

  return(sample.index)
}
