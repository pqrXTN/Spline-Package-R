#' @title Find boundary of data
#'
#' @description Find boundary of independent varibles, return index of these boundary.
#'
#'   E.g., when x is \code{c(0, 0.5, 0.2, 0.1)}, the function would return \code{c(1, 2)}.
#'
#' @param x A vector, matrix or data.frame of independent varibles.
#'
#' @return A vector of index of boundary cases.
#'
#' @examples
#' # find indices of bounary:
#' ## 1) of a vector:
#' find.boundary(x = c(0, 0.5, 0.2, 0.1))
#'
#' ## 2) of a matrix:
#' find.boundary(x = matrix (1:10, nrow=5, ncol=2))
#'
#'
find.boundary <- function(x){

  # if x is a matrix of data.frame, find index of maximum and minimum of every column
  # in case of repetition, union these indices into a set.
  if((class(x) == "matrix") | (class(x) == "data.frame")){
    x.dim <- ncol(x)
    maxrow.index <- apply(x.m, 2, which.max)
    minrow.index <- apply(x.m, 2, which.min)

    boundary.index <- union(maxrow.index, minrow.index)
  }else{
    boundary.index <- union(which.max(x), which.min(x))
  }

  return(boundary.index)
}
