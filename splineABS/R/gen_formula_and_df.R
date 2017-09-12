#' @title Generate data.frame and formula for high dimension spline
#'
#' @description In case x is a matrix or data.frame, generate a data.frame of x & y;
#'   The columns of new data.frame are named as "x1", "x2", ... "xn", "y".
#'   generate string of formular for high demesion like: \code{"y ~ x1 * x2 * ... * xn"}.
#'
#' @param x A vector or matrix or data.frame of independent varibles.
#' @param y A vector of response varible.
#'
#' @return A list of 1) a data.frame including x and y; 2) a string of spline formula;
#'    3) a number of colume rank of x.
#'
#'    \code{list(xy.df = xy.df, spline.formula = spline.formula, x.dim = x.dim)}.
#'
#' @examples
#'  x <- matrix(1:10, nrow=5, ncol=2)
#'  y <- 1:10
#'  result.list <- gen.formula.and.df(x, y)
#'
#' @export
#'
gen.formula.and.df <- function(x, y){

  # if the x is a matirx instead of a vector
  # generate a string of spline formula and a data.frame containing x and y
  if( (class(x) == "matrix") | (class(x) == "data.frame") ){

    ## generate data.frame combined with x (matirx) and y (vector).
    ## rename columns as x1, x2, ... , y.
    x.dim <- ncol(x)
    xy.df <- data.frame(x, y)
    x.name <- paste("x", 1:x.dim, sep = "")
    names(xy.df) <- c(x.name, "y")

    ## generate formula "y ~ x1 * x2 * ... * xn"
    x.formula <- paste(x.name, collapse=" * ")
    spline.formula <- paste("y ~ ", x.formula, sep="")

  }else{
    # x is a vector
    x.dim <- 1
    x.name <- "x"
    xy.df <- data.frame(x = x, y = y)
    spline.formula <- ("y ~ x")
  }

  return( list(xy.df = xy.df, spline.formula = spline.formula, x.dim = x.dim) )

}
