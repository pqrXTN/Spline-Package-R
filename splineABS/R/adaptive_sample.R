
#' do adaptive sampling depending on y value
#' @param y A vecotr of response value.
#' @param nstar A number of sample size. If it is \code{NULL}, the number would be \eqn{10n^{2/9}}
#' @param nslice A number of slices numbers. If \code{sliceMethod} is not \code{NULL}, it isn't effective.
#' @param sliceMethod A string of slice method among "NULL", "Scott", "Sturges", "FD"(Freddman and Diaconis)
#' @return Index of selected samples in \code{y}.
#' @examples
#' adap.sample(y=1:10, sliceMethod="Scott")
adap.sample <- function(y, nstar=NULL, nslice=10, sliceMethod="NULL"){
  sample.index <- 1
  return(sample.index)
}
