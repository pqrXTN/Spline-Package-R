#' @title splineABS: A package for smoothing splines via adaptive basis sampling
#'
#' @description  The splineABS package provides smoothing splines with Gaussian type responses.
#'
#' @details Compared to full sampling, the apply of adaptive sampling reduces the
#'  computatuial complex from \eqn{O(n^3)} to \eqn{O(n n*^2)},
#'  where \eqn{n*} is number of samples.
#'
#' @author Tianning Xu
#'
#' @import stats gss
#' @docType package
#' @name splineABS
#'
#' @references Ma P, Huang J Z, Zhang N. Efficient computation of smoothing splines via adaptive basis sampling[J]. Biometrika, 2015, 102(3): 631-645.
#'
#'   GU, C. (2013). Smoothing Spline ANOVA Models. New York: Springer, 2nd ed.
#'
NULL
