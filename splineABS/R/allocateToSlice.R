#' @title allocate elements into different slices
#'
#' @param nelement The number of elements to be allocated.
#' @param nslice   The number of slices.
#'
allocateToSlice <- function(nelement, nslice){

  aveSliceSize <- floor(nelement / nslice)

  nelement.slice <- rep(aveSliceSize, nslice)
  residue <- nelement -  aveSliceSize * nslice

  # select the "luck" slices to get an extra base.
  lucky.slice <- sample(1:nslice, residue)
  nelement.slice[lucky.slice] <- nelement.slice[lucky.slice] + 1

  return(nelement.slice)
}
