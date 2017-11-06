#' @title Generate sampling basis in each subset
#'
#' @description
#'   Return a list of indices of samples in whole data and indices of basis in
#'   each subset. Default: \code{subset.element.index == NULL}, randomly
#'   generate subsests equally.
#'
#' @details In order to garantee that every subset shares the same basis,
#'   each subset would have a copy of whole basis(by operation \code{union}).
#'   For example, if the basis are {#1, #2} and if the orginal subsets are
#'   {#1, #3, #4}, {#2, #5}, the return subsets would be {#1, #2, #3, #4} ,
#'   {#1, #2, #5}.
#'
#' @param N      The number of whole smaples.
#' @param ratio  A vector of the ratio of size of each subset. Default = equal.
#' @param basis.index   The indices of samples in whole data. It could be NULL.
#' @param subset.number The number of subsets.
#' @param subset.element.index A list of indices of each subset's element in
#'                            whole data. If NULL, randomly generate subsets.
#'
#'
#' @return  A list contains two lists named as \code{subsetElementIndex},
#'   \code{basisIndexInSubset}.
#' \itemize{
#'   \item \code{subsetElementIndex}: The global indices(global location) of
#'     each subset's elements in whole data.
#'   \item \code{basisIndexInSubset}: The local indices(local location) of
#'     basis in each subset.
#'   \item remark: global means the location of each element in whole data;
#'     local means the location in each subset.
#' }
#'
#' @examples
#' > generate.subset(20, basis.index = c(1,6,11,16), subset.number = 2)
#' $subsetElementIndex
#' $subsetElementIndex[[1]]
#' [1]  1  2  3  5  6  7  9 11 12 13 14 16 18
#'
#' $subsetElementIndex[[2]]
#' [1]  1  4  6  8 10 11 15 16 17 19 20
#'
#' $basisIndexInSubset
#' $basisIndexInSubset[[1]]
#' [1]  1  5  8 12
#'
#' $basisIndexInSubset[[2]]
#' [1] 1 3 6 8
#'
#'
#' > generate.subset(10, basis.index = c(1,2,6,7), subset.number = 2,
#'     subset.element.index =list(1:5, 6:10))
#' $subsetElementIndex
#' $subsetElementIndex[[1]]
#' [1] 1 2 3 4 5 6 7
#'
#' $subsetElementIndex[[2]]
#' [1]  1  2  6  7  8  9 10
#'
#' $basisIndexInSubset
#' $basisIndexInSubset[[1]]
#' [1] 1 2 6 7
#'
#' $basisIndexInSubset[[2]]
#' [1] 1 2 3 4
#'
#' @export
#'
generate.subset <- function(N, basis.index = NULL, subset.number = 1,
                            subset.element.index = NULL, ratio = NULL){

  whole.index <- 1:N
  basis.index.in.subset <- list()





  if(is.null(subset.element.index)){

    # 1) calculate size of each subset
    if(is.null(ratio)){
      # get a rough subset size approximately equally
      subset.meansize <- round(N / subset.number)
      # get an exact subset size
      subset.size <- c( rep(subset.meansize, subset.number-1),
                        N-(subset.number-1)*subset.meansize )
    }else{
      # get a rough subset size based on ratio.
      subset.size <- round(N * ratio / sum(ratio))
      index.largest.subset <- which.max(ratio)
      # get an exact subset size (adjust the largest subset)
      subset.size[index.largest.subset] <- subset.size[index.largest.subset]-
        (sum(subset.size) - N)
    }

    # 2) randomly generate subsets according to ratio

    # intialize
    remain.index <- whole.index
    subset.element.index <- list()

    for(i in 1:(subset.number)){
      if(i != subset.number){
        subset.element.index[[i]] <- sample(remain.index, subset.size[i])
        remain.index <- setdiff(remain.index, subset.element.index[[i]])
      }else{
        subset.element.index[[i]] <- remain.index
      }
    }
  }else{

    # if subsets are given in parameterm, just copy
    subset.element.index <- subset.element.index
  }


  # combine basia and each subset;
  # calculate the location of basis in each subset
  ## location would be used in  gss:ssanova.
  for(i in 1:(subset.number)){

    # 1) copy basis into each subset
    # ganrantee that each subset shares the same basis
    subset.element.index[[i]] <- sort(union(subset.element.index[[i]],
                                            basis.index))

    # 2) record the location of basis in each subset
    basis.index.in.subset[[i]] <- match(basis.index, subset.element.index[[i]])
  }

  # return a list contains two lists:
  ## 1) indices(global location) of subset elements;
  ## 2) local indices(local location) of basis in each subset
  return(list(subsetElementIndex = subset.element.index, basisIndexInSubset =
                basis.index.in.subset))

}
