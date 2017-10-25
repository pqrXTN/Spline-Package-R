#' @title Generate sampling basis in each subset
#'
#' @description  The subsets is divied randomly and evenly. Return a list of indices of samples and indices basis in each subset.
#'
#' @param N             The number of whole smaples.
#' @param sample.index  The indices of samples.
#' @param subset.number The number of subsets.
#' @param ratio         A vector of the ratio of size of each subset. Default is equal.
#'
#' @return       A list contains two vectors. The indices of samples and basis in each subset.
#'
#' @examples
#' generate.subset(y = 1:10, sample.index = c(1,3,5), subset.number = 2)
#'
#' @export
#'
generate.subset <- function(N, sample.index, subset.number=1, ratio = NULL){

  whole.index <- 1:N
  subset.index <- list()
  subset.sample.index <- list()
  if(is.null(ratio)){
    # get a rough subset size
    subset.meansize <- round(N / subset.number)
    # get an exact subset size
    subset.size <- c( rep(subset.meansize, subset.number-1),
                      N-(subset.number-1)*subset.meansize )
  }else{
    # get a rough subset size
    subset.size <- round(N * ratio / sum(ratio))
    index.largest.subset <- which.max(ratio)
    # get an exact subset size (adjust the largest subset)
    subset.size[index.largest.subset] <- subset.size[index.largest.subset] -
      (sum(subset.size) - N)
    print(subset.size)
  }


  remain.index <- whole.index

  for(i in 1:(subset.number)){

    if(i != subset.number){
      subset.index[[i]] <- sort(sample(remain.index, subset.size[i]))
      remain.index <- setdiff(remain.index, subset.index[[i]])
    }else{
      subset.index[[i]] <- sort(remain.index)
    }
    subset.sample.index[[i]] <- intersect(sample.index, subset.index[[i]])
  }

  return(list(subset.index=subset.index, subset.sample.index=subset.sample.index))
}
