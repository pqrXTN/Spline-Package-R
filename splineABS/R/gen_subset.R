#' @title Generate sampling basis in each subset
#'
#' @description  The subsets is divied randomly and evenly. Return a list of indices of samples and indices basis in each subset.
#' 
#' @param y             The response value in spline.
#' @param sample.index  The indices of samples.
#' @param subset.number The number of subsets.
#'
#' @return       A list contains two vectors. The indices of samples and basis in each subset.
#'
#' @examples 
#' generate.subset(y = 1:10, sample.index = c(1,3,5), subset.number = 2)
#'
#' @export
#'
generate.subset <- function(y, sample.index, subset.number=1){
  
  N <- length(y)
  whole.index <- 1:N
  subset.index <- list()
  subset.sample.index <- list()
  subset.meansize <- round(N / subset.number)
  subset.size <- c( rep(subset.meansize, subset.number-1),
                    N-(subset.number-1)*subset.meansize )
  
  remain.index <- whole.index
  
  # Divide whole data into m subsets.
  # Divide basis into m subsets based on the division.
  for(i in 1:(subset.number)){
    
    if(i != subset.number){
      subset.index[[i]] <- sort(sample(remain.index, subset.size[i]))
      remain.index <- setdiff(remain.index, subset.index[[i]])
    }else{
      subset.index[[i]] <- sort(remain.index)
    }
    
    # the index of basis is a intersection of sampleIndex and subsetIndex
    subset.sample.index[[i]] <- intersect(sample.index, subset.index[[i]])
  }
  
  return(list(subset.index=subset.index, subset.sample.index=subset.sample.index))
}
