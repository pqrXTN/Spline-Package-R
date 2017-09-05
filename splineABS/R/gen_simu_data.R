#

gen.1dfan.simu.data <- function(nobs=1024, nslices=11, sliceMethod=NULL, nstar=NULL, SNR=7, iter=1){

  set.seed(123+iter)

  # generate independent varible: x and original dependent varible: v
  x <- c(0,sort(runif(nobs-2)),1)
  v <- sin(2*(4*x-2))+2*exp(-16*(4*x-2)^2)
  ssig <- sd(v)

  # add Gussian noise into signal: y = v + err
  set.seed(1234+iter)
  e <- rnorm(nobs, 0, ssig/SNR)
  y <- v +e

  my.x <- x
  my.y <- y

  # initiate a set of sampling index, it automatically includes the minimum and maximum.
  my.sample <- c(1, length(my.y))

  # decide the sampling size and break point of each slice.
  if(!is.null(sliceMethod)){
    my.y.slice <- hist(my.y, breaks = sliceMethod, plot = FALSE)$breaks
    nslice <- length(my.y.slice)-1
  }else{
    nslice <- nslice
    my.y.slice <- c(seq(min(my.y), max(my.y), length = nslice+1))
  }

  if(slices == "Scott"){
    my.y.slice <- hist(my.y, breaks = "Scott", plot = FALSE)$breaks
    nslice <- length(my.y.slice)-1
  }else if(slices == "Sturges"){
    my.y.slice <- hist(my.y, breaks = "Sturges", plot = FALSE)$breaks
    nslice <- length(my.y.slice)-1
  }else if(slices == "FD"){
    my.y.slice <- hist(my.y, breaks= "FD", plot = FALSE)$breaks
    nslice <- length(my.y.slice)-1
  }else{
    nslice <- slices
    my.y.slice <- c(seq(min(my.y), max(my.y), length = nslice))
  }
  nslice <- 11
  nbasis <- max(30, ceiling(10 * nobs^(2/9)))
  nobs.slice <- ceiling(nbasis/nslice)

  ## set break points. Divide into nlice-1 intervals
  ### since the interval is [a,b) for conivience of sampling, 1 is added to right boundary
  my.y.slice <- c(seq(range(my.y)[1], range(my.y)[2],length=nslice-1), range(my.y)[2]+1)


}
