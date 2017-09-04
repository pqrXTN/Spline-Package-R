rm(list=ls())
setwd('/home/grad/nanzhang/Documents/R/ABS-SS/1D/')
library(gss)
nobs <- 1024
n=nobs
# produce the all functions in Donoho and Johnstone(1994)
DJ.EX.rand <- function (x, n = 1024, signal = 7, rsnr = 7, noisy = FALSE, plotfn = FALSE) 
{
  #x <- seq(1, n)/n
  #x <- runif(n)
  t <- c(0.1, 0.13, 0.15, 0.23, 0.25, 0.4, 0.44, 0.65, 0.76, 
         0.78, 0.81)
  h1 <- c(4, -5, 3, -4, 5, -4.2, 2.1, 4.3, -3.1, 2.1, -4.2)
  blocks <- rep(0, n)
  for (i in seq(1, length(h1))) {
    blocks <- blocks + (h1[i] * (1 + sign(x - t[i])))/2
  }
  h2 <- c(4, 5, 3, 4, 5, 4.2, 2.1, 4.3, 3.1, 5.1, 4.2)
  w <- c(0.005, 0.005, 0.006, 0.01, 0.01, 0.03, 0.01, 0.01, 
         0.005, 0.008, 0.005)
  bumps <- rep(0, n)
  for (i in seq(1, length(h2))) {
    bumps <- bumps + h2[i] * pmax(0, (1 - abs((x - t[i])/w[i])))^4
  }
  heavi <- 4 * sin(4 * pi * x) - sign(x - 0.3) - sign(0.72 - 
                                                        x)
  eps <- 0.05
  doppler <- sqrt(x * (1 - x)) * sin((2 * pi * (1 - eps))/(x + 
                                                             eps))
  blocks <- blocks/sqrt(var(blocks)) * signal
  bumps <- bumps/sqrt(var(bumps)) * signal
  heavi <- heavi/sqrt(var(heavi)) * signal
  doppler <- doppler/sqrt(var(doppler)) * signal
  if (noisy == TRUE) {
    values <- list(blocks = blocks + rnorm(n, 0, signal/rsnr), 
                   bumps = bumps + rnorm(n, 0, signal/rsnr), heavi = heavi + 
                     rnorm(n, 0, signal/rsnr), doppler = doppler + 
                     rnorm(n, 0, signal/rsnr))
  }
  else {
    values <- list(blocks = blocks, bumps = bumps, heavi = heavi, 
                   doppler = doppler)
  }
  if (plotfn == TRUE) {
    par(mfrow = c(3, 2))
    plot(x, values$blocks, type = "l", ylab = "(a) Blocks")
    plot(x, values$bumps, type = "l", ylab = "(b) Bumps")
    plot(x, values$heavi, type = "l", ylab = "(c) HeaviSine")
    plot(x, values$doppler, type = "l", ylab = "(d) Doppler")
  }
  return(values)
}


##########################################################################################
##                                        Blocks                                        ##
##########################################################################################

mse.compare=matrix(NA, 100, 3)

for(iter in 1:100)
{
  print(iter)
  
  set.seed(123+iter)
  x <- c(0,sort(rbeta(nobs-2,2,2)),1)
  
  v <- DJ.EX.rand(x, n=nobs) 
  ssig <- sd(v$doppler)
  
  SNR <- 7
  sigma <- ssig/SNR
  
  e <- rnorm(nobs, 0, sigma)
  v=v$doppler
  y <-v +e
  
  my.x <- x
  my.y <- y
  
  nslice <- 11
  nbasis <- max(30, ceiling(20 * nobs^(2/9)))
  nobs.slice <- ceiling(nbasis/nslice)
  my.y.slice <- c(seq(range(my.y)[1], range(my.y)[2],length=nslice-1), range(my.y)[2]+1)
  
  my.sample <- c(1, length(my.y))
  
  set.seed(128)
  for(i in 1:(nslice)){
    my.which <- which( my.y.slice[i]<= my.y   &  my.y  < my.y.slice[i+1])
    if( length(my.which) >= nobs.slice){
      my.sample <- union(my.sample,sample(my.which, nobs.slice))
    } else {
      my.sample <- sort(union(my.sample, my.which))
    }
  }
  
  set.seed(130+iter)
  my.sample0 = sample(1:n, length(my.sample))
  
  ###
  t0=proc.time()
  my.ubs <- ssanova(my.y~my.x, id.basis=my.sample0, alpha=-1)
  t.ubs=proc.time()-t0
  
  t0=proc.time()
  my.abs <- ssanova(my.y~my.x, id.basis=my.sample, alpha=-1)
  t.abs=proc.time()-t0
  
  t0=proc.time()
  my.full <- ssanova0(my.y~my.x)
  t.full=proc.time()-t0
  
  new=data.frame(my.x=my.x)
  
  est.ubs <- predict(my.ubs, newdata=new, se.fit=T)
  est.abs <- predict(my.abs, newdata=new, se.fit=T)
  est.full <- predict(my.full, newdata=new, se.fit=T)
  
  mse.ubs = mean((est.ubs$fit-v)^2)
  mse.abs = mean((est.abs$fit-v)^2)
  mse.full = mean((est.full$fit-v)^2)
  
  mse.compare[iter,]=c(mse.full, mse.ubs, mse.abs)
  
  
}

colnames(mse.compare)=c('Full', 'UBS','ABS')
mse.compare=data.frame(mse.compare)
boxplot(mse.compare)

write.csv(mse.compare, 'mse_1d_doppler_100rep_full_ubs_abs.csv', row.names=F)
