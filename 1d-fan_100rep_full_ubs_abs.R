rm(list=ls())
# setwd('/home/grad/nanzhang/Documents/R/ABS-SS/1D/')


library(gss)

nobs <- 1024
n=nobs


mse.compare=matrix(NA, 100, 3)


# Simulate splines for iter=100 times
for(iter in 1:100)
{
  print(iter)

  set.seed(123+iter)

  # generate independent varible: x and original dependent varible: v
  x <- c(0,sort(runif(nobs-2)),1)
  v <- sin(2*(4*x-2))+2*exp(-16*(4*x-2)^2)
  ssig <- sd(v)

  # set signal to noise rate
  SNR <- 7

  # add Gussian noise into signal: y = v + err
  # set.seed(1234+iter)
  e <- rnorm(nobs, 0, ssig/SNR)
  y <- v +e

  my.x <- x
  my.y <- y

  # decide the sampling size and break point of each slice.
  nslice <- 11
  nbasis <- max(30, ceiling(10 * nobs^(2/9)))
  nobs.slice <- ceiling(nbasis/nslice)

  ## set break points. Divide into nlice-1 intervals
  ### since the interval is [a,b) for conivience of sampling, 1 is added to right boundary
  my.y.slice <- c(seq(range(my.y)[1], range(my.y)[2],length=nslice-1), range(my.y)[2]+1)

  # initiate a set of sampling index, it automatically includes the minimum and maximum.
  my.sample <- c(1, length(my.y))

  # sampling at each slice into `mysample`. If the number of members is less than what we need, take in all.
  ## There are actually nslice-2 intervals except for minimum and maximum
  ## 但这里抽样有点粗糙，需要改进，因为实际上只抽了nslice-2份，因为最后一份里只有maximum
  ## 第一份里又包括了早就包括早my.sample 里面的 minium 我觉得可以把抽样边界改进：
  ## 去掉最后一个区间，把第一个区间改成从第二小开始
  set.seed(128)
  for(i in 1:(nslice-1)){
    my.which <- which( my.y.slice[i]<= my.y   &  my.y  < my.y.slice[i+1])
    if( length(my.which) >= nobs.slice){
      my.sample <- union(my.sample,sample(my.which, nobs.slice))
    } else {
      my.sample <- sort(union(my.sample, my.which))
    }
  }

  # convert sampling index in `mysample` into sampling contents in `my.sample0`
  set.seed(130+iter)
  my.sample0 = sample(1:n, length(my.sample))

  ###
  # apply gss::ssanova to 1) full samples; 2)uniform samples; 3) adaptive samples
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

write.csv(mse.compare, 'mse_1d_fan_100rep_full_ubs_abs.csv', row.names=F)
