library(vroom)
library(tidyverse)
library(tidyselect)
library(pracma)
library(doSNOW)
library(foreach)
library(plyr)
library(tictoc)

library(mixtools)
library(LaplacesDemon)
library(nor1mix)

source("RBF_approx_coeff.R")

secondary_up_bids_offers_all = vroom("secondary_up_bids_offers_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_up_bids_offers_all)[1] = "datetime"
secondary_up_bids_offers_all = as.data.frame(secondary_up_bids_offers_all)

secondary_up_bids_prices_all = vroom("secondary_up_bids_prices_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_up_bids_prices_all)[1] = "datetime"
secondary_up_bids_prices_all = as.data.frame(secondary_up_bids_prices_all)

M_supply = 10

comb <- function(...) {
  mapply(rbind.fill, ..., SIMPLIFY=FALSE)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = nrow(secondary_up_bids_offers_all)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

tic()
secondary_up_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  p = secondary_up_bids_prices_all[i,2:length(secondary_up_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = secondary_up_bids_offers_all[i,2:length(secondary_up_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  
  q_cumsum = cumsum(q)
  matrix_coeff_supply = RBF_approx_coeff(p,q,M_supply)
  
  x = seq(0,q_cumsum[length(q_cumsum)],by = 5)
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
}
toc()

# i = 3
# xxx = as.matrix(secondary_up_approx[[2]][i,])
# yyy = as.matrix(secondary_up_approx[[1]][i,])
# 
# p = secondary_up_bids_prices_all[i,2:length(secondary_up_bids_prices_all)]
# p = t(p)
# p = na.omit(p)
# p = c(p)
# 
# q = secondary_up_bids_offers_all[i,2:length(secondary_up_bids_offers_all)]
# q = t(q)
# q= na.omit(q)
# q = c(q)
# 
# q_cumsum = cumsum(q)
#   
# plot(x=xxx, y=yyy,xlab = "quatity after cumulative sum", ylab = "price")
# points(x=q_cumsum, y =p, pch='+',col = "red",cex=1.5 )










###########################################################################################################################
secondary_down_bids_offers_all = vroom("secondary_down_bids_offers_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_down_bids_offers_all)[1] = "datetime"
secondary_down_bids_offers_all = as.data.frame(secondary_down_bids_offers_all)

secondary_down_bids_prices_all = vroom("secondary_down_bids_prices_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_down_bids_prices_all)[1] = "datetime"
secondary_down_bids_prices_all = as.data.frame(secondary_down_bids_prices_all)

M_supply = 10


comb <- function(...) {
  mapply(rbind.fill, ..., SIMPLIFY=FALSE)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = nrow(secondary_down_bids_offers_all)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)


secondary_down_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  p = secondary_down_bids_prices_all[i,2:length(secondary_down_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = secondary_down_bids_offers_all[i,2:length(secondary_down_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  q2 = abs(q)
  
  
  q2_cumsum = cumsum(q2)
  
  
  matrix_coeff_supply = RBF_approx_coeff(p,q2,M_supply)
  
  x = seq(0,q2_cumsum[length(q2_cumsum)],by = 5)
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
  
}
#################################
# be note that,the approximations are calculated by the absolution of q. This means we need map -q2_cumsum with the approximations
#################################
# plot(x= -x, y=Supply_approx,xlab = "quatity after cumulative sum", ylab = "price")
# points(x= -q2_cumsum, y =p, pch='+',col = "red",cex=1.5 )


# 
# i = 9
# xxx = as.matrix(secondary_down_approx[[2]][i,])
# yyy = as.matrix(secondary_down_approx[[1]][i,])
# 
# p = secondary_down_bids_prices_all[i,2:length(secondary_down_bids_prices_all)]
# p = t(p)
# p = na.omit(p)
# p = c(p)
# 
# q = secondary_down_bids_offers_all[i,2:length(secondary_down_bids_offers_all)]
# q = t(q)
# q= na.omit(q)
# q = c(q)
# q2 = abs(q)
# 
# 
# q2_cumsum = cumsum(q2)
# 
# 
# plot(x=-xxx, y=yyy,xlab = "quatity after cumulative sum", ylab = "price")
# points(x=-q2_cumsum, y =p, pch='+',col = "red",cex=1.5 )
# 









#################################################################################

tertiary_up_bids_offers_all = vroom("tertiary_up_bids_offers_all.csv",col_types = list(datetime = col_character()))
colnames(tertiary_up_bids_offers_all)[1] = "datetime"
tertiary_up_bids_offers_all = as.data.frame(tertiary_up_bids_offers_all)

tertiary_up_bids_prices_all = vroom("tertiary_up_bids_prices_all.csv",col_types = list(datetime = col_character()))
colnames(tertiary_up_bids_prices_all)[1] = "datetime"
tertiary_up_bids_prices_all = as.data.frame(tertiary_up_bids_prices_all)

M_supply = 10


comb <- function(...) {
  mapply(rbind.fill, ..., SIMPLIFY=FALSE)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = nrow(tertiary_up_bids_offers_all)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

tic()
tertiary_up_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  p = tertiary_up_bids_prices_all[i,2:length(tertiary_up_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = tertiary_up_bids_offers_all[i,2:length(tertiary_up_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  
  q_cumsum = cumsum(q)
  matrix_coeff_supply = tryCatch(
    {
      RBF_approx_coeff(p,q,M_supply)
      
    },
    error = function(e){
      matrix(NA,M_supply, 3)
    })
  
  x = seq(1e-15,q_cumsum[length(q_cumsum)],by = 20 )
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
  
  
}
toc()
close(pb)
stopCluster(cl) 





# adjust the shape parameter of RBF_approx_coeff, until all curves are represented

temp = tertiary_up_approx[[1]]
missing_rows = which(rowSums(is.na(temp)) == ncol(temp)) # checking the rows made of NA's 

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = length(missing_rows)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

tic()
missing_rows_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  iter = missing_rows[i]
  p = tertiary_up_bids_prices_all[iter,2:length(tertiary_up_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = tertiary_up_bids_offers_all[iter,2:length(tertiary_up_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  
  q_cumsum = cumsum(q)
  matrix_coeff_supply = tryCatch(
    {
      RBF_approx_coeff(p,q,M_supply,1/2000) # could be 1/2500, 1/2000, 1/4000, 1/5000
      
    },
    error = function(e){
      matrix(NA,M_supply, 3)
    })
  
  x = seq(1e-15,q_cumsum[length(q_cumsum)],by = 20 )
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
  
  
}
toc()
close(pb)
stopCluster(cl) 




aa = missing_rows_approx[[1]]
bb = which(rowSums(is.na(aa)) == ncol(aa))

for(i in 1:nrow(aa)){
  index = missing_rows[i]
  len = length(temp[index,]) - length(aa[i,])
  temp[index,] = matrix(c(aa[i,],rep(NA,len)), nrow= 1)
}
 

tertiary_up_approx[[1]] = temp









#########################################################################################

tertiary_down_bids_offers_all = vroom("tertiary_down_bids_offers_all.csv",col_types = list(datetime = col_character()))
colnames(tertiary_down_bids_offers_all)[1] = "datetime"
tertiary_down_bids_offers_all = as.data.frame(tertiary_down_bids_offers_all)

tertiary_down_bids_prices_all = vroom("tertiary_down_bids_prices_all.csv",col_types = list(datetime = col_character()))
colnames(tertiary_down_bids_prices_all)[1] = "datetime"
tertiary_down_bids_prices_all = as.data.frame(tertiary_down_bids_prices_all)

M_supply = 10


comb <- function(...) {
  mapply(rbind.fill, ..., SIMPLIFY=FALSE)
}

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = nrow(tertiary_down_bids_offers_all)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

tic()
tertiary_down_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  p = tertiary_down_bids_prices_all[i,2:length(tertiary_down_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = tertiary_down_bids_offers_all[i,2:length(tertiary_down_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  q2 = abs(q)
  
  
  q2_cumsum = cumsum(q2)
  
  matrix_coeff_supply = tryCatch(
    {
      RBF_approx_coeff(p,q2,M_supply)
      
    },
    error = function(e){
      matrix(NA,M_supply, 3)
    })
  
  x = seq(1e-15,q2_cumsum[length(q2_cumsum)],by = 20 )
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
  
  
}
toc()
close(pb)
stopCluster(cl) 

 



# adjust the shape parameter of RBF_approx_coeff, until all curves are represented

temp = tertiary_down_approx[[1]]
missing_rows = which(rowSums(is.na(temp)) == ncol(temp)) # checking the rows made of NA's 

cl = makeSOCKcluster(6)
registerDoSNOW(cl)
iterations = length(missing_rows)
pb = txtProgressBar(max=iterations, style=3)
progress = function(n) setTxtProgressBar(pb, n)
opts = list(progress=progress)

tic()
missing_rows_approx = foreach(i = 1:iterations, .combine = comb, .options.snow = opts,.packages="pracma")%dopar%{
  iter = missing_rows[i]
  p = tertiary_down_bids_prices_all[iter,2:length(tertiary_down_bids_prices_all)]
  p = t(p)
  p = na.omit(p)
  p = c(p)
  
  q = tertiary_down_bids_offers_all[iter,2:length(tertiary_down_bids_offers_all)]
  q = t(q)
  q= na.omit(q)
  q = c(q)
  q2 = abs(q)
  
  
  q2_cumsum = cumsum(q2)
  
  matrix_coeff_supply = tryCatch(
    {
      RBF_approx_coeff(p,q2,M_supply,1/4000) # could be 1/2500, 1/2000, 1/4000, 1/5000
      
    },
    error = function(e){
      matrix(NA,M_supply, 3)
    })
  
  x = seq(1e-15,q2_cumsum[length(q2_cumsum)],by = 20 )
  Supply_approx = rep(0,times = length(x))
  
  for(n in 1:M_supply){
    Supply_approx= Supply_approx + matrix_coeff_supply[n,1]*( erf((x-matrix_coeff_supply[n,2] )/ matrix_coeff_supply[n,3])+1);
  }
  res1 = data.frame(matrix(Supply_approx,nrow = 1))
  res2 = data.frame(matrix(x,nrow = 1))
  list(res1, res2)
  
  
}
toc()
close(pb)
stopCluster(cl) 




aa = missing_rows_approx[[1]]
bb = which(rowSums(is.na(aa)) == ncol(aa))

for(i in 1:nrow(aa)){
  index = missing_rows[i]
  len = length(temp[index,]) - length(aa[i,])
  temp[index,] = matrix(c(aa[i,],rep(NA,len)), nrow= 1)
}


tertiary_down_approx[[1]] = temp


###################################################################################
# here is another way to represent the data: Instead of plotting QP plot, which is a plot show Q on x-axis and P on the y-axis 
# we are interested in the PQ plot. Why do we study PQ plot? Because, we want to use the machinery of mixture distribution estimation. 
secondary_up_bids_offers_all = vroom("secondary_up_bids_offers_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_up_bids_offers_all)[1] = "datetime"
secondary_up_bids_offers_all = as.data.frame(secondary_up_bids_offers_all)

secondary_up_bids_prices_all = vroom("secondary_up_bids_prices_all.csv",col_types = list(datetime = col_character()))
colnames(secondary_up_bids_prices_all)[1] = "datetime"
secondary_up_bids_prices_all = as.data.frame(secondary_up_bids_prices_all)

Price = secondary_up_bids_prices_all[1,2:length(secondary_up_bids_prices_all)]
Quantity = secondary_up_bids_offers_all[1,2:length(secondary_up_bids_offers_all)]

Price = t(Price)
Price = na.omit(Price)
Price = c(Price)

Quantity = t(Quantity)
Quantity = na.omit(Quantity)
Quantity = c(Quantity)
Quantity_cumsum = cumsum(Quantity)

########
#SPrice = UPrice[UPrice < 500]
#SQuant = UQuant[UPrice < 500]

SQuant = Quantity_cumsum/max(Quantity_cumsum)
plot(Price, SQuant, type = "p")
Xmin = 0
Xmax = max(Price)
Xvals = Xmin + (Xmax - Xmin)*(0:999)/999
PAppFc <- approxfun(Price,SQuant, method = "constant",f=0,rule=2,ties = max)
P <- PAppFc(Xvals)
lines(Xvals,P)

p <- diff(c(0,SQuant))
n <- min(round(1/min(p)), 1000)
AbsoluteFrequency = round(p*n)
n = sum(AbsoluteFrequency)
Sample = matrix(NA, nrow = n, ncol = 1)
k = 1
for (i in 1:length(Price)){
  Sample[k:(k+AbsoluteFrequency[i]-1),1] = Price[i]
  k = k+AbsoluteFrequency[i]
}
plot(ecdf(Sample))
points(SPrice, SQuant, col = "red")
lines(Xvals, P, col = "red")

# Normal mixture
library(mixtools)
mixmdl = normalmixEM(Sample, k = 5)
#mixmdl = normalmixEM(Sample, mean.constr = c(5, 10, 50, 100, 200), k = 5)

library(LaplacesDemon)
p <- mixmdl$lambda
mu <- mixmdl$mu
sigma <- mixmdl$sigma
x <- seq(from=0, to=max(Sample)+1, by=0.1)
plot(ecdf(Sample))
lines(x, pnormm(x, p, mu, sigma), col = "red") #CDF

plot(Quantity_cumsum, Price, type = "p")
Xmin = 0
Xmax = max(Quantity_cumsum)
Xvals = Xmin + (Xmax - Xmin)*(0:999)/999
PAppFc <- approxfun(Quantity_cumsum, Price, method = "constant",f=1,rule=2,ties = max)
P <- PAppFc(Xvals)
lines(Xvals,P)

q = seq(from=0, to=1, by=0.005)
library(nor1mix)
nm <- norMix(mu = mu, sigma = sigma, w = p)
lines(max(Quantity_cumsum)*q, qnorMix(q, nm), col = "red") #ICDF

