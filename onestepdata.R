
# h = 4
# price_low_bound = an_matrix[1,7]
# price_high_bound = an_matrix[1,8]


onestepdata = function (price_low_bound,price_high_bound,h,p,q){
  p1 = p
  q1 = matrix(0, length(q), 1)
  q1[1] = q[1]
  i = 1
  
  while(p1[i] <= price_low_bound){
    p1[i] = price_low_bound
    i = i +1
  }
  I1 = i - 1 # locate the prices lower than the prices where the curve jumps
  
  while( (p1[i] < price_high_bound) & (i < length(p1)) ){
    i = i +1
  }
  I2 = i -1  # locate the prices higher than the prices where the curve jumps
  
  while(i <= length(p1)){
    p1[i] = price_high_bound
    i = i +1
  }
  
  for(n in 2:length(q)){
    q1[n] = q[n] + q1[n-1]
  }
  
  m = round(q1[length(q1)]/h)
  datax = seq(0, m*h, by= h)
  dataf = matrix(0, length(datax), 1)
  i = 1
  for(n in 1:length(q1)){
    while ( (i <= m) & (datax[i] <= q1[n]) ){
      dataf[i] = p1[n]
      i = i +1
    }
  }
  
  dataf[i] = price_high_bound
  center = round( (q1[I1]/h + (q1[I2] - q[I2])/h) * .5 )
  
  return(list('datax'=datax,
              'dataf'=dataf,
              'center'=center))
}

