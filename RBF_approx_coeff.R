library(pracma)
source("onestepdata.R")


RBF_approx_coeff = function(p,q,M_supply,shape = 1/3000){
  
  q_cumsum = cumsum(q)
   
  an_matrix = matrix(0, nrow= 4,ncol = M_supply +1)
  # the first row is prices at M steps
  # the second row is quantities at M steps
  # the third row is offers can buy when reaching the M prices
  # the fourth row is number of steps before reaching the M prices
  an_matrix[1, M_supply+1] = p[length(p)]
  an_matrix[2, M_supply+1] = q[length(q)]
  an_matrix[3, M_supply+1] = q_cumsum[length(q_cumsum)]
  an_matrix[4, M_supply+1] = 1 # initialize to be 1, it means need a step to reach the final prices
  an_matrix[4, 1] = 1 # initialize to be 1, it means need a step to jump from 0
  
  iter1 = 1
  for(num in 0:(M_supply-1)){
    price_jump_scale = an_matrix[1,M_supply-num+1]/(M_supply-num)
    j = 0
    
    while( (iter1 <= length(p))  &  
           (p[length(p) - iter1] > (an_matrix[1, M_supply -num +1] -price_jump_scale) )){
      iter1 = iter1 + 1
      j = j + 1
    }
    
    an_matrix[1, M_supply - num] = p[length(p) - iter1]
    an_matrix[2, M_supply - num] = q[length(q) - iter1]
    an_matrix[3, M_supply - num] = q_cumsum[length(q) - iter1]
    an_matrix[4, M_supply - num +1] = an_matrix[4, M_supply - num +1] + j
    
  }
  
  
  matrix_coeff = matrix(0,M_supply, 3)
  
  for(i in 1:M_supply){
    p1 = an_matrix[1,i]
    p2 = an_matrix[1,i+1]
    jump_num = an_matrix[4, i+1]
    center = an_matrix[3, i]
    
    if(jump_num == 1){
      matrix_coeff[i,] = c((p2 - p1)/2, center, 0.5)
    }
    
    if(jump_num > 1){
      h = 4
      paras = onestepdata(p1, p2, h, p, q)
      datax = paras$datax
      dataf = paras$dataf
      center = paras$center
      a1 = (p2 -p1)/2
      a2 = center
      a3 = shape #2000 # 3000
      a4 = p1
      z0 = c(a2, a3)
      
      F = function(z, zdata) a1 * ( erf( z[2] * (datax- h*z[1]) ) +1) + a4
      z = lsqcurvefit(F,z0,datax,dataf)$x #optimization
      matrix_coeff[i,] = c(a1,h*z[1],1/z[2])
    }
  }
  return(matrix_coeff)
}

