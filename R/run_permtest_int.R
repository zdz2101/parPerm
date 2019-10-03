run_permtest_interaction<-function(x.mat = xtx, y = ymat, var1 = 2, var2 = 3, split = 101, num_perms = 100){
  #Split the data into the number of pieces to address issues on a chunk based level, not necessary, but suggested
  vindx = rep(1:split, each = floor(nrow(y)/(split - 1)))[1:nrow(y)]
  
  #Define bhat for the particular characteristic: sex, trt/ctl, age, bmi
  bhat = y %*% x.mat[,columns]
  
  rr1 <- foreach(i = 1:split, .combine = rbind, .packages="foreach") %dopar%{
    perm_stack = foreach(j = 1:num_perms, .packages="foreach") %do% {
      set.seed(j)
      n = nrow(x.mat)
      indx = sample(n,n,replace=FALSE)
      perms = y[vindx==i,] %*% x.mat[indx, columns]
    }
    
    #------ THIS DEFINITELY WORKS ------
    # aa1 = do.call(rbind, lapply(perm_stack, function(x)x[,1]))
    # ps1 = apply(rbind(bhat[vindx==i,1], aa1),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/num_perms
    # 
    # aa2 = do.call(rbind, lapply(perm_stack, function(x)x[,2]))
    # ps2 = apply(rbind(bhat[vindx==i,2], aa2),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/num_perms
    # 
    # rr1_piece = cbind(unlist(ps1),unlist(ps2))
    # rr1_piece
    #-----------------------------------
    
    rr1_piece <- foreach(k = 1:length(columns), .combine = cbind, .packages = "foreach") %do% {
      aa_k <- do.call(rbind, lapply(perm_stack, function(x)x[,k]))
      ps_k <- unlist(apply(rbind(bhat[vindx==i, k], aa_k),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/num_perms)
    }
  }
}