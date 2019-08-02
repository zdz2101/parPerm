#Issues to address:
# - which x/y matrix columns to permute, how many
# - how to split the dataset for error checking
# - how many permutations
# - clean up this code to look nice


#reminder to write beforehand:
#cores=detectCores()
#cl <- makeCluster(cores[1]-1) #not to overload your computer
#registerDoParallel(cl)

#how to package dependencies

run_permtest_mod<-function(x.mat = xtx, y = ymat, split = 101, num_perms = 100){

  #Split the data into the number of pieces to address issues on a chunk based level, not necessary, but suggested
  vindx = rep(1:split, each = floor(nrow(ymat)/(split - 1)))[1:nrow(ymat)]

  #Define bhat for the particular characteristic: sex, trt/ctl, age, bmi
  bhat = y %*% x.mat[,2:3]

  rr1 <- foreach(i = 1:split, .combine = rbind, .packages="foreach") %dopar%{
    #num_perm - how many seeds
    perm_stack = foreach(j = 1:perms, .packages="foreach") %do% {
      set.seed(j)
      n = nrow(x.mat)
      indx = sample(n,n,replace=FALSE)
      perms = y[vindx==i,indx] %*% x.mat[,2:3]
    }

    aa1 = do.call(rbind, lapply(perm_stack,function(x)x[,1]))
    ps1 = apply(rbind(bhat[vindx==i,1],aa1),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/100

    aa2 = do.call(rbind, lapply(perm_stack,function(x)x[,2]))
    ps2 = apply(rbind(bhat[vindx==i,2],aa2),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/100

    rr1_piece = cbind(unlist(ps1),unlist(ps2))
    rr1_piece
  }

}
