#' Run permutation test using parallel processing
#' 
#' @param x.mat the desired x matrix that was preprocessed containing all the characteristics
#' @param y the desired preprocessed neuroimaging data 
#' @param columns which characteristic/predictor we want analyzed, for multiple categories select the columns containing the binary indicators
#' @split how finely you want the processing split to allow for error checking as well
#' @num_perms number of permutations we want to run
#' @return the desired permutation result
#' 
#' a <- run_permtest(x.mat = xtx, y = ymat, columns = 2:3, split = 101, num_perms = 100)



#Issues to address:
# - which x/y matrix columns to permute, how many
# - how to split the dataset for error checking
# - how many permutations
# - clean up this code to look nice


#reminder to write beforehand:


#how to package dependencies

run_permtest_multicat<-function(x.mat = xtx, y = ymat, columns = 2:3, split = 101, num_perms = 100){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)

  #Split the data into the number of pieces to address issues on a chunk based level, not necessary, but suggested
  vindx = rep(1:split, each = floor(nrow(ymat)/(split - 1)))[1:nrow(ymat)]

  #Define bhat for the particular characteristic: sex, trt/ctl, age, bmi
  bhat = y %*% x.mat[,columns]

  rr1 <- foreach(i = 1:split, .combine = rbind, .packages="foreach") %dopar%{
    #num_perm - how many seeds
    perm_stack = foreach(j = 1:num_perms, .packages="foreach") %do% {
      set.seed(j)
      n = nrow(x.mat)
      indx = sample(n,n,replace=FALSE)
      perms = y[vindx==i,] %*% x.mat[indx, columns]
    }

    aa1 = do.call(rbind, lapply(perm_stack,function(x)x[,1]))
    ps1 = apply(rbind(bhat[vindx==i,1], aa1),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/100

    aa2 = do.call(rbind, lapply(perm_stack,function(x)x[,2]))
    ps2 = apply(rbind(bhat[vindx==i,2], aa2),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/100

    rr1_piece = cbind(unlist(ps1),unlist(ps2))
    rr1_piece
    
    #work in progress
    # rr1_piece <- matrix(ncol = length(columns))
    # for(k in 1:length(columns)){
    #   aa_k = do.call(rbind, lapply(perm_stack,function(x)x[,k]))
    #   ps_k = apply(rbind(bhat[vindx==i, k], aa_k), 2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/100
    #   rr1_piece[,k] <- unlist(ps_k)
    # }
    # rr1_piece
  }
}