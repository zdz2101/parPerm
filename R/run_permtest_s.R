#' Run permutation test using parallel processing
#' 
#' @param x.mat the desired x matrix that was preprocessed containing all the characteristics
#' @param y the desired preprocessed neuroimaging data 
#' @param columns which characteristic/predictor we want analyzed 
#' @split how finely you want the processing split to allow for error checking as well
#' @num_perms number of permutations we want to run
#' @return the desired permutation result
#' 
#' a <- run_permtest(x.mat = xtx, y = ymat, columns = 1, split = 101, num_perms = 100)



run_permtest <-function(x.mat = xtx, y = ymat, columns = 1, split = 101, num_perms = 100){
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  if(length(columns) != 1){
    return("Use rum_permtest_multicat for predictors with multiple categories")
  }
  
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

    rr1_piece = cbind(unlist(ps1))
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