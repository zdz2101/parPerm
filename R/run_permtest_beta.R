#' Run permutation test using parallel processing, for predictor variabbles containing multiple (3+ categories)
#' 
#' @param x.mat The desired x matrix that contains all the characteristics data/predictor variables; the matrix should contain the desired column user intends completing the permutation on.
#' @param y The desired preprocessed neuroimaging data, with a mask or not.
#' @param columns A statement indicating which characteristic/predictor we want analyzed, for multiple categories select the proper columns
#' @param split A value that allows user to finely split the processing task to allow for error checking.
#' @param num_perms User defined value for number of desired permutations 
#' @return The desired permutation result
#' 
#' @examples
#' \dontrun{
#' cores <- detectCores()
#' cl <- makeCluster(cores[1]-1) #not to overload your computer
#' registerDoParallel(cl)
#' 
#' 
#' a <- run_permtest(x.mat = xtx, y = ymat, columns = 1, split = 101, num_perms = 100)
#' }

run_permtest_beta <-function(x.mat = x, y = ymat, columns = 1, split = 101, num_perms = 100){
  
  if(length(columns) != 1){
    return("Use rum_permtest_multicat for predictors with multiple categories")
  }
  
  #Split the data into the number of pieces to address issues on a chunk based level, not necessary, but suggested
  vindx = rep(1:split, each = floor(nrow(y)/(split - 1)))[1:nrow(y)]
  
  #Define bhat for the particular characteristic: sex, trt/ctl, age, bmi
  xtx = xmat_trans(x.mat)
  bhat = (y %*% xtx)[,columns]
  
  rr1 <- foreach(i = 1:split, .combine = rbind,.packages=c("foreach", "parPerm","doParallel")) %dopar%{
    #num_perm - how many seeds
    perm_stack = foreach(j = 1:num_perms, .packages=c("foreach", "parPerm","doParallel")) %do% {
      set.seed(j)
      n1 = 1:100
      n2 = 101:200
      indx_1 = sample(n1,length(n1),replace=FALSE)
      indx_2 = sample(n2,length(n2),replace=FALSE)
      perms = (y[vindx==i,] %*% xmat_trans(x.mat[c(indx_1,indx_2),]))[,columns]
    }
    
    aa1 = do.call(rbind, lapply(perm_stack,function(x) x))
    ps1 = apply(rbind(bhat[vindx==i], aa1),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/num_perms
    
    rr1_piece = cbind(unlist(ps1))
    rr1_piece
  }
}