#' Run permutation test using parallel processing
#' 
#' @param x.mat The desired x matrix that contains all the characteristics data/predictor variables; the matrix should contain the desired column user intends completing the permutation on.
#' @param y The desired preprocessed neuroimaging data, with a mask or not.
#' @param columns A numeric value indicating which characteristic/predictor we want analyzed. User inputs the column number they want permuted. 
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
#' a <- run_permtest(x.mat = xtx, y = ymat, columns = 1, split = 101, num_perms = 100)
#' }


run_permtest_justy <- function(x.mat = x, y = ymat, columns = 2:3, split = 101, num_perms = 100){
  #Split the data into the number of pieces to address issues on a chunk based level, not necessary, but suggested
  vindx = rep(1:split, each = floor(nrow(y)/(split - 1)))[1:nrow(y)]
  
  #Define bhat for the particular characteristic: sex, trt/ctl, age, bmi
  xtx = xmat_trans(x.mat)
  bhat = y %*% x.mat[,columns]
  
  rr1 <- foreach(i = 1:split, .combine = rbind,.packages=c("foreach", "parPerm", "doParallel")) %dopar%{
    perm_stack = foreach(j = 1:num_perms, .packages=c("foreach", "parPerm", "doParallel")) %dopar% {
      set.seed(j)
      n = nrow(x.mat)
      indx = sample(n,n,replace=FALSE)
      perms = (y[vindx==i,indx] %*% xtx)[,columns]
    }
    
    rr1_piece <- foreach(k = 1:length(columns), .combine = cbind, .packages = "foreach") %do% {
      aa_k <- do.call(rbind, lapply(perm_stack, function(x)x[,k]))
      ps_k <- unlist(apply(rbind(bhat[vindx==i, k], aa_k),2, function(x)sum( x[-1]> abs(x[1]) |  x[-1]< (-1*abs(x[1])) ))/num_perms)
    }
  }
}
