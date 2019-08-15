#' Bootstrapping using parPerm
#' 
#' @param x.mat The desired x matrix that contains all the characteristics data/predictor variables; the matrix should contain the desired column user intends completing the permutation on.
#' @param y The desired preprocessed neuroimaging data, with a mask or not.
#' @param columns A statement indicating which characteristic/predictor we want analyzed, for multiple categories select the proper columns
#' @param split A value that allows user to finely split the processing task to allow for error checking.
#' @param num_perms User defined value for number of desired permutations 
#' @param boots Number of desired bootstraps
#' @return The desired results post-bootstrap
#' @examples 
#' #library(parallel)
#' #lirary(doparallel)
#' #library(parPerm)
#' #cores <- detectCores()
#' #cl <- makeCluster(cores[1]-1) #not to overload your computer
#' #registerDoParallel(cl)
#' #boooooot <- parperm_boot(x.mat.boot = xtx, y.boot = ymat, columns.boot = 2:3, split.boot = 101, num_perms.boot = 5, boots = 5)


parperm_boot <- function(x.mat.boot = xtx, y.boot = ymat, columns.boot = 2:3, split.boot = 101, num_perms.boot = 5, boots = 5){
  # bootruns <- list()
  # for(ll in 1:boots){
  #   set.seed(ll+20190815)
  #   n=nrow(x.mat.boot)
  #   samples=sample(n,n,replace=TRUE)
  #   bootruns[[ll]] <- run_permtest_multicat(x.mat = x.mat.boot[samples,], y = y.boot[,samples], columns = columns.boot, split = split.boot, num_perms = num_perms.boot)
  # }
  # return(bootruns)
  # 
  
  ###Need to figure out how to use parallelization here too -- this should work
  n = nrow(x.mat.boot)
  straps <- foreach(ll = 1:boots, .packages = c("foreach", "parPerm")) %dopar% {
    set.seed(ll+20190815)
    samples = sample(n,n,replace=TRUE)
    strap = run_permtest_multicat(x.mat = x.mat.boot[samples,], y = y.boot[,samples], columns = columns.boot, split = split.boot, num_perms = num_perms.boot)
  }
  return(straps)
}   

