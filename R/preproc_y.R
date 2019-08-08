#' Preprocess y matrix data, while double checking all data is nifti
#' 
#' @param neurodat The desired nifti dataset we want analyzed
#' @return A processed matrix ready for permutation test analysis
#' 
#' ymat <- ymat_trans(neurodat)

#Problems: what is mask == 1 for>
ymat_trans <- function(neurodat){
  
    #Check to see if files are NIFTI
    if(class(neurodat)[1] != "nifti"){
      print("Your files are not nifti files.")
    }
    
    else{
      dim = c( prod(dim(neurodat)[1:3]), #-- product of the 3 imaging dimensions
               dim(neurodat)[4] # -- num of obs/patients
              )
      ymat = array(neurodat@.Data, dim)
    }
    return(ymat)
}

