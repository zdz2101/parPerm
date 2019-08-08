#' Preprocess y matrix data, while double checking all data is nifti
#' 
#' @param neurodat The desired nifti dataset we want analyzed
#' @param mask User provided mask
#' @return A processed matrix ready for permutation test analysis
#' 
#' ymat <- ymat_trans(neurodat, mask, mask_logical = TRUE)

#Problems: what is mask == 1 for>
ymat_trans_mask <- function(neurodat, mask){
  if(class(neurodat)[1] != "nifti" | class(mask)[1] != "nifti"){
    print("Your files are not nifti files.")
  }
    
  else{
    #Get the dimensions your data
    dim = c( prod(dim(neurodat)[1:3]), #-- product of the 3 imaging dimensions
               dim(neurodat)[4] # -- num of obs/patients
             )
      
    #Use the mask of 1 -- ask about this
    ymat = array(neurodat@.Data, dim)[mask@.Data==1,]
  }
  return(ymat)
}
  

