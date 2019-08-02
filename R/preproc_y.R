#' Preprocess y matrix data, while double checking all data is nifti
#' 
#' @param gmvol the desired nifti dataset we want analyzed
#' @param mask if we are using a mask because data is too large
#' @param mask_logical an indicator whether of not we are using a mask
#' @return a processed matrix ready for permutation test analysis
#' 
#' ymat <- ymat_trans(gmvol, mask, mask_logical = TRUE)

#Problems: what is mask == 1 for>
ymat_trans <- function(gmvol, mask, mask_logical = FALSE){

  if (mask_logical == TRUE){
    
    #check to see if files are NIFTI
    if(class(gmvol)[1] != "nifti" | class(mask)[1] != "nifti"){
      print("Your files are not nifti files.")
    }
    
    else{
      
      #Get the dimensions your data
      dim = c( prod(dim(gmvol)[1:3]), #-- product of the 3 imaging dimensions
               dim(gmvol)[4] # -- num of obs/patients
      )
      
      #Use the mask of 1 -- ask about this
      ymat = array(gmvol@.Data, dim)[mask@.Data==1,]
    }
    return(ymat)
  }
  
  else{
    if(class(gmvol)[1] != "nifti"){
      print("Your files are not nifti files.")
    }
    
    else{
      dim = c( prod(dim(gmvol)[1:3]), #-- product of the 3 imaging dimensions
               dim(gmvol)[4] # -- num of obs/patients
      )
      
      #Use the mask of 1 -- ask about this
      ymat = array(gmvol@.Data, dim)
    }
    return(ymat)
  }
}

