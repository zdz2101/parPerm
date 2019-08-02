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

