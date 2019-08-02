#' Preprocess x matrix data
#' 
#' Make sure all columns in data frame import are numeric or binary
#' @param xmat A dataframe create from read_csv or read_excel
#' @return The x-matrix multiplication we desire
#' 
#' xmat = data.frame(read_excel('VMB Variables.xlsx',skip=1))
#' xtx <- xmat_trans

xmat_trans <- function(xmat) {
  xmat = as.matrix(xmat)
  xtx = xmat %*% solve(t(xmat) %*% xmat)
  return(xtx)
}
