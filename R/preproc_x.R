#' Preprocess x matrix data
#' 
#' @param xmat A dataframe containing strictly numerics or binaries, e.g. dataframes created from read_csv or read_excel
#' @return The x-matrix multiplication we desire
#' @examples
#' #xmat <- read_excel('VMB Variables.xlsx',skip=1)
#' #xtx <- xmat_trans(xmat)

xmat_trans <- function(xmat) {
  xmat = as.matrix(xmat)
  xtx = xmat %*% solve(t(xmat) %*% xmat)
  return(xtx)
}
