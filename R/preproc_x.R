#xmat=data.frame(read_excel('VMB Variables.xlsx',skip=1))
#depends how "clean the characteristic table of x comes in as, will need to figure out how to identify columns with variables

xmat_trans <- function(xmat) {
  xmat=as.matrix(xmat)
  xtx=xmat %*% solve(t(xmat) %*% xmat)
  return(xtx)
}
