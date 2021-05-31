total_return <-
function(returns){
  # return the total return of returns
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... total returns.
  return(as.numeric(changes[length(returns)])-1)
}
