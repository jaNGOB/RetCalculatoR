cagr <-
function(returns){
  # return the compound annual growth rate from daily returns.
  # INPUT returns ... xts ... (nx1) returns of the strategy starting by 1.
  # 
  # OUTPUT        ... float ... CAGR.
  t <- length(returns)
  return(((as.numeric(returns[t]))/as.numeric(returns[1]))^(1/t*252)-1)
}
