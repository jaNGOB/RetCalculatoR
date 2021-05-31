cagr <-
function(changes){
  # return the compound annual growth rate from daily returns.
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # 
  # OUTPUT        ... float ... CAGR.
  t <- length(changes)
  return(((as.numeric(changes[t]))/as.numeric(changes[1]))^(1/t*252)-1)
}
