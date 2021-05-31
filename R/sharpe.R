sharpe <-
function(changes, rf=0, periods=252, annualize=TRUE){
  # Sharpe ratio calculation
  #
  # INPUT returns   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  # INPUT annualize ... bool  ... flag if the output should be annualized
  #
  # OUTPUT          ... float ... Sharpe ratio of the strategy
  res <- mean(changes)/sd(changes)
  if (annualize){
    return(res * sqrt(252))
  } else {
    return(res)
  }
}
