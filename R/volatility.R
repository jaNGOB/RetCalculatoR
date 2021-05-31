volatility <-
function(changes, period = 'daily', annualize = TRUE){
  # volatlity over a defined period of time. 
  #
  # INPUT changes     ... xts   ... (nx1) pct returns of the strategy
  # INPUT period      ... xts   ... (nx1) over which period, daily, monthly, quarterly or yearly. 
  # INPUT annualize   ... bool  ... if volatility should be annualized or not
  #
  # OUTPUT            ... float ... volatility of the strategy
  ret <- aggregate_returns(changes, period)
  if (annualize){
    return(sd(ret) * sqrt(252))
  } else {
    return(sd(ret))
  }
}
