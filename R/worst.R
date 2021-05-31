worst <-
function(changes, period = 'daily'){
  # worst return over chosen period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... xts   ... (nx1) over which period, daily, monthly, quarterly or yearly. 
  #
  # OUTPUT        ... float ... minimum return over chosen period
  ret <- aggregate_returns(changes, period)
  return(min(ret))
}
