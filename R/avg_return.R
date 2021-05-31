avg_return <-
function(changes, period = 'daily'){
  # How much did we win in the average winning period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... str   ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... float ... average return over chosen period.
  ret <- aggregate_returns(changes, period)
  return(mean(ret[ret!=0]))
}
