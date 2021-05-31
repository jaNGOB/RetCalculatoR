avg_loss <-
function(changes, period ='daily'){
  # How much did we loose in the average loosing period
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT          ... float ... average loss per period.
  agg_ret <- aggregate_returns(changes, period)
  return(mean(agg_ret[agg_ret < 0]))
}
