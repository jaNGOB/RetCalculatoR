sortino <-
function(changes, rf=0, periods=252, annualize=TRUE){
  # Sortino ratio calculation
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  # INPUT annualize ... bool  ... flag if the output should be annualized
  #
  # OUTPUT          ... float ... Sortino ratio of the strategy
  downside = (sum(changes[changes < 0] ** 2))/ length(changes)
  res = mean(changes) / sqrt(downside)
  if (annualize){
    return(res * sqrt(252))
  } else {
    return(res)
  }
}
