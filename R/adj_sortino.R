adj_sortino <-
function(changes, rf=0, periods=252, annualize=TRUE){
  # Adjust the sortino ratio.
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  # INPUT annualize ... bool  ... flag if the output should be annualized
  #
  # OUTPUT          ... float ... Adjusted sortino ratio of the strategy
  sort <- sortino(changes, rf, periods, annualize)
  return(sort/sqrt(2))
}
