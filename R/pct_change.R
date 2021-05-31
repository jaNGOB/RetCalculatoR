pct_change <-
function(prices){
  # change calculates the percentage change in a time series.
  # INPUT prices  ... xts ... (nx1) actual returns of the strategy
  #
  # OUTPUT        ... xts ... (nx1) percentage changes
  tmp <-   na.omit(prices / lag.xts(prices) - 1)
  return(tmp)
}
