drawdown <-
function(returns){
  # calculates the drawdowns of the strategy as a xts.
  # INPUT returns ... xts ... (nx1) returns of the strategy starting by 1.
  # 
  # OUTPUT        ... xts ... (nx1) Drawdowns.
  roll_max <- cummax(returns)
  return(returns/roll_max -1)
}
