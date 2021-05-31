maxdrawdown <-
function(drawdown){
  # calculates the max drawdown from drawdowns.
  # INPUT drawdown ... xts    ... (nx1) drawdowns of the strategy as calculated by "drawdown"
  # 
  # OUTPUT         ... float  ... (1x1) Max drawdowns.
  return(min(drawdown))
}
