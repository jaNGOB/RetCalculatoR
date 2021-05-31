comp <-
function(changes){
  # Calculates total compounded returns
  # INPUT changes ... xts ... (nx1) pct returns of the strategy
  # 
  # OUTPUT        ... float ... overall return.
  return((prod(1+changes) - 1))
}
