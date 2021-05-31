returns <-
function(changes){
  # retruns calculets the return of 1$ using percentage changes.
  # INPUT    changes ... nx1 ... percentage changes of the strategy
  #
  # OUTPUT   ..... nx1 ... returns from 1
  return(cumprod(1+changes))
}
