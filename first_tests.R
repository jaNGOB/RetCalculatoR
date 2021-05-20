library(tidyquant)

getSymbols("AAPL", from = '2018-01-01',
           to = "2021-04-01",warnings = FALSE,
           auto.assign = TRUE)


change <- function(prices){
  # change calculates the percentage change in a time series.
  # INPUT    prices ... nx1 ... prices
  #
  # OUTPUT   ..... nx1 ... percentage changes
  tmp <- (prices / lag(prices) - 1)
  return(tmp[2:length(tmp)])
}


returns <- function(changes){
  # retruns calculets the return of 1$ using percentage changes.
  # INPUT    prices ... nx1 ... percentage changes
  #
  # OUTPUT   ..... nx1 ... returns from 1
  return(cumprod(1+changes))
}


total_return <- function(returns){
  # return the total return of returns
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... total returns.
  return(as.numeric(returns[length(returns)])-1)
}


cagr <- function(returns){
  # return the compound annual growth rate from daily returns.
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... CAGR.
  t <- length(returns)
  return(((as.numeric(returns[t]))/as.numeric(returns[1]))^(1/t*252)-1)
}


drawdown <- function(returns){
  # calculates the drawdowns.
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... nx1 ... Drawdowns.
  rolling.max <- cummax(returns)
  return(returns/roll_max -1)
}


maxdrawdown <- function(drawdown){
  # calculates the max drawdown from drawdowns.
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... Max drawdowns.
  return(min(drawdown))
}


pct_changes <- (change(AAPL$AAPL.Adjusted))

ret <- returns(pct_changes)


