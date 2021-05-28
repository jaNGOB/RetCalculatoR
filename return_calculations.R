# 
#
#
#

pct_change <- function(prices){
  # change calculates the percentage change in a time series.
  # INPUT    prices ... nx1 ... prices
  #
  # OUTPUT   ..... nx1 ... percentage changes
  tmp <-   na.omit(prices / lag.xts(prices) - 1)
  return(tmp)
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


comp <- function(returns){
  # Calculates total compounded returns
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... Max drawdowns.
  return((prod(1+returns) - 1))
}


aggregate_returns <- function(df, period = 'daily'){
  # aggregate the returns over a period. 
  # INPUT df ... nx1 ... returns as calculated by the function.
  # INPUT period ... str ... 
  # INPUT compounded ... bool ... 
  #
  # OUTPUT ... nx1 ... returns.
  
  if (period == 'weekly'){
    apply.weekly(df, comp)
  } else if (period == 'monthly'){
    return(apply.monthly(df, comp))
  } else if (period == 'yearly'){
    return(apply.yearly(df, comp))
  } else if (period == 'quarterly'){
    return(apply.quarterly(df, comp))
  } else {
    return(df)
  }
}

avg_return <- function(returns, period = 'daily'){
  ret <- aggregate_returns(returns, period)
  return(mean(ret[ret!=0]))
}

avg_win <- function(returns, period = 'daily'){
  # How much did we win in the average winning period
  #
  # INPUT 
  # INPUT period ... str ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT ... float ... mean win.
  
  agg_ret <- aggregate_returns(returns, period)
  return(mean(agg_ret[agg_ret > 0]))
}

avg_loss <- function(returns, period ='daily'){
  # How much did we loose in the average loosing period
  #
  # INPUT 
  # INPUT period ... str ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT ... float ... mean win.
  
  agg_ret <- aggregate_returns(returns, period)
  return(mean(agg_ret[agg_ret < 0]))
}

sharpe <- function(returns, rf=0, periods=252, annualize=TRUE){
  res <- mean(returns)/sd(returns)
  if (annualize){
    return(res * sqrt(252))
  } else {
    return(res)
  }
}

sortino <- function(returns, rf=0, periods=252, annualize=TRUE){
  downside = (sum(returns[returns < 0] ** 2))/ length(returns)
  res = mean(returns) / sqrt(downside)
  if (annualize){
    return(res * sqrt(252))
  } else {
    return(res)
  }
}


adj_sortino <- function(returns, rf=0, periods=252, annualize=TRUE){
  sort <- sortino(returns, rf, periods, annualize)
  return(sort/sqrt(2))
}


best <- function(returns, period = 'daily'){
  ret <- aggregate_returns(returns, period)
  return(max(ret))
}


worst <- function(returns, period = 'daily'){
  ret <- aggregate_returns(returns, period)
  return(min(ret))
}


volatility <- function(returns, period = 'daily', annualize = TRUE){
  ret <- aggregate_returns(returns, period)
  if (annualize){
    return(sd(ret) * sqrt(252))
  } else {
    return(sd(ret))
  }
}

print_table <- function(returns){
  print(maxdrawdown(returns))
  print(sharpe(returns))
  print(sortino(returns))
  print(comp(chg))
}


###########
## TESTS ##
###########

library(tidyquant)
getSymbols("AAPL", from = '2020-01-01',
           to = "2021-03-01",warnings = FALSE,
           auto.assign = TRUE)

chg <- pct_change(AAPL$AAPL.Adjusted)
ret <- returns(chg)

chg$date <- index(chg)

