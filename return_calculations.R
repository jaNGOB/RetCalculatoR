# 
#
#
#

pct_change <- function(prices){
  # change calculates the percentage change in a time series.
  # INPUT prices  ... xts ... (nx1) actual returns of the strategy
  #
  # OUTPUT        ... xts ... (nx1) percentage changes
  tmp <-   na.omit(prices / lag.xts(prices) - 1)
  return(tmp)
}


returns <- function(changes){
  # retruns calculets the return of 1$ using percentage changes.
  # INPUT    changes ... nx1 ... percentage changes of the strategy
  #
  # OUTPUT   ..... nx1 ... returns from 1
  return(cumprod(1+changes))
}


total_return <- function(returns){
  # return the total return of returns
  # INPUT returns ... nx1 ... returns as calculated by the function.
  # 
  # OUTPUT ... 1x1 ... total returns.
  return(as.numeric(changes[length(returns)])-1)
}


cagr <- function(changes){
  # return the compound annual growth rate from daily returns.
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # 
  # OUTPUT        ... float ... CAGR.
  t <- length(changes)
  return(((as.numeric(changes[t]))/as.numeric(changes[1]))^(1/t*252)-1)
}


drawdown <- function(returns){
  # calculates the drawdowns of the strategy as a xts.
  # INPUT returns ... xts ... (nx1) returns of the strategy starting by 1.
  # 
  # OUTPUT        ... xts ... (nx1) Drawdowns.
  rolling.max <- cummax(returns)
  return(returns/roll_max -1)
}


maxdrawdown <- function(drawdown){
  # calculates the max drawdown from drawdowns.
  # INPUT drawdown ... xts    ... (nx1) drawdowns of the strategy as calculated by "drawdown"
  # 
  # OUTPUT         ... float  ... (1x1) Max drawdowns.
  return(min(drawdown))
}


comp <- function(changes){
  # Calculates total compounded returns
  # INPUT changes ... xts ... (nx1) pct returns of the strategy
  # 
  # OUTPUT        ... float ... overall return.
  return((prod(1+changes) - 1))
}


aggregate_returns <- function(changes, period = 'daily'){
  # aggregate the returns over a period. 
  # INPUT changes ... xts ... (nx1) pct returns of the strategy
  # INPUT period  ... str ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... xts ... (nx1)returns.
  
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

avg_return <- function(changes, period = 'daily'){
  # How much did we win in the average winning period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... str   ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... float ... average return over chosen period.
  ret <- aggregate_returns(changes, period)
  return(mean(ret[ret!=0]))
}

avg_win <- function(changes, period = 'daily'){
  # How much did we win in the average winning period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... str   ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... float ... average win per period.
  agg_ret <- aggregate_returns(changes, period)
  return(mean(agg_ret[agg_ret > 0]))
}


avg_loss <- function(changes, period ='daily'){
  # How much did we loose in the average loosing period
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT          ... float ... average loss per period.
  agg_ret <- aggregate_returns(changes, period)
  return(mean(agg_ret[agg_ret < 0]))
}


sharpe <- function(changes, rf=0, periods=252, annualize=TRUE){
  # Sharpe ratio calculation
  #
  # INPUT returns   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  #°INPUT annualize ... bool  ... flag if the output should be annualized
  #
  # OUTPUT          ... float ... Sharpe ratio of the strategy
  res <- mean(changes)/sd(changes)
  if (annualize){
    return(res * sqrt(252))
  } else {
    return(res)
  }
}


sortino <- function(changes, rf=0, periods=252, annualize=TRUE){
  # Sortino ratio calculation
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  #°INPUT annualize ... bool  ... flag if the output should be annualized
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


adj_sortino <- function(changes, rf=0, periods=252, annualize=TRUE){
  # Adjust the sortino ratio.
  #
  # INPUT changes   ... xts   ... (nx1) pct returns of the strategy
  # INPUT rf        ... float ... risk free rate
  # INPUT period    ... str   ... over which period, daily, monthly, quarterly or yearly.
  #°INPUT annualize ... bool  ... flag if the output should be annualized
  #
  # OUTPUT          ... float ... Adjusted sortino ratio of the strategy
  sort <- sortino(changes, rf, periods, annualize)
  return(sort/sqrt(2))
}


best <- function(changes, period = 'daily'){
  # best return over chosen period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... xts   ... (nx1) over which period, daily, monthly, quarterly or yearly. 
  #
  # OUTPUT        ... float ... maximum return over chosen period
  ret <- aggregate_returns(changes, period)
  return(max(ret))
}


worst <- function(changes, period = 'daily'){
  # worst return over chosen period
  #
  # INPUT changes ... xts   ... (nx1) pct returns of the strategy
  # INPUT period  ... xts   ... (nx1) over which period, daily, monthly, quarterly or yearly. 
  #
  # OUTPUT        ... float ... minimum return over chosen period
  ret <- aggregate_returns(changes, period)
  return(min(ret))
}


volatility <- function(changes, period = 'daily', annualize = TRUE){
  # volatlity over a defined period of time. 
  #
  # INPUT changes     ... xts   ... (nx1) pct returns of the strategy
  # INPUT period      ... xts   ... (nx1) over which period, daily, monthly, quarterly or yearly. 
  # INPUT annualize   ... bool  ... if volatility should be annualized or not
  #
  # OUTPUT            ... float ... volatility of the strategy
  ret <- aggregate_returns(changes, period)
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

