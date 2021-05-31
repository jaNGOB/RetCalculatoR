aggregate_returns <-
function(changes, period = 'daily'){
  # aggregate the returns over a period. 
  # INPUT changes ... xts ... (nx1) pct returns of the strategy
  # INPUT period  ... str ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... xts ... (nx1)returns.
  
  if (period == 'weekly'){
    return(xts::apply.weekly(changes, comp))
  } else if (period == 'monthly'){
    return(xts::apply.monthly(changes, comp))
  } else if (period == 'yearly'){
    return(xts::apply.yearly(changes, comp))
  } else if (period == 'quarterly'){
    return(xts::apply.quarterly(changes, comp))
  } else {
    return(changes)
  }
}
