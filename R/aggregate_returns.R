aggregate_returns <-
function(changes, period = 'daily'){
  # aggregate the returns over a period. 
  # INPUT changes ... xts ... (nx1) pct returns of the strategy
  # INPUT period  ... str ... over which period, daily, monthly, quarterly or yearly.
  #
  # OUTPUT        ... xts ... (nx1)returns.
  
  if (period == 'weekly'){
    apply.weekly(changes, comp)
  } else if (period == 'monthly'){
    return(apply.monthly(changes, comp))
  } else if (period == 'yearly'){
    return(apply.yearly(changes, comp))
  } else if (period == 'quarterly'){
    return(apply.quarterly(changes, comp))
  } else {
    return(changes)
  }
}
