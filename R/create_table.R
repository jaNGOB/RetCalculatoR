create_table <-
function(chg, ret){
  
  fun <- c("Total Returns", "CAGR", "Max. Drawdown", "Sharpe Ratio", "Sortino Ratio", 
           "Adj. Sortino Ratio", "Best month", "Worst month", "Average winning month", "Average losing month")
  res <- c(as.numeric(ret[length(ret)]), cagr(ret), maxdrawdown(ret), sharpe(chg), 
           sortino(chg), adj_sortino(chg), best(chg, "monthly"), worst(chg, "monthly"), 
           avg_win(chg, "monthly"), avg_loss(chg, "monthly"))
  
  tab <- data.frame(fun, res)
  colnames(tab) <- c("Metric", "Result")
  out <- gridExtra::tableGrob(tab, rows = NULL)
  return(out)
}