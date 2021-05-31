ret_dd_plot <-
function(ret, benchmark = FALSE, benchmark_df = NULL){
  dd <- drawdown(ret)
  
  if (benchmark){
    comb <- data.frame((ret-1), (benchmark_df-1), dd)
    colnames(comb) <- c("returns", "benchmark", "drawdowns")
    comb['date'] <- as.Date(row.names(comb))
  }else{
    comb <- data.frame((ret-1), dd)
    colnames(comb) <- c("returns", "drawdowns")
    comb['date'] <- as.Date(row.names(comb))
  }
  
  out1 <- ggplot(comb, aes(x=date, y = returns)) + 
    geom_line(color = "orange", size = 1)+
    labs(y = "Change in percentage", title = "Strategy Return and Drawdown")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position = "none")
  
  if (benchmark){
    out1 <- out1 + geom_line(data = comb, aes(x = date, y = benchmark), 
                             color = 'brown', linetype=1)
  }
  
  out2 <- ggplot(comb, aes(x=date, y = drawdowns)) + 
    geom_area(color = "steelblue", fill = "steelblue", size = 1, alpha = 0.4)+
    geom_line(color="steelblue", size=0.5) +
    labs(y = "", x = "Date")+
    scale_y_continuous(labels=scales::percent)+  
    theme(legend.position = "none")
  
  out <- plot_grid(out1, out2, nrow = 2, align = "v", rel_heights = c(2, 1))
  return(out)
}
