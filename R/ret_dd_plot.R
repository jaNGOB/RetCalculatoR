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
    
    out1 <- ggplot2::ggplot(comb, ggplot2::aes(x=date, y = returns, group = 1)) + 
      ggplot2::geom_line(color = "orange", size = 1)+
      ggplot2::labs(y = "Change in percentage", title = "Strategy Return and Drawdown")+
      ggplot2::scale_y_continuous(labels=scales::percent)+
      ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                     axis.text.x=ggplot2::element_blank(),
                     axis.ticks.x=ggplot2::element_blank(), 
                     legend.position = "none")
    
    if (benchmark){
      out1 <- out1 + 
        ggplot2::geom_line(data = comb, ggplot2::aes(x = date, y = benchmark, group = 1), 
                           color = 'brown', linetype=1)
    }
    
    out2 <- ggplot2::ggplot(comb, ggplot2::aes(x=date, y = drawdowns)) + 
      ggplot2::geom_area(color = "steelblue", fill = "steelblue", size = 1, alpha = 0.4)+
      ggplot2::geom_line(color="steelblue", size=0.5) +
      ggplot2::labs(y = "", x = "Date")+
      ggplot2::scale_y_continuous(labels=scales::percent)+  
      ggplot2::theme(legend.position = "none")
    
    out <- cowplot::plot_grid(out1, out2, nrow = 2, align = "v", rel_heights = c(2, 1))
    return(out)
  }
