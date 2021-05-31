library(RetCalculatoR)
library(xts)
library(ggplot2)
library(cowplot)
library(formattable)
library(gridExtra)
library(tidyverse)


# Generate random trading data.

dates <- seq(as.Date("2014/09/04"), by = "day", length.out = 1000)
dates <- dates[!weekdays(dates) %in% c("Samstag", "Sonntag")]
changes <- rnorm(length(dates))/100
ret <- xts(returns(changes), order.by = dates)
chg <- xts(changes, order.by = dates)

# Create a simple SMA Crossover strategy

df <- chg
colnames(df) <- 'change'
df$price <- ret

df$lma <- rollapply(df$price, 50, mean)
df$sma <- rollapply(df$price, 20, mean)
df <- na.omit(df)

df$position <- ifelse(df$lma > df$sma, -1, 1)
df$strat_return <- df$position * df$change

# Analyze the strategy

sharpe(df$strat_return)

ret_dd_plot(returns(df$strat_return), TRUE, df$price)

generate_report(df$strat_return)

ret_dd_plot_2 <-
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
    
    out1 <- ggplot(comb, aes(x=date, y = returns, group = 1)) + 
      geom_line(color = "orange", size = 1)+
      labs(y = "Change in percentage", title = "Strategy Return and Drawdown")+
      scale_y_continuous(labels=scales::percent)+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), 
            legend.position = "none")
    
    if (benchmark){
      out1 <- out1 + geom_line(data = comb, aes(x = date, y = benchmark, group = 1), 
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

