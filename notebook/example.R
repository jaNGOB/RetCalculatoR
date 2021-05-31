library(RetCalculatoR)
library(xts)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(zoo)


# Generate random trading data.

dates <- seq(as.Date("2014/09/04"), by = "day", length.out = 1000)
dates <- dates[!weekdays(dates) %in% c("Samstag", "Sonntag")]
changes <- rnorm(length(dates))/100
ret <- xts::xts(returns(changes), order.by = dates)
chg <- xts::xts(changes, order.by = dates)

# Create a simple SMA Crossover strategy

df <- chg
colnames(df) <- 'change'
df$price <- ret

df$lma <- zoo::rollapply(df$price, 50, mean)
df$sma <- zoo::rollapply(df$price, 20, mean)
df <- na.omit(df)

df$position <- ifelse(df$lma > df$sma, -1, 1)
df$strat_return <- df$position * df$change

# Analyze the strategy

sharpe(df$strat_return)

ret_dd_plot(returns(df$strat_return), TRUE, df$price)

generate_report(df$strat_return)

density_plot_2(df$strat_return)


density_plot_2 <-
  function(chg){
    colnames(chg) <- "pct_returns"
    
    out <- ggplot2::ggplot(chg, ggplot2::aes(x=pct_returns)) +
      ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="steelblue", binwidth=0.005, alpha=0.6)+
      ggplot2::geom_density(alpha=0.25, fill="black")+
      ggplot2::geom_vline(ggplot2::aes(xintercept=mean(pct_returns)), color="orange",
                    linetype="dashed", size=1)+
      ggplot2::labs(title="Distribution of Daily Returns", x="", y = "Density")+
      ggplot2::scale_x_continuous(labels=scales::percent)
    return(out)
  }

