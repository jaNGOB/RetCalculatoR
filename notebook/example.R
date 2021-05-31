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

ret_dd_plot_2(returns(df$strat_return), TRUE, df$price)

generate_report(df$strat_return)

create_heatmap_2(df$strat_return)

create_heatmap_2 <-
  function(chg){
    mydata <- data.frame(chg, format(as.Date(zoo::index(chg)), "%m"), format(as.Date(zoo::index(chg)), "%Y"))
    colnames(mydata) <- c("chg", "date_month", "date_year")
    
    myAvgRet <- mydata %>%
      group_by(date_year, date_month) %>%
      summarise(AVGreturns = comp(chg))
    
    out <- ggplot2::ggplot(myAvgRet, aes(x = date_month, date_year)) +
      geom_tile(aes(fill = AVGreturns)) +
      geom_text(aes(label = scales::percent(round(AVGreturns, 2))), size=3)+
      scale_x_discrete("Month", labels = as.character(myAvgRet$date_month), breaks = (myAvgRet$date_month))+
      scale_y_discrete("Year", labels = as.character(myAvgRet$date_year), breaks = myAvgRet$date_year) + 
      scale_fill_gradient(low = "red", high = "green")
    return(out)
  }

