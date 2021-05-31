library(RetCalculatoR)

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

generate_report(df$strat_return)

maxdrawdown(df$strat_return)

sharpe(df$strat_return)
