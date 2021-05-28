#Plot the results
options(scipen=10000)

ggplot(data = df3, aes(x = items.timestamp, y = items.views)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Views",
       title = "German Page Views: Same-sex Marriage",
       subtitle = "Daily Page Views")


ggplot(data = df4, aes(x = items.timestamp, y = items.views)) +
  geom_point() +
  labs(x = "Date",
       y = "Total Views",
       title = "German Page Views: LGBT Parenting",
       subtitle = "Daily Page Views")


df6<-melt(df5, id.vars ="Date", variable.name = "series")

ggplot(df6, aes(Date, value))+geom_line(aes(colour= series)) + 
    labs(x = "Date",
       y = "Total Views",
       title = "German Page Views: Sanna Marin, Women in Government, Same-Sex Marriage and 
LGBT Parenting",
       subtitle = "Daily Page Views")



# Plot Returns and Drawdown of the strategy

dd <- drawdown(ret)
comb <- data.frame((ret-1), dd)
colnames(comb) <- c("returns", "drawdowns")
comb['date'] <- as.Date(row.names(comb))
colors <- c("Drawdowns" = "steelblue", "Returns" = "orange")

ggplot(comb, aes(x=date)) + 
  geom_area(aes(y = drawdowns, color = "Drawdowns"), fill = "steelblue", size = 1, alpha = 0.4) +
  geom_line(aes(y = returns, color = "Returns"), size = 1) +
  labs(y = "Change in percentage", x = "Date", title = "Strategy Return and Drawdown") +
  scale_color_manual(values = colors) + 
  theme(legend.title=element_blank())
