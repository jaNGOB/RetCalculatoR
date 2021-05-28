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


