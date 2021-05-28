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
