# Heatmap template
# If tidyverse is not yet installed, you may do so like such: > install.packages('tidyverse')
library(tidyverse)

# Some sector and geographies for illustration
sector <- c("January","February", "March", "April", "May", "June","July","August","September","October","November","December")
geography <- c("2017", "2018", "2019", "2020")
# Simulate market returns based on a random normal distribution, with a mean return of 8% and volatility (standard deviation) 15%
returns <- data.frame(Returns = round(rnorm(n = 1000, mean = 0.08, sd = 0.15), 2)) 

mydata <- cbind(Sector = sample(sector, 1000, replace = T), Geo = sample(geography, 1000, replace = T), returns)

head(mydata, 10)

# Let's take a simple average across sector + geographies
myAvgRet <- mydata %>%
  group_by(Sector, Geo) %>%
  summarise(AVGreturns = mean(Returns))

# Summarised data in long-form
myAvgRet

# Convert to wide-form and move Sector names to rownames so that we can get a numeric matrix

myAvgRet.mat <- myAvgRet %>%
  # Convert long-form to wide-form
  spread(key = Geo, value = AVGreturns) %>%
  as.data.frame %>%
  # Extract column 'Sector' and use it to name rows. 
  # This is necessary so the final output is a numeric matrix, the input which the heatmap function takes
  column_to_rownames(var = "Sector") %>%
  as.matrix

# The matrix looks already like how we want to visualise it
myAvgRet.mat

str(myAvgRet.mat)

plot1 <- heatmap(myAvgRet.mat, Colv = NA, Rowv = NA, scale = "column")

ggplot(myAvgRet, aes(x = Sector, Geo)) +
  geom_tile(aes(fill = AVGreturns)) +
  geom_text(aes(label = round(AVGreturns, 3)))
