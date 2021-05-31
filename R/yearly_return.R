yearly_return <-
function(chg){
  years <- unique(format(as.Date(index(chg)), "%Y"))
  yearly_ret <- c(0)
  for (y in years){
    tmp <- returns(chg[as.character(y)])-1
    yearly_ret <- c(yearly_ret, as.numeric(tmp[length(tmp)]))
  }
  yearly_ret <- yearly_ret[2:(length(years)+1)]
  
  y_df <- data.frame(years, yearly_ret, mean(yearly_ret))
  
  ggplot(data=y_df, aes(x=years, y=yearly_ret)) +
    geom_bar(stat="identity") + 
    geom_line(aes(x = years, y = mean.yearly_ret.), color='red', linetype = "dashed") +
    labs(y = "Return in percent", x = "Year", title = "Yearly return") +
    scale_y_continuous(labels=scales::percent)
}
