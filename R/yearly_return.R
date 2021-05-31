yearly_return <-
function(chg){
  years <- unique(format(as.Date(zoo::index(chg)), "%Y"))
  yearly_ret <- c(0)
  for (y in years){
    tmp <- returns(chg[as.character(y)])-1
    yearly_ret <- c(yearly_ret, as.numeric(tmp[length(tmp)]))
  }
  yearly_ret <- yearly_ret[2:(length(years)+1)]
  
  y_df <- data.frame(years, yearly_ret, mean(yearly_ret))
  
  ggplot2::ggplot(data=y_df, ggplot2::aes(x=years, y=yearly_ret, group = 1)) +
    ggplot2::geom_bar(stat="identity") + 
    ggplot2::geom_line(ggplot2::aes(x = years, y = mean.yearly_ret., group = 1), size=2, color='red', linetype = "dashed") +
    ggplot2::labs(y = "Return in percent", x = "Year", title = "Yearly return") +
    ggplot2::scale_y_continuous(labels=scales::percent)
}
