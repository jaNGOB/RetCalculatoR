create_heatmap <-
function(chg){
  mydata <- data.frame(chg, month(as.Date(index(chg))), year(as.Date(index(chg))))
  colnames(mydata) <- c("chg", "date_month", "date_year")
  
  myAvgRet <- mydata %>%
    group_by(date_year, date_month) %>%
    summarise(AVGreturns = comp(chg))
  
  out <- ggplot(myAvgRet, aes(x = date_month, date_year)) +
    geom_tile(aes(fill = AVGreturns)) +
    geom_text(aes(label = scales::percent(round(AVGreturns, 2))), size=3)+
    scale_x_continuous("Month", labels = as.character(myAvgRet$date_month), breaks = (myAvgRet$date_month))+
    scale_y_continuous("Year", labels = as.character(myAvgRet$date_year), breaks = myAvgRet$date_year) + 
    scale_fill_gradient(low = "red", high = "green")
  return(out)
}
