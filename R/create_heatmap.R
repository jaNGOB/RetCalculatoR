create_heatmap <-
function(chg){
  mydata <- data.frame(chg, format(as.Date(zoo::index(chg)), "%m"), format(as.Date(zoo::index(chg)), "%Y"))
  colnames(mydata) <- c("chg", "date_month", "date_year")
  
  myAvgRet <- mydata %>%
    dplyr::group_by(date_year, date_month) %>%
    dplyr::summarise(AVGreturns = comp(chg))
  
  out <- ggplot2::ggplot(myAvgRet, ggplot2::aes(x = date_month, date_year)) +
    ggplot2::geom_tile(ggplot2::aes(fill = AVGreturns)) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(round(AVGreturns, 2))), size=3)+
    ggplot2::scale_x_discrete("Month", labels = as.character(myAvgRet$date_month), breaks = (myAvgRet$date_month))+
    ggplot2::scale_y_discrete("Year", labels = as.character(myAvgRet$date_year), breaks = myAvgRet$date_year) + 
    ggplot2::scale_fill_gradient(low = "red", high = "green")
  return(out)
}