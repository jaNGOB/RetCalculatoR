density_plot <-
function(chg){
  colnames(chg) <- "pct_returns"
  
  out <- ggplot(chg, aes(x=pct_returns)) +
    geom_histogram(aes(y=..density..), colour="black", fill="steelblue", binwidth=0.005, alpha=0.6)+
    geom_density(alpha=0.25, fill="black")+
    geom_vline(aes(xintercept=mean(pct_returns)), color="orange",
               linetype="dashed", size=1)+
    labs(title="Distribution of Daily Returns", x="", y = "Density")+
    scale_x_continuous(labels=scales::percent)
  return(out)
}
