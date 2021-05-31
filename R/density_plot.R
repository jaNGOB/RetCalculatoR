density_plot <-
function(chg){
  colnames(chg) <- "pct_returns"
  
  out <- ggplot2::ggplot(chg, ggplot2::aes(x=pct_returns)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), colour="black", fill="steelblue", binwidth=0.005, alpha=0.6)+
    ggplot2::geom_density(alpha=0.25, fill="black")+
    ggplot2::geom_vline(ggplot2::aes(xintercept=mean(pct_returns)), color="orange",
                        linetype="dashed", size=1)+
    ggplot2::labs(title="Distribution of Daily Returns", x="", y = "Density")+
    ggplot2::scale_x_continuous(labels=scales::percent)
  return(out)
}