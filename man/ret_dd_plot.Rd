\name{ret_dd_plot}
\alias{ret_dd_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot returns and drawdowns.
}
\description{Plots returns and drawdowns in a single plot.
}
\usage{
ret_dd_plot(ret)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ret}{Returns
}
}
\details{Currently no additional details.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Jan Gobeli & Tuomas Ahonen
}
\note{No further notes.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
ret_dd_plot <-
function(ret, benchmark = FALSE, benchmark_df = NULL){
  dd <- drawdown(ret)
  
  if (benchmark){
    comb <- data.frame((ret-1), (benchmark_df-1), dd)
    colnames(comb) <- c("returns", "benchmark", "drawdowns")
    comb['date'] <- as.Date(row.names(comb))
  }else{
    comb <- data.frame((ret-1), dd)
    colnames(comb) <- c("returns", "drawdowns")
    comb['date'] <- as.Date(row.names(comb))
  }
  
  out1 <- ggplot(comb, aes(x=date, y = returns)) + 
    geom_line(color = "orange", size = 1)+
    labs(y = "Change in percentage", title = "Strategy Return and Drawdown")+
    scale_y_continuous(labels=scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          legend.position = "none")
  
  if (benchmark){
    out1 <- out1 + geom_line(data = comb, aes(x = date, y = benchmark), 
                             color = 'brown', linetype=1)
  }
  
  out2 <- ggplot(comb, aes(x=date, y = drawdowns)) + 
    geom_area(color = "steelblue", fill = "steelblue", size = 1, alpha = 0.4)+
    geom_line(color="steelblue", size=0.5) +
    labs(y = "", x = "Date")+
    scale_y_continuous(labels=scales::percent)+  
    theme(legend.position = "none")
  
  out <- plot_grid(out1, out2, nrow = 2, align = "v", rel_heights = c(2, 1))
  return(out)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
