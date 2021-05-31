\name{create_heatmap}
\alias{create_heatmap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to create a heatmap of metrics.
\description{Heatmap displaying returns data in a compact and visually appealing manner.
}
\usage{
create_heatmap(chg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chg}{Percentage changes.
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
function(chg){
  mydata <- data.frame(chg, format(as.Date(index(chg)), "%m"), format(as.Date(index(chg)), "%Y"))
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
