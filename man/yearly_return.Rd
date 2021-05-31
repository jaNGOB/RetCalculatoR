\name{yearly_return}
\alias{yearly_return}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to plot yearly returns.
}
\description{Plots the yearly returns of an investment asset or portfolio.
}
\usage{
yearly_return(chg)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chg}{Percentage changes
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
function (chg) 
{
    years <- unique(year(as.Date(index(chg))))
    yearly_ret <- c(0)
    for (y in years) {
        tmp <- returns(chg[as.character(y)]) - 1
        yearly_ret <- c(yearly_ret, as.numeric(tmp[length(tmp)]))
    }
    yearly_ret <- yearly_ret[2:(length(years) + 1)]
    y_df <- data.frame(years, yearly_ret, mean(yearly_ret))
    ggplot(data = y_df, aes(x = years, y = yearly_ret)) + geom_bar(stat = "identity") + 
        geom_line(aes(x = years, y = mean.yearly_ret.), color = "red", 
            linetype = "dashed") + labs(y = "Return in percent", 
        x = "Year", title = "Yearly return") + scale_y_continuous(labels = scales::percent)
  }
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
