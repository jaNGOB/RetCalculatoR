\name{create_table}
\alias{create_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to create a table with summary metrics.
}
\description{Output of metrics and analytics in a neat table.
}
\usage{
create_table(chg, ret)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{chg}{Percentage changes
}
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
function (chg, ret) 
{
    fun <- c("Total Returns", "CAGR", "Max. Drawdown", "Sharpe Ratio", 
        "Sortino Ratio", "Adj. Sortino Ratio", "Best month", 
        "Worst month", "Average winning month", "Average losing month")
    res <- c(as.numeric(ret[length(ret)]), cagr(ret), maxdrawdown(ret), 
        sharpe(chg), sortino(chg), adj_sortino(chg), best(chg, 
            "monthly"), worst(chg, "monthly"), avg_win(chg, "monthly"), 
        avg_loss(chg, "monthly"))
    tab <- data.frame(fun, res)
    colnames(tab) <- c("Metric", "Result")
    out <- tableGrob(tab, rows = NULL)
    return(out)
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
