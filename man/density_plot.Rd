\name{density_plot}
\alias{density_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to construct a plot of densities of returns.
}
\description{Visual representation of returns data and their distributions.
}
\usage{
density_plot(chg)
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
    colnames(chg) <- "pct_returns"
    out <- ggplot(chg, aes(x = pct_returns)) + geom_histogram(aes(y = ..density..), 
        colour = "black", fill = "steelblue", binwidth = 0.005, 
        alpha = 0.6) + geom_density(alpha = 0.25, fill = "black") + 
        geom_vline(aes(xintercept = mean(pct_returns)), color = "orange", 
            linetype = "dashed", size = 1) + labs(title = "Distribution of Daily Returns", 
        x = "", y = "Density") + scale_x_continuous(labels = scales::percent)
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
