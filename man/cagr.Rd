\name{cagr}
\alias{cagr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to compute the cumulative annual growth rate.
}
\description{The rate of return required for an investment to grow from its beginning balance to its ending balance, assuming profits are reinvested at the end of each year.
}
\usage{
cagr(changes)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{changes}{Percentage changes
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
function (changes) 
{
    t <- length(changes)
    return(((as.numeric(changes[t]))/as.numeric(changes[1]))^(1/t * 
        252) - 1)
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
