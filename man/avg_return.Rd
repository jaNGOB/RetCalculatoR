\name{avg_return}
\alias{avg_return}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to compute the average return.
}
\description{Average return measures how much an investment has increased on average during a specific time period.
}
\usage{
avg_return(changes, period = "daily")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{changes}{Percentage changes
}
  \item{period}{Frequency
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
function (changes, period = "daily") 
{
    ret <- aggregate_returns(changes, period)
    return(mean(ret[ret != 0]))
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
