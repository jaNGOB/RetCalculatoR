\name{adj_sortino}
\alias{adj_sortino}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A function to compute the adjusted Sortino ratio.
}
\description{
Measaures the  risk-adjusted return of an investment asset, portfolio, or strategy.
}
\usage{
adj_sortino(changes, rf = 0, periods = 252, annualize = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{changes}{Percentage changes
}
  \item{rf}{Risk-free rate
}
  \item{periods}{Frequency
}
  \item{annualize}{Option to annualize, TRUE by default
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
